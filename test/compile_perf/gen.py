#!/usr/bin/env python3
"""Generate a synthetic Acton project with slow typechecking.

The project reproduces a real case. The test_service_discovery module in a user
project became several times slower to typecheck after acton-yang#472 ("Use
generic MKeyedList with tuple keys for adata lists"). The module imports a large
closure of generated YANG-style data classes. Profiles showed the time in the
constraint solver. For each Sel/Mut constraint whose receiver is still a
unification variable, each solver iteration computed two things again from
scratch:

  - the transitive import closure (transitiveImports, then lookupModuleInfo
    for each module)
  - importedConAttr / importedProtoAttr: every imported class that declares
    the selected attribute name, plus every descendant of every declaring
    class, made unique with a pairwise nubBy

Hundreds of generated classes share the attribute names get, create, copy
and to_gdata, and YANG leaf names such as name, address and port. The
candidate lists are therefore large. The number of rank calls (constraints
x solver iterations) multiplies their cost.

The generated project has the same shape as acton-yang output:

  yadata.act          The MNode / MList / MKeyedList base classes and the
                      Iterable / Indexed protocol extensions. A copy of the
                      relevant part of yang.adata.
  schema_NNN.act      Generated "YANG schema" modules. Each list gets an
                      MNode entry class and a keyed-list class. The entry
                      classes take their leaf attributes from a shared name
                      pool. Each module imports its predecessor, so the
                      consumer sees a deep transitive closure. The defaults
                      match the real-world shape: few modules, each with a
                      large number of classes, like a generated device
                      schema module.
  consume.act         A consumer in the style of test_service_discovery.
                      It iterates over keyed lists, filters on leaf values,
                      and copies entries into another tree with .get() and
                      .create(). It has one Sel-heavy function per --funcs,
                      with --blocks statement blocks each.

--style keyed makes each list a subclass of the generic MKeyedList[K, T],
as acton-yang does after #472. --style old makes each list a subclass of
MList with a per-class get() and a per-class Indexed extension, as
acton-yang did before #472.

--keys scalar gives each list a str key. --keys compound gives each list a
(name, port) key: a named tuple with --style keyed, or one argument per key
leaf with --style old (which then also gets no Indexed extension).

Measured result: in this synthetic form the style has no effect, and
compound keys cost about 1.5x with both key representations, on both
compiler generations. The real project regresses more, because its solver
takes more iterations (the synthetic consumer resolves receiver types too
quickly to reproduce that). See README.md for the numbers, including the
real-world before/after measurements.

Measure with run.sh, or by hand:

  1. Build the project once.
  2. Append a comment to src/consume.act.
  3. Rebuild. The consume typecheck time is the metric.
"""

import argparse
import os
import shutil

LEAF_POOL = [
    ("name", "str"),
    ("address", "?str"),
    ("port", "?u64"),
    ("enabled", "?bool"),
    ("description", "?str"),
    ("mtu", "?u64"),
    ("vlan_id", "?u64"),
    ("secret", "?str"),
]


def leaf_count_for(mod_idx: int, c: int, l: int) -> int:
    # rotate leaf counts 4..len(LEAF_POOL) so entries overlap on the shared
    # leaf-name pool without being identical
    return 4 + (mod_idx + c + l) % (len(LEAF_POOL) - 3)


def leaves_for(leaf_count: int, keys: str) -> list:
    """The leaf list for one entry class.

    With compound keys, the key is (name, port), so port becomes a required
    u64 instead of an optional one.
    """
    leaves = LEAF_POOL[:leaf_count]
    if keys == "compound":
        leaves = [(n, "u64" if n == "port" else t) for n, t in leaves]
    return leaves


def is_required(t: str) -> bool:
    return not t.startswith("?")


def entry_class(qname: str, leaf_count: int, keys: str) -> list[str]:
    """MNode entry class in the style of acton-yang generated adata."""
    leaves = leaves_for(leaf_count, keys)
    out = []
    out.append(f"class {qname}_entry(yadata.MNode):")
    for n, t in leaves:
        out.append(f"    {n}: {t}")
    out.append("")
    # required leaves first, then the optional ones with a None default
    ordered = [x for x in leaves if is_required(x[1])] + \
              [x for x in leaves if not is_required(x[1])]
    args = ", ".join(f"{n}: {t}" + ("" if is_required(t) else "=None") for n, t in ordered)
    out.append(f"    mut def __init__(self, {args}) -> None:")
    for n, _ in leaves:
        out.append(f"        self.{n} = {n}")
    out.append("")
    out.append("    mut def to_gdata(self) -> str:")
    out.append("        return self.name")
    out.append("")
    out.append(f"    mut def copy(self) -> {qname}_entry:")
    ctor_args = ", ".join(f"{n}=self.{n}" for n, _ in leaves)
    out.append(f"        return {qname}_entry({ctor_args})")
    out.append("")
    return out


def list_class(qname: str, leaf_count: int, style: str, keys: str) -> list[str]:
    """List class in the style of acton-yang generated adata.

    style "keyed": subclass of the generic MKeyedList[K, T] which declares
    .get() and carries the Indexed extension (acton-yang after #472).
    style "old": subclass of MList[T] with a generated per-class .get() and
    per-class Indexed extension (acton-yang before #472).

    keys "scalar": the key is the name leaf (a str).
    keys "compound": the key is (name, port). With style keyed, get/create
    take a named tuple, as acton-yang does after #472. With style old,
    get/create take one argument per key leaf, as acton-yang did before
    #472 (which also generated no Indexed extension for compound keys).
    """
    leaves = leaves_for(leaf_count, keys)
    compound = keys == "compound"
    key_type = "(name: str, port: u64)" if compound else "str"
    out = []
    if style == "keyed":
        match = "e.name == k.name and e.port == k.port" if compound else "e.name == k"
        out.append(f"class {qname}(yadata.MKeyedList[{key_type}, {qname}_entry]):")
        out.append(f"    mut def __init__(self, elements: list[{qname}_entry]=[]) -> None:")
        out.append(f"        yadata.MKeyedList.__init__(self, lambda e, k: {match}, elements)")
        out.append("")
    else:
        out.append(f"class {qname}(yadata.MList[{qname}_entry]):")
        out.append(f"    mut def __init__(self, elements: list[{qname}_entry]=[]) -> None:")
        out.append("        self.elements = elements")
        out.append("")
        if compound:
            out.append(f"    pure def get(self, name: str, port: u64) -> ?{qname}_entry:")
            out.append("        for e in self:")
            out.append("            if e.name == name and e.port == port:")
            out.append("                return e")
        else:
            out.append(f"    pure def get(self, key: str) -> ?{qname}_entry:")
            out.append("        for e in self:")
            out.append("            if e.name == key:")
            out.append("                return e")
        out.append("")
    opt_leaves = [(n, t) for n, t in leaves if not is_required(t)]
    create_args = ", ".join(f"{n}: {t}=None" for n, t in opt_leaves)
    if compound and style == "old":
        out.append(f"    mut def create(self, name: str, port: u64, {create_args}) -> {qname}_entry:")
        out.append("        e = self.get(name, port)")
    elif compound:
        out.append(f"    mut def create(self, key_: {key_type}, {create_args}) -> {qname}_entry:")
        out.append("        e = self.get(key_)")
    else:
        out.append(f"    mut def create(self, key: str, {create_args}) -> {qname}_entry:")
        out.append("        e = self.get(key)")
    out.append("        if e is not None:")
    for n, _ in opt_leaves:
        out.append(f"            if {n} is not None:")
        out.append(f"                e.{n} = {n}")
    out.append("            return e")
    if compound and style == "keyed":
        out.append("        (name=name, port=port) = key_")
    elif not compound:
        out.append("        name = key")
    ctor_args = ", ".join(f"{n}={n}" for n, _ in leaves)
    out.append(f"        res = {qname}_entry({ctor_args})")
    out.append("        self.elements.append(res)")
    out.append("        return res")
    out.append("")
    out.append(f"    mut def copy(self) -> {qname}:")
    out.append("        copied_elements = []")
    out.append("        for e in self:")
    out.append("            copied_elements.append(e.copy())")
    out.append(f"        return {qname}(elements=copied_elements)")
    out.append("")
    if style == "old" and keys == "scalar":
        out.append(f"extension {qname}(Indexed[str, {qname}_entry]):")
        out.append(f"    def __getitem__(self, k: str) -> {qname}_entry:")
        out.append("        e = self.get(k)")
        out.append("        if e is not None:")
        out.append("            return e")
        out.append("        raise KeyError(\"no such key\")")
        out.append("")
        out.append(f"    mut def __setitem__(self, k: str, n: {qname}_entry) -> None:")
        out.append("        for i in range(len(self.elements)):")
        out.append("            if self.elements[i].name == k:")
        out.append("                self.elements[i] = n")
        out.append("                return")
        out.append("        self.elements.append(n)")
        out.append("")
        out.append(f"    mut def __delitem__(self, k: str) -> None:")
        out.append("        for i in range(len(self.elements)):")
        out.append("            if self.elements[i].name == k:")
        out.append("                del self.elements[i]")
        out.append("                return")
        out.append("        raise KeyError(\"no such key\")")
        out.append("")
    return out


def container_class(qname: str, list_names: list[str]) -> list[str]:
    """MNode container holding one attribute per keyed list."""
    out = []
    out.append(f"class {qname}(yadata.MNode):")
    for i, ln in enumerate(list_names):
        out.append(f"    lst{i}: {ln}")
    out.append("")
    out.append("    mut def __init__(self) -> None:")
    for i, ln in enumerate(list_names):
        out.append(f"        self.lst{i} = {ln}()")
    out.append("")
    out.append("    mut def to_gdata(self) -> str:")
    out.append(f"        return '{qname}'")
    out.append("")
    out.append(f"    mut def copy(self) -> {qname}:")
    out.append(f"        res = {qname}()")
    for i, _ in enumerate(list_names):
        out.append(f"        res.lst{i} = self.lst{i}.copy()")
    out.append("        return res")
    out.append("")
    return out


def schema_module(mod_idx: int, containers: int, lists: int, style: str, keys: str) -> str:
    out = []
    out.append('"""Generated schema module (mimics acton-yang adata output)."""')
    out.append("")
    out.append("import yadata")
    if mod_idx > 0:
        out.append(f"import schema_{mod_idx - 1:03d} as prev")
    out.append("")
    container_names = []
    for c in range(containers):
        cq = f"m{mod_idx:03d}__c{c}"
        list_names = []
        for l in range(lists):
            lq = f"{cq}__lst{l}"
            leaf_count = leaf_count_for(mod_idx, c, l)
            out.extend(entry_class(lq, leaf_count, keys))
            out.extend(list_class(lq, leaf_count, style, keys))
            list_names.append(lq)
        out.extend(container_class(cq, list_names))
        container_names.append(cq)
    # module root, linking to the previous module's root like YANG augments do
    out.append("class root(yadata.MNode):")
    for c, cq in enumerate(container_names):
        out.append(f"    c{c}: {cq}")
    if mod_idx > 0:
        out.append("    prev_root: ?prev.root")
    out.append("")
    out.append("    mut def __init__(self) -> None:")
    for c, cq in enumerate(container_names):
        out.append(f"        self.c{c} = {cq}()")
    if mod_idx > 0:
        out.append("        self.prev_root = None")
    out.append("")
    out.append("    mut def to_gdata(self) -> str:")
    out.append(f"        return 'm{mod_idx:03d}'")
    out.append("")
    out.append("    mut def copy(self) -> root:")
    out.append("        res = root()")
    for c, _ in enumerate(container_names):
        out.append(f"        res.c{c} = self.c{c}.copy()")
    out.append("        return res")
    out.append("")
    return "\n".join(out)


def yadata_module() -> str:
    return '''"""Base classes for generated data (mimics yang.adata)."""

class MNode(object):
    mut def to_gdata(self) -> str:
        raise NotImplementedError("to_gdata")

    mut def copy(self) -> Self:
        raise NotImplementedError()


class MList[T(MNode)](MNode):
    elements: list[T]
    mut def __init__(self, elements: list[T]) -> None:
        self.elements = elements


extension MList[T(MNode)](Iterable[T]):
    def __iter__(self) -> Iterator[T]:
        return self.elements.__iter__()


class MKeyedList[K(Eq), T(MNode)](MList[T]):
    @property
    _key_matches: pure(T, K) -> bool

    mut def __init__(self, key_matches: pure(T, K) -> bool, elements: list[T]) -> None:
        MList.__init__(self, elements)
        self._key_matches = key_matches

    pure def get(self, key: K) -> ?T:
        for e in self.elements:
            if self._key_matches(e, key):
                return e


extension MKeyedList[K(Eq), T(MNode)](Indexed[K, T]):
    def __getitem__(self, key: K) -> T:
        e = self.get(key)
        if e is not None:
            return e
        raise KeyError("no such key")

    mut def __setitem__(self, key: K, n: T) -> None:
        for i in range(len(self.elements)):
            if self._key_matches(self.elements[i], key):
                self.elements[i] = n
                return
        self.elements.append(n)

    mut def __delitem__(self, key: K) -> None:
        for i in range(len(self.elements)):
            if self._key_matches(self.elements[i], key):
                del self.elements[i]
                return
        raise KeyError("no such key")
'''


def consumer_func(fn_idx: int, src_mod: int, dst_mod: int, containers: int,
                  lists: int, blocks: int, style: str, keys: str) -> list[str]:
    """A service-discovery style transform: walk keyed lists in the source
    tree, filter on leaf values, mirror entries into the destination tree."""
    compound = keys == "compound"
    out = []
    out.append(f"def _extract_{fn_idx:02d}(dev: schema_{src_mod:03d}.root, out: schema_{dst_mod:03d}.root) -> None:")
    for b in range(blocks):
        c = b % containers
        l = b % lists
        dc = (b + 1) % containers
        dl = (b + 3) % lists
        # the extra leaf is read from dev.c{dc}.lst{dl} entries and written to
        # out.c{dc}.lst{dl} entries; only touch leaves that exist in both
        n_common = min(leaf_count_for(src_mod, dc, dl), leaf_count_for(dst_mod, dc, dl))
        extras = [n for n, _ in LEAF_POOL[4:n_common]]
        v = f"b{b}"
        if compound and style == "keyed":
            key_args = f"(name={v}.name, port={v}.port)"
        elif compound:
            key_args = f"{v}.name, {v}.port"
        else:
            key_args = f"{v}.name"
        out.append(f"    for {v} in dev.c{c}.lst{l}:")
        out.append(f"        if {v}.name == \"skip\":")
        out.append(f"            continue")
        out.append(f"        {v}_addr = {v}.address")
        if compound:
            out.append(f"        if {v}_addr is None:")
            out.append(f"            continue")
            create_extra = f"address={v}_addr"
        else:
            out.append(f"        {v}_port = {v}.port")
            out.append(f"        if {v}_addr is None or {v}_port is None:")
            out.append(f"            continue")
            create_extra = f"address={v}_addr, port={v}_port"
        out.append(f"        {v}_peer = dev.c{dc}.lst{dl}.get({key_args})")
        out.append(f"        if {v}_peer is not None and {v}_peer.enabled is not None:")
        out.append(f"            {v}_e = out.c{dc}.lst{dl}.create({key_args}, {create_extra})")
        out.append(f"            {v}_e.enabled = {v}_peer.enabled")
        if extras:
            extra = extras[b % len(extras)]
            out.append(f"            {v}_x = {v}_peer.{extra}")
            out.append(f"            if {v}_x is not None:")
            out.append(f"                {v}_e.{extra} = {v}_x")
        out.append(f"        else:")
        out.append(f"            {v}_new = out.c{dc}.lst{dl}.create({key_args})")
        out.append(f"            {v}_new.address = {v}_addr")
        if not compound:
            out.append(f"            {v}_new.port = {v}_port")
    out.append("")
    return out


def consumer_module(modules: int, containers: int, lists: int, funcs: int,
                    blocks: int, style: str, keys: str) -> str:
    out = []
    out.append('"""Consumer in the style of test_service_discovery."""')
    out.append("")
    # import a handful of schema modules directly; the import chain pulls in
    # the rest of the transitive closure
    top = modules - 1
    imported = sorted(set([top, top // 2, max(0, top - 1), max(0, top - 2), 0]))
    for m in imported:
        out.append(f"import schema_{m:03d}")
    out.append("")
    for f in range(funcs):
        src_mod = imported[f % len(imported)]
        dst_mod = imported[(f + 1) % len(imported)]
        out.extend(consumer_func(f, src_mod, dst_mod, containers, lists, blocks,
                                 style, keys))
    # a root actor so the project has an executable root
    out.append("actor main(env):")
    out.append(f"    dev = schema_{top:03d}.root()")
    out.append(f"    out_ = schema_{top:03d}.root()")
    for f in range(funcs):
        src_mod = imported[f % len(imported)]
        dst_mod = imported[(f + 1) % len(imported)]
        out.append(f"    _extract_{f:02d}(schema_{src_mod:03d}.root(), schema_{dst_mod:03d}.root())")
    out.append("    env.exit(0)")
    out.append("")
    return "\n".join(out)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", required=True, help="project directory to create")
    ap.add_argument("--modules", type=int, default=6, help="number of schema modules")
    ap.add_argument("--containers", type=int, default=2, help="containers per module")
    ap.add_argument("--lists", type=int, default=20, help="keyed lists per container")
    ap.add_argument("--funcs", type=int, default=8, help="transform functions in consumer")
    ap.add_argument("--blocks", type=int, default=12, help="statement blocks per function")
    ap.add_argument("--style", choices=["keyed", "old"], default="keyed",
                    help="keyed: generic MKeyedList base (acton-yang#472); old: per-class get/Indexed")
    ap.add_argument("--keys", choices=["scalar", "compound"], default="scalar",
                    help="scalar: str key (name); compound: (name, port) key -- "
                         "a named tuple with --style keyed, one argument per "
                         "key leaf with --style old")
    args = ap.parse_args()

    src = os.path.join(args.out, "src")
    if os.path.exists(args.out):
        shutil.rmtree(args.out)
    os.makedirs(src)

    with open(os.path.join(args.out, "Build.act"), "w") as f:
        f.write('name = "attr_lookup_repro"\n')
        f.write("fingerprint = 0x6191f570ef692f2b\n")

    with open(os.path.join(src, "yadata.act"), "w") as f:
        f.write(yadata_module())

    for m in range(args.modules):
        with open(os.path.join(src, f"schema_{m:03d}.act"), "w") as f:
            f.write(schema_module(m, args.containers, args.lists, args.style,
                                  args.keys))

    with open(os.path.join(src, "consume.act"), "w") as f:
        f.write(consumer_module(args.modules, args.containers, args.lists,
                                args.funcs, args.blocks, args.style, args.keys))

    n_lists = args.modules * args.containers * args.lists
    print(f"generated {args.modules} schema modules, {n_lists} {args.style} lists "
          f"({args.keys} keys, "
          f"{2 * n_lists + args.modules * (args.containers + 1)} classes) in {args.out}")


if __name__ == "__main__":
    main()
