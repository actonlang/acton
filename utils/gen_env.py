#!/usr/bin/env python3

from collections import namedtuple
import re
import subprocess
import sys

ACTONC = "dist/bin/actonc"

Typedef = namedtuple('Typedef', ['name', 'type', 'body', 'generated'])
StructDec = namedtuple('StructDec', ['name', 'body', 'generated'])
Struct = namedtuple('Struct', ['name', 'type', 'body', 'generated'])
FuncDec = namedtuple('FuncDec', ['name', 'returns', 'args', 'body', 'generated'])
Func = namedtuple('Func', ['name', 'returns', 'args', 'body', 'generated'])


def read_file(filename):
    with open(filename, "r") as f:
        return [line.rstrip('\n') for line in f.readlines()]


def write_file(filename, lines):
    with open(filename, "w") as f:
        for line in lines:
            f.write(line)


def get_stuff(lines, input_generated=False):
    """Extract C functions & structs from list of lines into list of namedtuple
    objects, Func() & Struct()

    List of returned objects is in same order as in file

    We use markers in the file to determine if something has been generated or not
    """
    res = []
    cur_func = None
    cur_struct = None
    body = []
    generated = input_generated
    for line in lines:
        if re.match(r"// START GENERATED", line):
            generated = True
        if re.match(r"// END GENERATED", line):
            generated = False

        if cur_func:
            if re.match("}$", line):
                body.append(line)
                # end of func
                res.append(Func(
                    name = cur_func.name,
                    returns = cur_func.returns,
                    args = cur_func.args,
                    body = body,
                    generated = generated
                ))
                cur_func = None
                body = []
            else:
                body.append(line)
        elif cur_struct:
            if re.match("};$", line):
                body.append(line)
                # end of func
                res.append(Struct(
                    name = cur_struct.name,
                    type = cur_struct.type,
                    body = body,
                    generated = generated
                ))
                cur_struct = None
                body = []
            else:
                body.append(line)
        else:
            # Function start
            m = re.match(r"(?P<returns>[^ ][^\(\)]*) (?P<name>[^ ]+) ?\((?P<args>[^\)]*)\) {", line)
            if m:
                body = [line]
                cur_func = Func(
                    name = m.group('name'),
                    returns = m.group('returns'),
                    args = m.group('args'),
                    body = None,
                    generated = generated
                )

            # Function single line (header declaration)
            m = re.match(r"(?P<returns>[^ ][^\(\)]*) (?P<name>[^ ]+) ?\((?P<args>[^\)]*)\);", line)
            if m:
                res.append(FuncDec(
                    name = m.group('name'),
                    returns = m.group('returns'),
                    args = m.group('args'),
                    body = [line],
                    generated = generated
                ))

            # Struct start
            m = re.match(r"struct (?P<name>[^ ]+) {", line)
            if m:
                body = [line]
                cur_struct = Struct(
                    name = m.group('name'),
                    type = m.group('name'),
                    body = None,
                    generated = generated
                )

            m = re.match(r"typedef ((?P<type>(struct )?[^ ]+) )?(?P<name>[^ ]+);", line)
            if m:
                res.append(Typedef(
                    name = m.group('name'),
                    type = m.group('type'),
                    body = [line],
                    generated = generated
                ))

            # Struct single line
            m = re.match(r"(extern )?struct (?P<name>[^ ]+);", line)
            if m:
                res.append(StructDec(
                    name = m.group('name'),
                    body = [line],
                    generated = generated
                ))


            # Struct single line
            m = re.match(r"(extern )?struct (?P<type>[^ ]+) (?P<name>[^ ]+);", line)
            if m:
                res.append(Struct(
                    name = m.group('name'),
                    type = m.group('type') or m.group('name'),
                    body = [line],
                    generated = generated
                ))

    return res


def cgen_builtin():
    ac = subprocess.run([ACTONC, "--stub", "--cgen", "stdlib/src/__builtin__.act"], stdout=subprocess.PIPE)
    return ac.stdout.decode('utf-8').splitlines()

def hgen_builtin():
    ac = subprocess.run([ACTONC, "--stub", "--hgen", "stdlib/src/__builtin__.act"], stdout=subprocess.PIPE)
    return ac.stdout.decode('utf-8').splitlines()


skip_funcs = ['$Env$new', '$Connection$new', '$ListenSocket$new', '$RFile$new', '$WFile$new']

def get_env_cgen_stuff():
    """Get the generated stuff

    Quite hard since there are no markers so we use dumb heuristics to find the
    stuff related to env!

    Currently, all the env related things are towards the end of the file
    generated C file, so we just need to find the start, which is the first line
    that has a $Env parameter to a lambda. Changes to __builtin__.act could
    change this, so perhaps we need a different condition for figuring out the
    start.

    Skip __init__ function, since it needs modifications inside the function
    body, so it's handled separately.
    """
    res = []
    found = False
    for thing in get_stuff(cgen_builtin(), input_generated=True):
        if re.search('\$Env', thing.body[0]):
            found = True
        if found and thing.name not in skip_funcs:
            res.append(thing)
    return res


def get_env_cgen_init():
    """Get the interesting bits in the __init__ function generated by cgen

    Does not return the entire function but just part of the body that we are
    interested in
    """
    res = []
    found = False
    init_fun = [f for f in get_stuff(cgen_builtin()) if f.name == '$__init__'][0]
    for line in init_fun.body[:-1]:
        if not found and re.search(r"\$l\$1lambda", line):
            found = True
            res.append("    {")
        if found:
            res.append(line)

    return res


def get_env_hgen_stuff():
    res = []
    found = False
    for thing in get_stuff(hgen_builtin(), input_generated=True):
        if re.match(r"struct .l.1lambda;", thing.body[0]):
            found = True
        if found and thing.name not in skip_funcs:
            res.append(thing)
    return res


def filter_out(things, others):
    res = []

    errors = False
    for thing in things:
        thkey = (type(thing), thing.name)
        if thkey in others:
            o = others[thkey]
            if o.generated:
                print(f"Overwriting {thing.name} in generated section")
                res.append(thing)
            else:
                print(f"Using manually defined {thing.name}")
            if isinstance(thing, Func):
                if thing.returns != o.returns:
                    print(f"ERROR: {thing.name} return type differs: ({thing.returns}) vs ({o.returns})")
                    errors = True
                if thing.args != o.args:
                    print(f"ERROR: {thing.name} args differs: ({thing.args}) vs ({o.args})")
                    errors = True
            if isinstance(thing, Struct):
                if thing.type != o.type:
                    print(f"ERROR: {thing.name} struct type differs: ({thing.type}) vs ({o.type})")
                    errors = True
            continue
        res.append(thing)

#    if errors:
#        return False

    return res


def gen_envc():
    out = []
    env_in_funcs = {}

    envc_stuff = get_stuff(read_file("builtin/env.c"))
    envc_stuff_dict = {(type(thing), thing.name): thing for thing in envc_stuff}

    env_cgen_stuff = filter_out(get_env_cgen_stuff(), envc_stuff_dict)
    if env_cgen_stuff is False:
        print("ERROR: return / args mismatch, returning early...")
        return

    skip = False
    # itereate through env.c, find our markers where we a) skip the existing
    # lines between the markers and b) insert the new content we prepared
    for line in read_file("builtin/env.c"):
        if re.match(r"// START GENERATED __builtin__.act", line):
            skip = True
            out.append(line)
            for thing in env_cgen_stuff:
                if thing.name == '$__init__':
                    continue
                for thing_line in thing.body:
                    out.append(thing_line)

        if re.match(r"// END GENERATED __builtin__.act", line):
            skip = False

        if re.match(r"    // START GENERATED __builtin__.act \$__init__", line):
            skip = True
            out.append(line)
            for l in get_env_cgen_init():
                out.append(l)

        if re.match(r"    // END GENERATED __builtin__.act \$__init__", line):
            skip = False

        if not skip:
            out.append(line)

    write_file("builtin/env.c", "\n".join(out))


def gen_envh():
    out = []
    skip = False

    envh_stuff = get_stuff(read_file("builtin/env.h"))
    envh_stuff_dict = {(type(thing), thing.name): thing for thing in envh_stuff}

    env_hgen_stuff = filter_out(get_env_hgen_stuff(), envh_stuff_dict)

    for line in read_file("builtin/env.h"):
        if re.match("// START GENERATED __builtin__.act", line):
            skip = True
            out.append(line)
            for thing in env_hgen_stuff:
                for thing_line in thing.body:
                    out.append(thing_line)

        if re.match("// END GENERATED __builtin__.act", line):
            skip = False

        if not skip:
            out.append(line)


    write_file("builtin/env.h", "\n".join(out))


def check_env(filename):
    errors = False
    env_stuff = get_stuff(read_file(filename))
    env_stuff_dict = {(type(thing), thing.name): thing for thing in env_stuff}
    if filename == "builtin/env.c":
        env_gen_stuff = get_env_cgen_stuff()
    elif filename == "builtin/env.h":
        env_gen_stuff = get_env_hgen_stuff()
    else:
        raise Exception("Unsupported file")
    for thing in env_gen_stuff:
        thkey = (type(thing), thing.name)

        try:
            et = env_stuff_dict[thkey]
        except KeyError:
            print(f"ERROR: Missing {thkey}")
            errors = True
            continue

        if isinstance(thing, FuncDec):
            if thing.returns != et.returns:
                print(f"ERROR: {thing.name} return type differs: ({thing.returns}) vs ({et.returns})")
                errors = True
            if thing.args != et.args:
                print(f"ERROR: {thing.name} args differs: ({thing.args}) vs ({et.args})")
                errors = True
        elif isinstance(thing, Func):
            if thing.returns != et.returns:
                print(f"ERROR: {thing.name} return type differs: ({thing.returns}) vs ({et.returns})")
                errors = True
            if thing.args != et.args:
                print(f"ERROR: {thing.name} args differs: ({thing.args}) vs ({et.args})")
                errors = True
        elif isinstance(thing, StructDec):
            pass
        elif isinstance(thing, Struct):
            if thing.type != et.type:
                print(f"ERROR: {thing.name} struct type differs: ({thing.type}) vs ({et.type})")
                errors = True
            gen_len = len(thing.body[:-1])
            if thing.body[0:gen_len] != et.body[0:gen_len]:
                print(f"ERROR: {thing.name} struct fields differs")
                errors = True
        elif isinstance(thing, Typedef):
            if thing.type != et.type:
                print(f"ERROR: {thing.name} typedef differs: ({thing.type}) vs ({et.type})")
                errors = True
        else:
            raise ValueError(f"Unhandled type: {type(thing)}")

        if thing.body != et.body:
            if et.generated:
                print(f"ERROR: {thing.name} body differs")
                errors = True

    return errors


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--check', action="store_true")
    parser.add_argument('--gen', action="store_true")
    args = parser.parse_args()

    if args.check:
        errors = False

        print("Checking builtin/env.c")
        if check_env("builtin/env.c") is True:
            errors = True

        print("Checking builtin/env.h")
        if check_env("builtin/env.h") is True:
            errors = True

        if errors:
            sys.exit(1)
        else:
            sys.exit(0)

    if args.gen:
        gen_envc()
        gen_envh()
