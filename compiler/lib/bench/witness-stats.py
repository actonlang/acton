#!/usr/bin/env python3
"""Summarize --witness-stats JSON lines from one compiler/corpus run."""
import argparse
import json
from collections import Counter, defaultdict


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logs", nargs="+", help="Compiler output files; do not mix before/after runs")
    parser.add_argument("--prefix", default="", help="Include only modules beginning with this prefix")
    parser.add_argument("--json", action="store_true", help="Print a machine-readable summary")
    args = parser.parse_args()
    totals = defaultdict(Counter)
    modules = defaultdict(set)
    candidates = Counter()
    registries = defaultdict(Counter)
    registry_groups = defaultdict(lambda: defaultdict(Counter))
    seen = set()
    for filename in args.logs:
        with open(filename, encoding="utf-8") as stream:
            for lineno, line in enumerate(stream, 1):
                if not line.startswith("{"):
                    continue
                try:
                    row = json.loads(line)
                except ValueError as error:
                    parser.error(f"{filename}:{lineno}: invalid JSON: {error}")
                if row.get("kind") != "witness-stats" or not row["module"].startswith(args.prefix):
                    continue
                if row["version"] != 1:
                    parser.error(f"unsupported witness statistics version: {row['version']}")
                key = row["module"], row["stage"]
                if key in seen:
                    parser.error(f"duplicate module/stage {key}: summarize each build separately")
                seen.add(key)
                stage = row["stage"]
                modules[stage].add(row["module"])
                totals[stage].update({k: v for k, v in row.items() if type(v) is int and k != "version"})
                if "registry" in row:
                    for key, value in row["registry"].items():
                        if type(value) is int:
                            registries[stage][key] += value
                        elif isinstance(value, dict):
                            registry_groups[stage][key].update(value)
                if stage == "boxed":
                    candidates[row["module"]] += row["nonstatic_constructor_expressions"]
    result = {stage: {"modules": len(modules[stage]), **totals[stage]} for stage in totals}
    for stage, counts in registries.items():
        result[stage]["registry"] = {**counts, **registry_groups[stage]}
    if args.json:
        print(json.dumps({"stages": result, "boxed_candidates": dict(candidates.most_common())}, indent=2))
    else:
        print("stage        modules  bindings  static bindings  nonstatic recipes  in loops")
        for stage in ("typed", "normalized", "boxed"):
            if stage not in result:
                continue
            r = result[stage]
            print(f"{stage:12} {r['modules']:7} {r['bindings']:9} {r['static_bindings']:16} "
                  f"{r['nonstatic_constructor_expressions']:18} {r['nonstatic_constructor_expressions_in_loops']:9}")
        print("\nLargest remaining nonstatic recipe counts (boxed):")
        for module, count in candidates.most_common(10):
            print(f"{count:7} {module}")
        if registries:
            print("\nRegistry: nonstatic constructor sites by dependency lifetime")
            print("stage          closed  parameter    local  unknown  closed descriptions")
            for stage in ("typed", "normalized", "boxed"):
                if stage not in registries:
                    continue
                groups = registry_groups[stage]
                c = groups["nonstatic_sites_by_lifetime"]
                d = groups["nonstatic_recipes_by_lifetime"]
                print(f"{stage:12} {c['closed']:8} {c['parameter_dependent']:10} "
                      f"{c['local_dependent']:8} {c['unknown']:8} {d['closed']:20}")
        print("\nRecipe counts describe syntax, not runtime allocations or safely shareable objects.")


if __name__ == "__main__":
    main()
