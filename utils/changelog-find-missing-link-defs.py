#!/usr/bin/env python3
# Script to find short link refs, like '[#123]' in CHANGELOG.md which we use for
# issue and pull request references. Maintaining the list of link definitions at
# the end of the doc is a sort of PITA which this script helps with.
#
# Usage:
# kll@Boxy ~/acton$ utils/changelog-find-missing-link-defs.py
# Missing link definitions:
# [#315]: https://github.com/actonlang/acton/pull/315
# [#431]: https://github.com/actonlang/acton/issues/431
# [#512]: https://github.com/actonlang/acton/pull/512
# [#514]: https://github.com/actonlang/acton/pull/514
# [#527]: https://github.com/actonlang/acton/issues/527
# ...
#
# Copy the output text and add to CHANGELOG.md
#
# Sort the output with: '<,'>!awk -F'[\#\\]]' '{ print $2 "\t" $0 }' | sort -n | cut -f 2

import re
import requests

def find_short_link_refs(lines):
    res = {}
    for line in lines:
        m = re.search(r"[^]](\[#[0-9]+\])[^:[\(]", line)
        if m:
            res[m.group(1)] = False
    return res

def find_link_defs(lines):
    res = {}
    for line in lines:
        m = re.search(r"^(\[#[0-9]+\]): (.*)", line)
        if m:
            res[m.group(1)] = m.group(2)
    return res

def get_url(tag):
    # We don't know if a particular number tag is a pull request or an issue.
    # Github will automatically redirect us though, so we just go pull/{num} and
    # then check the returned url if it says issue or pull.
    num = re.sub("[^0-9]", "", tag)
    r = requests.get(f"https://github.com/actonlang/acton/pull/{num}")
    return r.url


f = open("CHANGELOG.md")
lines = f.readlines()
f.close()

links = find_short_link_refs(lines)
defs = find_link_defs(lines)
missing = sorted(set(links) - set(defs))
print("Missing link definitions:")
for num in missing:
    url = get_url(num)
    print(f"{num}: {url}")
