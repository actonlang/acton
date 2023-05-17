#!/usr/bin/env python3

import argparse
import os
import re

found_classes = {}

def found_class(name,
                gcbm_declared=None,
                gcbm_defined=None,
                gcbm_zeroinit=None,
                gcbm_safeinit=None,
                gcbm_set=None,
                gcdescr_declared=None,
                gcdescr_zeroinit=None,
                gcdescr_set=None,
                malloc=0,
                tmalloc=0):
    if name not in found_classes:
        found_classes[name] = {
            "gcbm_declared": gcbm_declared,
            'gcbm_defined': gcbm_defined,
            'gcbm_zeroinit': gcbm_zeroinit,
            'gcbm_safeinit': gcbm_safeinit,
            'gcbm_set': gcbm_set,
            'gcdescr_declared': gcdescr_declared,
            'gcdescr_zeroinit': gcdescr_zeroinit,
            'gcdescr_set': gcdescr_set,
            'malloc': 0,
            'tmalloc': 0,
        }

    if gcbm_declared is not None:
        found_classes[name]['gcbm_declared'] = gcbm_declared
    if gcbm_defined is not None:
        found_classes[name]["gcbm_defined"] = gcbm_defined
    if gcbm_zeroinit is not None:
        found_classes[name]["gcbm_zeroinit"] = gcbm_zeroinit
    if gcbm_safeinit is not None:
        found_classes[name]["gcbm_safeinit"] = gcbm_safeinit
    if gcdescr_declared is not None:
        found_classes[name]["gcdescr_declared"] = gcdescr_declared
    if gcdescr_zeroinit is not None:
        found_classes[name]["gcdescr_zeroinit"] = gcdescr_zeroinit
    if gcdescr_set is not None:
        found_classes[name]["gcdescr_set"] = gcdescr_set

    found_classes[name]["malloc"] += malloc
    found_classes[name]["tmalloc"] += tmalloc


def find_c_and_h_files(root_dir):
    for dirpath, dirnames, filenames in os.walk(root_dir):
        for filename in filenames:
            if filename.endswith(".c") or filename.endswith(".h"):
                file_path = os.path.join(dirpath, filename)
                if filename.endswith(".c"):
                    process_file(file_path, check_c_file)
                elif filename.endswith(".h"):
                    process_file(file_path, check_h_file)

def process_file(file_path, check_function):
    print(f"Processing {file_path}")
    lines = read_file(file_path)
    check_function(lines)

def read_file(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    return lines

def check_c_file(lines):
    for line in lines:
        # Look for definition of the GC bitmap
        m = re.match(r"^GC_word ([^ ]+)D_gcbm\[", line)
        if m:
            found_class(m.group(1), gcbm_defined=True)

        # Check if memset is used to initialize the GC bitmap _gcbm
        m = re.search(r"memset\(&([^ ]+)D_gcbm, 0, sizeof\([^ ]+D_gcbm\)\);", line)
        if m:
            found_class(m.group(1), gcbm_safeinit=True)

        # Find struct definitions for our classes, i.e. the struct used will end
        # with G_class and the name will end with G_methods
        m = re.match(r"^struct\s+([^ ]+)G_class ([^ ]+)G_methods = {", line)
        if m:
            found_class(m.group(2), gcdescr_zeroinit=True)

        # Look for GCdescr in the method table being set using GC_make_descriptor
        m = re.match(r" +([^ ]+)G_methods.\$GCdescr = GC_make_descriptor", line)
        if m:
            found_class(m.group(1), gcdescr_set=True)

        # Look for plain malloc
        m = re.search(r"malloc\(sizeof\(struct ([^)]+)\)", line)
        if m:
            found_class(m.group(1), malloc=1)

        # Look for GC_MALLOC_EXPLICITLY_TYPED using the class name and GCdescr
        m = re.search(r"GC_MALLOC_EXPLICITLY_TYPED\(sizeof\(struct ([^ ]+)\), ([^ ]+)G_methods.\$GCdescr\)", line)
        if m:
            found_class(m.group(1), tmalloc=1)


def check_h_file(lines):
    in_struct = None
    for line in lines:
        m = re.match(r"^struct ([^ ]+)G_class {", line)
        if m:
            in_struct = m.group(1)

        if in_struct is not None:
            if line.strip() == "};":
                in_struct = None
            if re.match(r"^ +GC_descr \$GCdescr;", line):
                found_class(in_struct, gcdescr_declared=True)

        m = re.match(r"extern GC_word ([^ ]+)D_gcbm", line)
        if m:
            found_class(m.group(1), gcbm_declared=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Find and patch .c and .h files.")
    parser.add_argument(
        "-d", "--directory", type=str, default=".", help="The directory to scan (default: current directory)"
    )
    args = parser.parse_args()

    find_c_and_h_files("builtin")
    find_c_and_h_files("rts")
    find_c_and_h_files("stdlib")
    # Show found classes and their status as indicated in the found_classes
    # dict. Each field is marked with an X if set to True or an empty space if
    # set to False. The fields are:
    #  - gcbm_declared: _gcbm declared in .h file
    #  - gcbm_defined: _gcbm defined in .c file but not initialized
    #  - gcbm_zeroinit: _gcbm initialized in .c file to 0
    #  - gcbm_safeinit: _gcbm initialized in .c file to 1 (will be scanned, thus safe)
    #  - gcbm_set: _gcbm set to proper value
    #  - gcdescr_defined: gcdescr defined in .h file
    #  - gcdescr_set: gcdescr set to proper value
    #  - malloc: malloc is used in a struct definition
    for c in sorted(found_classes.keys()):
        cl = found_classes[c]
        # Skip classes that don't have anything set, they're likely not relevant
        # for us, e.g. plain mallocs of entirely different classes
        print(f"{c:40} BM:" +
              f"{cl['gcbm_declared'] and 'd' or ' '}" +
              f"{cl['gcbm_defined'] and 'D' or ' '}" +
              f"{cl['gcbm_zeroinit'] and '0' or ' '}" +
              f"{cl['gcbm_safeinit'] and '1' or ' '}" +
              f"{cl['gcbm_set'] and '+' or ' '}" +
              " GCdescr:" +
              f"{cl['gcdescr_declared'] and 'd' or ' '}" +
              f"{cl['gcdescr_declared'] and 'D' or ' '}" +
              f"{cl['gcdescr_zeroinit'] and '0' or ' '}" +
              f"{cl['gcdescr_set'] and '+' or ' '}" +
              f"   malloc: {cl['malloc']} / {cl['tmalloc']}")
