#!/usr/bin/env python3

import argparse
import os
import re

found_classes = {}

def found_class(name, gcbm_declared=None, gcbm_defined=None, gcbm_initialized=None, gcbm_set=None, gcdescr_defined=None, gcdescr_set=None, malloc=None):
    if name not in found_classes:
        found_classes[name] = {
            "gcbm_declared": gcbm_declared,
            'gcbm_defined': gcbm_defined,
            'gcbm_initialized': gcbm_initialized,
            'gcbm_set': gcbm_set,
            'gcdescr_defined': gcdescr_defined,
            'gcdescr_set': gcdescr_set,
            'malloc': malloc,
        }
    if gcbm_declared is not None:
        found_classes[name]['gcbm_declared'] = gcbm_declared
    if gcbm_defined is not None:
        found_classes[name]["gcbm_defined"] = gcbm_defined
    if gcbm_initialized is not None:
        found_classes[name]["gcbm_initialized"] = gcbm_initialized
    if gcdescr_defined is not None:
        found_classes[name]["gcdescr_defined"] = gcdescr_defined
    if gcdescr_set is not None:
        found_classes[name]["gcdescr_set"] = gcdescr_set
    if malloc is not None:
        found_classes[name]["malloc"] = malloc

def find_c_and_h_files(root_dir):
    for dirpath, dirnames, filenames in os.walk(root_dir):
        for filename in filenames:
            if filename.endswith(".c") or filename.endswith(".h"):
                file_path = os.path.join(dirpath, filename)
                if filename.endswith(".c"):
                    process_file(file_path, patch_c_file)
                elif filename.endswith(".h"):
                    process_file(file_path, patch_h_file)

def process_file(file_path, patch_function):
    print(f"Processing {file_path}")
    lines = read_file(file_path)
    modified_lines = patch_function(lines)
    write_file(file_path, modified_lines)

def read_file(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    return lines

def write_file(file_path, lines):
    with open(file_path, 'w') as f:
        # For __builtin__.h and __builtin__.c, add include at the top.
        if file_path.endswith("__builtin__.h") or file_path.endswith("__builtin__.c"):
            f.write('#include "gc/gc_typed.h"\n')
        f.writelines(lines)

def patch_c_file(lines):
    res = []
    malloc_line = None
    for line in lines:
        m = re.match(r"^struct (([^ ]+)G_class) ([^ ]+)_methods;", line)
        if m:
            found_class(m.group(2), gcbm_defined=True)
            res.append(line)
            res.append(f"GC_word {m.group(2)}D_gcbm[GC_BITMAP_SIZE(struct {m.group(2)})];\n")
            continue

        # Find struct definitions of method tables, which match a struct
        # definition name ending with _class and a variable name ending with
        # _methods. Insert 0 as the first element when initializing the struct.
        m = re.match(r"^struct ([^ ]+)G_class ([^ ]+)G_methods = {$", line)
        if m:
            found_class(m.group(2), gcbm_initialized=True)
            res.append(f"struct {m.group(1)}G_class {m.group(2)}G_methods = {{\n    0,\n")
            continue

        m = re.match(r"^struct ([^ ]+)G_class ([^ ]+)G_methods = {(.*)", line)
        if m:
            found_class(m.group(2), gcbm_initialized=True)
            res.append(f"struct {m.group(1)}G_class {m.group(2)}G_methods = {{0," + m.group(3))
            continue

        # Replace $GCINFO with $name
        m = re.match(r"^( +)([^ ]+)\.\$GCINFO =(.*)", line)
        if m:
            oname = re.sub(r"G_methods", "", m.group(2))
            gcbm = re.sub(r"G_methods", "D_gcbm", m.group(2))
            res.append(f"{m.group(1)}memset({gcbm}, 0xFF, sizeof({gcbm}));\n")
            found_class(m.group(2).replace("G_methods", ""), gcbm_initialized=True)
            res.append(f"{m.group(1)}{m.group(2)}.$GCdescr = GC_make_descriptor({gcbm}, GC_WORD_LEN(struct {oname}));\n")
            found_class(m.group(2).replace("G_methods", ""), gcdescr_set=True)
            res.append(f"{m.group(1)}{m.group(2)}.$name ={m.group(3)}\n")
            continue

        m = re.search(r"\$class->\$GCINFO", line)
        if m:
            res.append(re.sub(r"\$class->\$GCINFO", "$class->$name", line))
            continue

        # Replace malloc with GC_MALLOC_EXPLICITLY_TYPED
        m = re.match(r"^( +)self = malloc\(sizeof\(struct ([^)]+)\)\);", line)
        if m:
            # Suppress malloc here and emit it on next loop
            malloc_line = m
            #res.append(f"{m.group(1)}{m.group(2)} = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct {m.group(3)}), );\n")
            continue

        m = re.match(r"^( +)self->\$class = &([^;]+G_methods);", line)
        if malloc_line is not None and m:
            if malloc_line.group(2) != m.group(2).replace("G_methods", ""):
                raise ValueError(f"ERROR: malloc and class name mismatch: {malloc_line.group(2)} != {m.group(1)}")

            res.append(f"{malloc_line.group(1)}self = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct {malloc_line.group(2)}), {m.group(2)}.$GCdescr);\n")
            res.append(line)
            found_class(malloc_line.group(2).replace("G_methods", ""), malloc=True)
            malloc_line = None
            continue

        # In line like this:
        #     B_TimesD_str $tmp = malloc(sizeof(struct B_TimesD_str));
        # replace malloc with GC_MALLOC_EXPLICITLY_TYPED
        m = re.match(r"^( +)([^ ]+) \$tmp = malloc\(sizeof\(struct ([^)]+)\)\);", line)
        if m:
            res.append(f"{m.group(1)}{m.group(2)} $tmp = GC_MALLOC_EXPLICITLY_TYPED(sizeof(struct {m.group(3)}), {m.group(3)}G_methods.$GCdescr);\n")
            found_class(m.group(3), malloc=True)
            continue

        res.append(line)
    return res

def patch_h_file(lines):
    res = []
    in_struct = False
    add_after = None
    for line in lines:

        m = re.match(r"^struct ([^ ]+) {", line)
        if m and m.group(1) not in ("$R", "$ROW", "$ROWLISTHEADER"):
            in_struct = True
            if not m.group(1).endswith('_class'):
                found_class(m.group(1), gcbm_declared=True)
                found_class(m.group(1), gcdescr_defined=True)
                add_after = f"extern GC_word {m.group(1)}D_gcbm[GC_BITMAP_SIZE(struct {m.group(1)})];\n"

        if in_struct:
            if line.strip() == "};":
                in_struct = False
            # Replace $GCINFO with $GCdescr
            m = re.match(r"^( +)char.*GCINFO;", line)
            if m:
                res.append(f"{m.group(1)}GC_descr $GCdescr;\n")
                res.append(f"{m.group(1)}char *$name;\n")
                continue

        res.append(line)

        if add_after and re.match(r"^};", line):
            res.append(add_after)
            add_after = None

    return res


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
    #  - gcbm_defined: _gcbm defined in .c file
    #  - gcbm_initialized: _gcbm initialized in .c file to 0
    #  - gcbm_set: _gcbm set to proper value
    #  - gcdescr_defined: gcdescr defined in .h file
    #  - gcdescr_set: gcdescr set to proper value
    #  - malloc: malloc is used in a struct definition
    for c in sorted(found_classes.keys()):
        cl = found_classes[c]
        print(f"{c:40} {cl['gcbm_declared'] and 'X' or ' '}{cl['gcbm_defined'] and 'X' or ' '}{cl['gcbm_initialized'] and 'X' or ' '}{cl['gcbm_set'] and 'X' or ' '}{cl['gcdescr_defined'] and 'X' or ' '}{cl['gcdescr_set'] and 'X' or ' '}{cl['malloc'] and 'X' or ' '}")
