#!/usr/bin/env python3
"""Script to run Acton examples embedded in Markdown and update output

Will find first source code example, run it and insert the output back into the
Markdown file. The document is expected to contain
"""

import argparse
import os
import re
import subprocess
import tempfile
import time

# TODO: This is the name of the language for the src block in the markdown file.
# It should really be acton, but we don't get syntax hilighting with that
# (nothing supports that), so using python for now
SRCNAME='python'

def run_cmd(workdir, cmd):
    current_dir = os.getcwd()
    acton = os.path.join(current_dir, "../dist/bin/acton")
    # Run acton in the specified working directory
    os.environ["PATH"] = os.path.join(current_dir, "../dist/bin") + ":" + os.getenv("PATH")
    cp = subprocess.run(cmd, shell=True, cwd=workdir, capture_output=True, text=True)
    # filter out color codes
    output = re.sub(r'\x1b\[[0-9;]*m', '', cp.stdout)
    output = re.sub(r'Building project in .*', 'Building project in /home/user/foo', output)
    return output

def run(source, cmd=None):
    cmd = cmd or "acton build && out/bin/example"
    with tempfile.TemporaryDirectory() as tmpdirname:
        print(f"Got a temp dir: {tmpdirname}")
        try:
            run_cmd(tmpdirname, "acton new example")
            # stupid hack to get around some bug on macos where the newly
            # created directory (by acton) is not visible to us, how?
            time.sleep(0.1)
            source_file = open(tmpdirname + "/example/src/example.act", "w")
            source_file.write(source)
            source_file.close()
            output = run_cmd(os.path.join(tmpdirname, "example"), cmd)
            return output
        except Exception as exc:
            print(exc)
            print(f"ERROR, check {tmpdirname}")
            time.sleep(60)

def get_source_from_md(content):
    """Extract Acton source code from Markdown document

    We just get the first source block and it should look like:

        Source:
        ```acton
        actor main....
        ```

    The starting "Source:" line is significant. The source block should
    immediately follow it.
    """
    look_for = '^Source:$'
    source = ""
    filename = None
    for line in content:
        if look_for == '^Source:$' and re.search(look_for, line):
            look_for = f"^```{SRCNAME}"
            continue

        if look_for == f"^```{SRCNAME}":
            if not re.search(look_for, line):
                raise ValueError(f"Didn't find source block, ```{SRCNAME} should follow the Source: line")

            m = re.match(r"^```{SRCNAME} (.+)$", line)
            if m:
                filename = m.group(1)
            look_for = '^```$'
            continue

        if look_for == "^```$":
            if re.search(look_for, line):
                return source, filename
            source += line

    if look_for == '^Source:$':
        raise ValueError("Didn't find source block, missing 'Source:' line")


def get_run_from_md(content):
    """Extract Acton source code from Markdown document

    We just get the first run block and it should look like:

        Run:
        ```sh
        acton test
        ```
    """
    look_for = '^Run:$'
    source = ""
    for line in content:
        if look_for == '^Run:$' and re.search(look_for, line):
            look_for = f"^```sh$"
            continue

        if look_for == f"^```sh$":
            if not re.search(look_for, line):
                raise ValueError(f"Didn't find source block, ```{SRCNAME} should follow the Source: line")
            look_for = '^```$'
            continue

        if look_for == "^```$":
            if re.search(look_for, line):
                return source
            source += line


def write_output_to_md_content(output, md_content):
    new = ""
    look_for = '^Output:$'
    for line in md_content:
        if look_for == '^Output:$':
            new += line
            if re.search(look_for, line):
                look_for = "^```sh$"
                continue

        elif look_for == "^```sh$":
            new += line
            if not re.search(look_for, line):
                raise ValueError("Didn't find output block, ```sh should follow the Output: line")
            new += output
            look_for = '^```$'
            continue
        elif look_for == "^```$":
            if re.search(look_for, line):
                new += line
                look_for = None
                continue
        elif look_for is None:
            new += line

    if look_for == '^Output:$':
        raise ValueError("Didn't find output block, missing 'Output:' line")

    return new


def write_md(fn, content):
    with open(fn, "w") as f:
        f.write(content)


def process(md):
    f = open(md, "r")
    md_content = f.readlines()
    f.close()
    #print("MARKDOWN:")
    #print("----------------------------------------------------")
    #print("".join(md_content))
    #print("----------------------------------------------------")
    source, src_file = get_source_from_md(md_content)
    cmd = get_run_from_md(md_content)
    #print("SOURCE:")
    #print(source)
    output = run(source, cmd)
    #print("OUTPUT:")
    #print(output)
    new_md = write_output_to_md_content(output, md_content)

    print("NEW MARKDOWN:")
    print("----------------------------------------------------")
    print("".join(new_md))
    print("----------------------------------------------------")
    write_md(md, new_md)



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("md", nargs="+")

    args = parser.parse_args()

    for md in args.md:
        process(md)
