# Embedding files

`embed("path")` is an expression whose value is the content of a file, as
`bytes`. The compiler takes the content when the program is built and puts
it in the executable, so the program needs no file when it runs.

A project `shop` has the files `data/banner.png` and `data/title.txt` beside
its `src/` directory. The module `src/greeting.act` embeds both:

```python
banner: bytes = embed("data/banner.png")
title: str = embed("data/title.txt").decode()

actor main(env):
    proc def __startup__():
        print(title.strip(), len(banner))
        env.exit(0)
```

The value is the bytes of the file as they are on disk. There is no form for
text: `embed("data/title.txt").decode()` gives a `str`, and it checks and
copies the content when it is evaluated.

`embed` can stand wherever an expression can stand, with two exceptions. It
cannot be the default value of a parameter: give the content a name at the
top level of the module and use the name. And it cannot be used at the prompt
of `acton repl`: write it in a module.

## The path

The path is written in double quotes on one line and is read as it is
written, with no escape sequence and no interpolation.

The path counts from the project root, which is the directory that holds
`Build.act`. In a module of a dependency it counts from the root of the
dependency. In a file that is compiled without a project, as in
`acton greeting.act`, it counts from the directory of that file. Such a file
cannot embed a file when the absolute path of its directory has a double
quote or a control character. The same holds for a project whose `out` is a
symbolic link, with the path of the project root.

A path follows these rules:

- It is relative and has `/` between its components. No component is empty,
  `.` or `..`.
- It has no backslash, no double quote, no brace and no control character.
- Its first component is not `out`, the directory that a build writes.
- It names a regular file of fewer than 2147483648 bytes (2 GiB). A symbolic
  link is followed, and only the content counts.
- Each component is spelled as its directory lists it. Where a file system
  takes `web/Index.html` for the file `web/index.html`, `embed` does not.

A path that breaks a rule, or that names no file, is an error when the module
is compiled.

## The files of a directory

`embed` takes one file, and a path that names a directory is an error. The
files of a directory get into a program through a
[derived module](derived_modules.md), a module whose source a function returns
while the build runs. The function `embed_files` of the module
`std.build.derive` returns a module with one `embed` for each file below a
directory.

The project `shop` has the files `web/index.html` and `web/app.js`. Its
`Build.act` has one entry in the table `derived_modules`:

```python
derived_modules = {
    assets: (function=std.build.derive.embed_files, inputs=["web/"],
             args=(strip="web"))
}
```

The entry makes `assets` a module of the project. A build calls the function
and writes the source that it returns to `out/derived/src/assets.act`:

```python
# Derived by std.build.derive.embed_files.

files: dict[str, bytes] = {
    "/app.js": embed("web/app.js"),
    "/index.html": embed("web/index.html"),
}
```

A module of the project imports `assets` and takes the content of a file from
the dictionary `files`:

```python
import assets

actor main(env):
    proc def __startup__():
        print(len(assets.files["/index.html"]))
        env.exit(0)
```

`inputs=["web/"]` names the files below `web/`, also those in the directories
below it. The section [Input paths](derived_modules.md#input-paths) lists the
files that are left out, such as a file whose name starts with a dot. A backup
file such as `web/app.js~` is not among them, so it is embedded. A directory
without input files gives an empty `files`. A build stops when the directory
is not there.

`strip` is a prefix that is left out of each key of `files`. With
`strip="web"`, the content of `web/index.html` is
`assets.files["/index.html"]`. Without `strip` it is
`assets.files["web/index.html"]`. The prefix is compared with the start of a
path character by character, so `strip="web/"` gives the key `index.html`. A
build stops when the path of a file does not start with the prefix.

A build also stops for a file whose path `embed` refuses. With a file
`web/a{b}.txt`, whose name has braces, the message is:

```console
The function std.build.derive.embed_files of the entry assets of derived_modules in /home/user/shop/Build.act failed:
ValueError: embed cannot name the file "web/a{b}.txt". A path of embed is relative, with / between its components. It has no empty component, no component . or .., no backslash, no double quote, no brace and no control character. Its first component is not out
```

### When a file below the directory changes

The source of the module `assets` holds the path of each file and no content,
and `embed_files` reads no file. The compiler takes the content of each file
as it does for every `embed`.

- A file has new content: the build calls the function, which returns the
  same source, so the build does not write `out/derived/src/assets.act`. It
  compiles the module `assets` again because a file that the module embeds
  has new content. The section
  [When an embedded file changes](#when-an-embedded-file-changes) says what
  else is compiled.
- A file is added or removed: the function returns source with one line more
  or one line fewer. The build writes `out/derived/src/assets.act` and
  compiles the module.

`acton build --watch` and `acton test --watch` build again in both cases.

The language server calls no function. Until the next build it works with the
source of `assets` as the last build wrote it: a file that was added is not in
`files`, and a file that was removed is reported as missing at its line in
`out/derived/src/assets.act`. That report goes when a build has written the
module and the language server compiles it again, which an edit of a module
that imports `assets` starts.

### Another module for the same files

The module that `embed_files` returns has a dictionary from path to content.
A function of the project can return another module for the files of a
directory, for example one that also has an entity tag for each file, which a
web server sends in the header `ETag`. The module `std.build.derive` has two
functions for the source that such a function returns:

- `embed_call(path)` returns the expression `embed("path")` for a path. It
  raises `ValueError`, with the message above, for a path that `embed`
  refuses.
- `str_literal(text)` returns a str literal whose value is `text`.

The function `page_table` in `src/sitegen.act` uses both:

```python
import std.build.derive as derive

def page_table(inputs: dict[str, derive.InputFile]) -> str:
    lines = ["table: dict[str, (body: bytes, etag: str)] = {{"]
    for path, f in inputs.items():
        etag = derive.str_literal('"' + f.sha256[:32] + '"')
        lines.append("    " + derive.str_literal(path)
                     + ": (body=" + derive.embed_call(path) + ", etag=" + etag + "),")
    lines.append("}}")
    return "\n".join(lines) + "\n"
```

A brace in a str literal is written twice, so the function writes `{{` for
the brace that opens the dictionary. With the entry
`pages: (function=sitegen.page_table, inputs=["web/"])`, a build writes this
source for the module `pages`:

```python
table: dict[str, (body: bytes, etag: str)] = {
    "web/app.js": (body=embed("web/app.js"), etag="\"d6447d6f631d17aaf8b0db0adf147064\""),
    "web/index.html": (body=embed("web/index.html"), etag="\"b0693dc92f76e08bf1485b3dd9b514a2\""),
}
```

The function takes the SHA-256 of each file from the description of the file
and reads no file. When a file has new content, the line of that file
changes, and the build writes the source of the module again. The page
[Derived modules](derived_modules.md) describes the function of an entry and
the description of an input file.

## When an embedded file changes

A build looks at each embedded file. It reads a file only when what the file
system records of the file has changed since the last build: its size, the
times of its last changes, or which file is at the path. So a file that did
not change costs a build no read, whatever its size. When a file has new
content, the build compiles the module that embeds it again, and generates
again the code of each module that uses a name whose value holds the content.
`acton test` runs a test again when the test uses such a name. A file that is
written again with the same content is read once and compiles nothing.

`acton build --watch` and `acton test --watch` build again when an embedded
file is saved or removed, and when a file that was missing is created. This
holds for the files that the modules of such a build embed: the modules of
the project that the command builds, and those of its local dependencies.
The build that follows looks at all these modules and compiles only what the
new content changes. The watch of a file that is compiled without a project,
as in `acton greeting.act --watch`, does the same for the files that this
file embeds.

In an editor, the language server reports an embedded file that is missing
when it compiles the module. The report goes when the file is there and the
module is compiled again, which an edit of the module or of a module that it
imports starts.

A build looks at an embedded file more than once: before it compiles a
module, and again after the C compiler has read the file. When the file is
written or removed between two looks, the build stops with a message such as
`data/title.txt changed while the build ran. Run the build again`. The
executable of such a build can hold other content than the module was compiled
with.

A build with `--only-build` compiles no module. When an embedded file has
changed since its module was compiled, such a build stops with a message such
as `data/title.txt changed since its module was compiled. Run the build
without --only-build`.

The module that embeds a file is type checked again each time the content of
the file changes. So a name that holds a large file, or a file that changes
often, belongs in a small module of its own.

Keep an embedded file outside `src/`, for example in a directory `data/` or
`web/` beside it. For a module with an `.ext.c` file that names an included
file through a macro, a build reads every file under `src/` that is no Acton
source, so a large file there slows down each build.

## In the executable

The content of an embedded file is read-only data of the executable. Nothing
is read or copied when the program starts: the operating system loads a part
of the content when the program first reads it. Evaluating `embed` allocates
the header of a `bytes` value and no content.

A file that two modules embed is in the executable twice. Every executable
that holds the module holds the file, also the executable of a test module
that imports it.

The files that one executable embeds should together stay below 1.9 GiB. Above
that, an executable may not link, and for macOS and Windows it may link and
then not start.

## The word embed

`embed` is a reserved word. It cannot name a variable, a function, a
parameter or a class, and it cannot be the first part of a module name. An
attribute or a later part of a module name with this spelling is written in
quotes, as in `page."embed"`.

A module file `src/embed.act` and the modules below a directory `src/embed/`
are compiled, but no module of the same project can import them, since the
first part of a module name cannot be written in quotes.

A project cannot have both `src/tool.act` and `src/tool.embed.act`. The
generated file for the content that a module `tool` embeds is `tool.embed.c`,
and the generated C file of the module of `src/tool.embed.act` has the same
path. A build of such a project stops with a message that names both files.
