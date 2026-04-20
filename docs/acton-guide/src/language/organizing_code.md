# Organizing code

As a program grows, you need more than a single file full of functions.
The goal is not to create a deep tree of files. The goal is to make the
code easier to read, change, and import.

<div class="beginner-content">
<p>Start by grouping related code together, then split it when it begins
to serve more than one purpose. A practical first cut is often one
module for parsing, one for domain logic, and one for I/O or startup
code. Keep local modules, project metadata, and external dependencies
in one clear path rather than treating them as separate puzzles.</p>

<p>You do not need a deep hierarchy early on. Small modules with clear
names are usually easier to work with than a large set of thin files
with vague responsibilities.</p>
</div>

This section connects the local source tree, the project file, and the
package graph. Use it together with [Projects](../projects.md) and
[Package Management](../package_management.md).

<div class="advanced-content">
<p>In Acton, a module boundary is more than a foldering choice. Because
module-level bindings are constant, a module behaves much more like a
namespace and API surface than like an object with hidden mutable
runtime state. That means a good split changes both how people read the
program and how the compiler sees it.</p>

<p>A coherent module split narrows imports, isolates reasons to change,
and makes the public surface smaller. It also improves the mechanical
side of the toolchain: discovery, type checking, caching, and API
documentation all work in terms of modules. Once a file becomes a real
boundary that other code imports on purpose, its top-level names and
docstrings stop being decoration and start becoming part of the module's
interface.</p>
</div>

This section is about the everyday structure around your code:

- how to split code into modules
- how to import names
- how naming conventions make code easier to read

Read next:

- [Modules](../modules.md)
- [Projects](../projects.md)
- [Package Management](../package_management.md)
- [Naming](../naming.md)
