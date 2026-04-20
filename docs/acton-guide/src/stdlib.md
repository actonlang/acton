# Built-in & Standard Library

This part of the guide is reference material.

Acton has two closely related pieces here:

- built-in definitions that are always available, such as core types and
  built-in protocols
- standard library modules that you import explicitly, such as `math`
  and `re`

<div class="beginner-content">
<p>Built-in names are part of the language environment. Standard library
modules are brought into scope with <code>import</code>.</p>
</div>

Use [Built-in protocols](stdlib/builtin_protocols.md) for the protocols
defined in `__builtin__`, including `Eq`, `Ord`, `Hashable`,
`Iterable`, and `Mapping`.

<div class="advanced-content">
<p>This split matches the implementation structure as well: built-in
protocols and core types live in <code>__builtin__</code>, while module
APIs such as <code>math</code> live in separate standard library
modules.</p>
</div>

Use [Standard Library](stdlib/standard_library.md) for imported modules.
