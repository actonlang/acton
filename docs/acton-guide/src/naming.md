# Naming

Acton has naming rules and conventions. Some are enforced by the
compiler, and some are conventions that keep larger programs readable.

## Functions

Use lower-case words with `_` between them.

- `parse_user`
- `load_config`
- `send_report`

Function names should describe what the function does.

## Actors and classes

Use `PascalCase` with two or more alphanumeric characters.

- `HttpServer`
- `OrderBook`
- `FileCache`

Do not use a single upper-case letter for a class or actor name. Those
names are reserved for type variables.

## Type variables

Type variables use a single upper-case letter, optionally followed by
digits.

- `A`
- `T1`

Use them for generic code, not for ordinary domain concepts. If a name
describes a real thing in the program, make it a normal type or actor
name instead.

## Modules and files

Module names come from file paths, so filename choice matters. Use
short, lower-case names and let the import path reflect the structure of
`src/`. For example, `src/a/b.act` is imported as `import a.b`.

Naming is part of API design. In Acton, a module name, an imported
symbol, and a type name often appear together, so keeping them short and
predictable reduces noise in the code. This matters even more once a
project has several modules and cross-module dependencies.

## Private names

A leading `_` is the usual marker for implementation details that
should not be treated as part of the public surface. Use it for names
that are only meant to be used inside one module or actor.

## Practical guidance

<div class="beginner-content">
<p>Good names make code easier to split into modules, and they make
imports easier to read. If a name feels awkward at the call site, it is
usually worth changing before the code grows.</p>
</div>

- Prefer names that describe what a thing is or does.
- Use the same word for the same concept across modules.
- Avoid abbreviations unless they are standard in your domain.
- Make helper names specific enough that call sites read naturally.
