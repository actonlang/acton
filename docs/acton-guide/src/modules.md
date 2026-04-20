# Modules

Modules let you split a program into smaller named units. In a project,
modules live under `src/`, and subdirectories become part of the module
name. For example, `src/a/b.act` is imported as `import a.b`.

See [Projects](projects.md) for how `acton build` discovers those files
and [Package Management](package_management.md) for external
dependencies that live alongside local modules.

Use modules to group code by responsibility. A good module has a clear
job. For example:

- parsing and validation
- domain logic and shared types
- I/O, network access, or other side effects
- startup and orchestration

<div class="beginner-content">
<p>Start with one module per clear topic. If a file starts mixing
unrelated ideas, move one topic into its own module. Keep the project
tree and the module tree aligned so the file layout stays easy to
follow.</p>
</div>

## Import forms

Use `import` when you want the module name itself:

- `import time`
- `import time as timmy`

Use `from ... import ...` when you want specific names directly:

- `from time import now`
- `from time import now as rightnow`

```python
import time
import time as timmy
from time import now
from time import now as rightnow

actor main(env):
    print(time.now())
    print(timmy.now())
    print(now())
    print(rightnow())

    env.exit(0)
```

<div class="beginner-content">
<p>Keep imports explicit, and use aliases only when they improve
readability or avoid a name clash.</p>
</div>

## Module-level code

Module-level names are constants. Mutable program state belongs in
actors, not in modules.

That means:

- helper functions and constants fit naturally at module level
- mutable variables do not
- program startup logic should live in actors such as `main`

```python
default_timeout = 5

def timeout_seconds():
    return default_timeout
```

<div class="advanced-content">
<p>Modules can also carry docstrings. Because module-level bindings are
constant, importing a module is closer to importing a namespace of
definitions than shared mutable runtime state.</p>

<p>That is why module boundaries work best when they are stable and
purposeful. Prefer names that explain the boundary directly, and keep
the module surface small unless the file is intentionally acting as a
public API.</p>
</div>

## How to split code

Split a module when it starts doing too many different jobs.

Good reasons to make a new module include:

- a group of functions is reused from several places
- a feature has a clear boundary, such as `parser`, `storage`, or
  `protocol`
- one part of the code changes for different reasons than the rest
- the file has grown enough that imports and names are hard to scan

Avoid creating modules just to make a tree look deep. A short, direct
module path is usually easier to work with than a very fine-grained one.
