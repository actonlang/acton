# Modules

Acton modules can be used to hierarchically structure programs by splitting code into smaller logical units (modules).

Import modules by using the `import` keyword. The module will be available with its fully qualified name.

Use `from` .. `import` to import a single function from a module.

Functions and modules can be aliased using the `as` keyword.

```python
import time
import time as timmy
from time import now
from time import now as rightnow

actor main(env):
    time.now()         # using the fully qualified name
    timmy.now()        # using aliased module name
    now()              # using the directly imported function
    rightnow()         # using function alias

    env.exit(0)
```

Remember, all state in an Acton program must be held by actors. There can be no mutable variables in an Acton module, only constants! Similarly, there can be no global instantiation code in a module.
