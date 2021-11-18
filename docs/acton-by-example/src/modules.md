# Modules

Acton modules can be used to hierarchically structure programs by splitting code into smaller logical units (modules).

Import modules by using the `import` keyword. The module will be available with its fully qualified name.

Use `from` .. `import` to import a single function from a module.

Functions and modules can be aliased using the `as` keyword.

```python
import acton.rts
import acton.rts as arts
from acton.rts import sleep
from acton.rts import sleep as sleepy

actor main(env):
    acton.rts.sleep(0)   # using the fully qualified name
    arts.sleep(0)        # using aliased module name
    sleep(0)             # using the directly imported function
    sleepy(0)            # using function alias

    await async env.exit(0)
```

Remember, all state in an Acton program must be held by actors. There can be no mutable variables in an Acton module, only constants!
