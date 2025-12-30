# Regular expressions

Source:
```python
import re

actor main(env):
    m = re.match(r"(foo[a-z]+)", "bla bla foobar abc123")
    if m is not None:
        print("Got a match:", m.group[1])

    env.exit(0)
```

`re.match` also accepts an optional `start_pos` to begin scanning at a
specific index (defaults to 0).

Output:
```sh
Got a match: foobar
```
