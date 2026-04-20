# re

The `re` module provides regular expression matching.

Source:
```python
import re

actor main(env):
    m = re.match(r"(foo[a-z]+)", "bla bla foobar abc123")
    if m is not None:
        print("Got a match:", m.group[1])

    env.exit(0)
```

<div class="beginner-content">
<p>Import the module with <code>import re</code>, then call functions
such as <code>re.match(...)</code>.</p>
</div>

`re.match` also accepts an optional `start_pos` to begin scanning at a
specific index (defaults to 0).

Output:
```sh
Got a match: foobar
```
