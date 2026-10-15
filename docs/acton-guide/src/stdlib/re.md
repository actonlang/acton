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

## Compiling a pattern once

`re.match` compiles its pattern on every call. To match the same pattern many
times, compile it once with `re.compile` and use the methods of the `Pattern`
it returns. `match`, `matches` and `split` take the same arguments as the
module functions, without the pattern.

Source:
```python
import re

actor main(env):
    ipv4 = re.compile(r"^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})$")
    for address in ["192.168.1.1", "fe80::1", "10.0.0.255"]:
        m = ipv4.match(address)
        if m is not None:
            print(address, "has last octet", m.group[4])
        else:
            print(address, "is not an IPv4 address")

    env.exit(0)
```

A `Pattern` does not change after it is created, so actors can share one.

Output:
```sh
192.168.1.1 has last octet 1
fe80::1 is not an IPv4 address
10.0.0.255 has last octet 255
```
