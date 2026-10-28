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
it returns. `match`, `matches`, `count`, `replace` and `split` take the same
arguments as the module functions, without the pattern.

On supported platforms, compiled patterns use PCRE2's JIT compiler. Patterns
with explicit resource limits use the interpreter to preserve those limits.
The interpreter also handles patterns that JIT cannot compile, environments
that cannot allocate executable memory, and matches that exceed the JIT stack.

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

## Counting and replacing matches

Use `re.count(pattern, string)` to count non-overlapping matches. It returns
the same count as `len(re.matches(pattern, string))`.

`re.replace(pattern, string, replacement)` replaces each match with the
literal `replacement` string. Backslashes and dollar signs in the replacement
are ordinary characters; they do not refer to capture groups. Both functions
also work with compiled patterns as `pattern.count(string)` and
`pattern.replace(string, replacement)`.

Source:
```python
import re

actor main(env):
    numbers = re.compile(r"[0-9]+")
    text = "Room 12, floor 3"
    print(numbers.count(text))
    print(numbers.replace(text, "?"))
    print(re.replace("é", "café", "e"))
    env.exit(0)
```

Output:
```sh
2
Room ?, floor ?
cafe
```

`matches`, `count` and `replace` scan from left to right. After an empty match,
the next search starts one Unicode character later, so an empty match is
processed only once at each position. Scanning stops once it reaches the end
of the string. They perform no search on an empty string, so its count is zero
and replacement leaves it unchanged. For example,
`re.replace("", "éa", "-")` returns `"-é-a"`. A search that finds an empty
match at the end still includes it, so `re.replace("$", "éa", "!")` returns
`"éa!"`; a preceding match that consumes the end leaves no further search.
