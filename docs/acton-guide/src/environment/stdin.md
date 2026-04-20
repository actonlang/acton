# Reading stdin input

Read from stdin by installing a handler with `env.stdin_install`. The
handler receives data as it arrives. In the common text case, the data
is decoded to `str`.

```python
actor main(env):
    def interact(input):
        print("Got some input:", input)

    env.stdin_install(interact)
```

You can make the text decoding explicit by providing `on_stdin`,
`encoding`, and `on_error`.

When `encoding` is not set, Acton tries to discover the encoding from
`LANG`. If nothing useful is found, it falls back to UTF-8.

```python
actor main(env):
    def interact(input):
        print("Got some input:", input)

    def on_stdin_error(err, data):
        print("Some error with decoding the input data:", err)
        print("Raw bytes data:", data)

    env.stdin_install(on_stdin=interact, encoding="utf-8",
        on_error=on_stdin_error)
```

If the data is binary, or if you want to delay decoding, install a
bytes handler instead.

```python
actor main(env):
    def interact(bytes_input):
        # Decode only when this code is ready to decide how.
        print("Got some input:", bytes_input.decode())

    env.stdin_install(on_stdin_bytes=interact)
```

<div class="advanced-content">
<p>The important distinction is between a decoded text stream and a raw
byte stream. If the input protocol is truly textual, the string path is
usually right. If framing, binary payloads, or uncertain encodings
matter, keep the data as <code>bytes</code> until your own code decides
how to decode it.</p>
</div>

## Common patterns

Use the text callback for ordinary command line input, especially when
the input is line-oriented or clearly textual.

Use the bytes callback when the input may contain binary data, partial
fragments of multibyte characters, or a protocol with its own framing
rules.

Treat decode errors as part of the interface. If the program expects
text, decide what to do when the bytes do not decode cleanly instead of
letting that decision hide inside a helper.
