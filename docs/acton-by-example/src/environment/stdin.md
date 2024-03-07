# Reading stdin input

Read input from stdin by installing a handler for stdin data. The returned data is `str`
```python
actor main(env):
    def interact(input):
        print("Got some input:", input)
        
    env.stdin_install(interact)
```


It is possible to specify the encoding and an on_error() callback which is invoked if there are problem with decoding the data. When encoding is not specified (default `None`), an attempt is made to discover the encoding by reading the `LANG` environment variable. If no encoding is discovered, the default is to use `utf-8`.

```python
actor main(env):
    def interact(input):
        print("Got some input:", input)
        
    def on_stdin_error(err, data):
        print("Some error with decoding the input data:", err)
        print("Raw bytes data:", data)
        
    env.stdin_install(on_stdin=interact, encoding="utf-8", on_error=on_stdin_error)
```

You can read the raw data in `bytes` form by installing a bytes handler instead:

```python
actor main(env):
    def interact(bytes_input):
        # Note how the input might contain parts (some bytes) of a multi-byte
        # Unicode character in which case decoding will fail
        print("Got some input:", bytes_input.decode())
        
    env.stdin_install(on_stdin_bytes=interact)
```

This allows reading binary data and more explicit control over how to decode the data.
