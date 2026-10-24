# std.syslog

The `std.syslog` module provides a UDP client and server, an
[RFC 5424](https://www.rfc-editor.org/rfc/rfc5424) message codec, and a decoder
for canonical [RFC 3164](https://www.rfc-editor.org/rfc/rfc3164) messages.

The transport sends one message per datagram using
[RFC 5426](https://www.rfc-editor.org/rfc/rfc5426). This implementation supports
UDP; TCP and TLS transports are not implemented. Pass the destination or listen
port explicitly. The standard syslog UDP port is 514. The examples use loopback
port 5514, which can be opened without privileged-port permissions.

## Sending messages

```python
import net
import std.syslog as syslog

actor main(env):
    def finish(client: syslog.Client):
        client.close()
        env.exit(0)

    def connected(client: syslog.Client):
        client.log("Hello from Acton", severity=syslog.INFO,
                   facility=syslog.LOCAL0, hostname="localhost",
                   app_name="syslog-client",
                   structured_data={"event@32473": [("instance", "one")]})
        after 0.2: finish(client)

    def failed(client: syslog.Client, error: str):
        print("Syslog client error:", error)
        client.close()
        env.exit(1)

    cap = net.UDPConnectCap(net.UDPCap(net.NetCap(env.cap)))
    syslog.Client(cap, "127.0.0.1", 5514, connected, failed)
```

`Client(cap, address, port, on_connect, on_error, max_size=2048,
max_pending=64)` resolves the destination and opens the UDP socket
asynchronously. `on_connect(client)` means the local socket is ready to send;
it does not confirm that a server exists at the destination. `send(message)`
queues messages before that callback, up to `max_pending` entries. Invalid
messages, messages exceeding `max_size`, and a full startup queue are reported
through `on_error(client, error)`.

`log(text, severity=INFO, facility=USER, hostname=None, app_name=None,
procid=None, msgid=None, structured_data={})` supplies the current UTC timestamp
and encodes text as UTF-8 with a BOM. Use `send(Message(...))` to choose the
timestamp and body bytes yourself.

UDP has no delivery acknowledgement. Messages can be lost, duplicated, or
reordered. The client does not retry. `close()` discards queued messages and
does not wait for delivery. The short delay in this example lets the
asynchronous send run before the program exits; it is not a delivery guarantee.
In a long-running application, keep the client open for subsequent messages.

## Receiving messages

```python
import net
import std.syslog as syslog

actor main(env):
    def bound(server: syslog.Server, error: ?str):
        if error is not None:
            print("Syslog server could not listen:", error)
            env.exit(1)
        else:
            print("Listening on", server.local_address(), server.local_port())

    def received(server: syslog.Server, message: syslog.Message,
                 peer: str, port: int):
        print("{peer}:{port} facility={message.facility} severity={message.severity}")
        print("timestamp:", message.timestamp, "hostname:", message.hostname,
              "app:", message.app_name, "message:", repr(message.message))

    def failed(server: syslog.Server, error: str):
        print("Syslog server error:", error)

    cap = net.UDPListenCap(net.UDPCap(net.NetCap(env.cap)))
    syslog.Server(cap, "127.0.0.1", 5514, bound, received, failed)
```

`Server(cap, address, port, on_listen, on_message, on_error,
max_size=65507, legacy=True)` calls `on_listen(server, error)` when binding
finishes, with `None` on success. `on_message(server, message, peer, port)`
provides the parsed message and the source address and port. Malformed or
oversized datagrams are discarded and reported to `on_error(server, error)`;
the listener remains available for subsequent datagrams. Set `legacy=False`
to accept only RFC 5424 messages.

The server runs until closed or the program exits. `close()` is safe to call
repeatedly. Port 0 requests an ephemeral port; query `local_port()` after the
successful `on_listen` callback to find the selected port. `local_address()`
returns the bound address. Both return an empty value before the listener is
ready or after it is closed.

Both actors accept `max_size` from 480 through 65507 bytes. These limits apply
to the entire encoded message. Choose a limit that suits the receiving system
and network; larger UDP datagrams may require IP fragmentation.

## Message codec

`Message(message=None, facility=USER, severity=INFO, timestamp=None,
hostname=None, app_name=None, procid=None, msgid=None, structured_data={},
version=1)` stores a message independently of the network. The constructor
stores fields without validation; `encode(message) -> bytes` validates them
and produces RFC 5424 version 1. `decode(data) -> Message` parses that format.
Malformed input or invalid fields raise `ValueError`. These functions have no
transport size limit.

Missing header values use `None`, which maps to the protocol's `-` marker.
`timestamp()` returns the current UTC time with six fractional digits.
The codec validates calendar dates and RFC 5424 timestamps, including explicit
UTC offsets. Leap seconds and more than six fractional digits are rejected.

The `message` field is bytes. `None` omits MSG; `b""` represents an explicitly
empty MSG. Arbitrary bytes are preserved. A body beginning with the UTF-8 BOM
`b"\xef\xbb\xbf"` must contain valid UTF-8; decoding preserves the BOM. This is
why the server example prints `repr(message.message)` instead of assuming every
body can be decoded as text.

`structured_data` is a `dict[str, list[(name: str, value: str)]]`. Each key is
an SD-ID and its value is an ordered list of parameter pairs. Parameters accept
both positional values such as `("instance", "one")` and named values such as
`(name="instance", value="one")`. Read them using `param.name` and `param.value`.

The codec escapes quotes, backslashes, and closing brackets in values. It
preserves dictionary insertion order and parameter order, including repeated
parameter names. The dictionary makes SD-IDs unique; decoding rejects duplicate
SD-IDs in wire data. Use an enterprise-qualified ID for private structured data,
for example:

```python
event = syslog.Message(
    b"service started",
    facility=syslog.LOCAL0,
    severity=syslog.NOTICE,
    timestamp=syslog.timestamp(),
    hostname="host.example.com",
    app_name="service",
    structured_data={"event@32473": [("instance", "one")]}
)
wire = syslog.encode(event)
parsed = syslog.decode(wire)
param = parsed.structured_data["event@32473"][0]
print("{param.name}={param.value}")
```

Facility numbers range from 0 through 23. Constants include `KERN`,
`USER`, `MAIL`, `DAEMON`, `AUTH`, `SYSLOG`, and `LOCAL0` through `LOCAL7`.
The wire priority is `facility * 8 + severity`. Severity constants use syslog's
conventional numbering:

| Constant | Value |
| --- | --- |
| `EMERGENCY` | 0 |
| `ALERT` | 1 |
| `CRITICAL` | 2 |
| `ERROR` | 3 |
| `WARNING` | 4 |
| `NOTICE` | 5 |
| `INFO` | 6 |
| `DEBUG` | 7 |

Use these constants from `syslog`. The separate `logging` module has different
level values, so `logging.INFO` is not a syslog severity.

## Legacy messages

`decode_legacy(data) -> Message` accepts canonical RFC 3164 packets with the
shape `<PRI>Mmm dd hh:mm:ss HOST MSG`. A single-digit day uses a leading space.
The resulting message has `version=0`; `timestamp` retains the original text
without inventing a year or timezone. `message` retains the entire MSG,
including its TAG and any process ID. The remaining RFC 5424 fields are absent.
The decoder rejects missing headers and does not guess vendor-specific forms.

`encode()` and `Client.send()` reject version 0 messages. To forward a legacy
event as RFC 5424, construct a new version 1 `Message` and explicitly choose
how to supply its timestamp and other fields.
