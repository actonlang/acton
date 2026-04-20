# Capabilities to access outside world

Any useful program eventually needs to interact with the outside world.
That can mean reading files, opening sockets, or sending data to a
remote host. In many languages those operations are always available to
any code. In Acton they are explicit.

Things outside the actor world are represented by actors and accessed
through capability references. A capability is a reference that grants
permission for a specific kind of operation. Without the reference, the
operation is not available.

For example, `TCPConnection` needs a `TCPConnectCap` to connect to a
remote host over TCP. The type system enforces that requirement. If the
right capability is not available, the code does not compile.

`TCPConnectCap` sits inside a capability hierarchy that starts at
`WorldCap` and narrows from there:

> WorldCap >> NetCap >> TCPCap >> TCPConnectCap

The root actor, typically `main()`, takes an `Env` reference as its
first argument. `env.cap` is the root `WorldCap` capability for
accessing the outside world.

```python
import net

actor main(env):

    def on_connect(c):
        c.close()

    def on_receive(c, data):
        pass

    def on_error(c, msg):
        print("Client ERR", msg)

    connect_cap = net.TCPConnectCap(net.TCPCap(net.NetCap(env.cap)))
    client = net.TCPConnection(connect_cap, env.argv[1], int(env.argv[2]),
        on_connect, on_receive, on_error)
```

That structure matters because it lets the program choose how much
authority to hand out. A deeply nested helper, or a dependency of a
dependency, can only do what its received capability allows.

## Restrict and delegate

When a function takes a capability argument, it should normally take
the narrowest capability it actually needs. If the code only needs to
open a TCP connection, pass `TCPConnectCap`. Do not pass `WorldCap`
just because it is available.

When you write a helper, ask what the helper really does. Give it only
the capability needed for that work. If a library asks for a wider
capability than the work requires, that is a design problem in the
library.

<div class="advanced-content">
<p>The deeper design point is capability attenuation: code should pass
along narrower powers than it originally received whenever possible.
That keeps authority local, makes APIs easier to audit, and prevents a
convenient helper from quietly becoming a wide ambient escape hatch
into the outside world.</p>
</div>

## Capability-friendly interfaces

Capability-friendly APIs are explicit about their authority boundaries.
If one part of a library logs to files and another part talks to a
remote host, split those responsibilities or make the narrower paths
easy to select.

The goal is not ceremony. The goal is to keep authority local and
visible. A capability that is not passed in cannot be used, and a
capability that is not passed on cannot escape further into the program.
