# Capabilities to access outside world

Any interesting program will need to interact with the outside world, like accessing the network or reading files. In C and many other languages, it is possible for any function at any time to simply make calls and access the external world, like read a file (maybe your private SSH key and send it over the network). Acton makes all such access to the outside world explicit through *capability references*.

In an Acton program, having a reference to an actor gives you the ability to do something with that actor. Without a reference, it is impossible to access an actor and it is not possible to forge a reference. This provides a simple and effective security model that also extends to accessing things outside of the Acton system, like files or remote hosts over the network.

Things outside of the actor world are represented by actors and to access such actors, a *capability reference* is required. For example, we can use `TCPConnection` to connect to a remote host over the network using TCP. The first argument is of the type `TCPConnectCap`, which is the *capability* of using a TCP socket to connect to a remote host. This is enforced by the Acton type system. Not having the correct capability reference will lead to a compilation error.

`TCPConnectCap` is part of a capability hierarchy, starting with the generic `WorldCap` and becoming further and further restricted:

> WorldCap >> NetCap >> TCPCap >> TCPConnectCap

The root actor (typically `main()`) takes as its first argument a reference to `Env`, the environment actor. `env.cap` is `WorldCap`, the root capability for accessing the outside world.

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
    client = net.TCPConnection(connect_cap, env.argv[1], int(env.argv[2]), on_connect, on_receive, on_error)
```

Capability based privilege restriction prevent some deeply nested part of a program, perhaps in a dependency to a dependency, to perform operations unknown to the application author. Access to capabilities must be explicitly handed out and a program can only perform operations based on the capabilities it has access to.

## Restrict and delegate

Functions and methods taking a Cap argument normally takes the most restricted or refined capability. In the example with setting up a TCP connection, it is the `TCPConnectCap` capability we need, which is the most restricted.

Rather than handing over `WorldCap` to a function, consider what capabilities that function actually needs and only provide those. If a library asks for wider capabilities than it needs, do not use it.


## Capability friendly interfaces

As a library author, you should only require precisely the capabilities that the library requires. Do not be lazy and require `WorldCap`. If the library offers multiple functionalities, for example logging to files or to a remote host, strive to make parts optional such that it the application developer and choose to only use a subset and only provide the capability required for that subset.
