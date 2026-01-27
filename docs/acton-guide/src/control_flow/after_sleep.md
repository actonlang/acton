# `after` and sleep

In many languages, it is fairly common to use `sleep()` for things like timeouts, pacing and similar. In Acton, and more generally in async actor based languages, sleeping is frowned upon and often not even available.

The idiomatic control pattern in Acton is using `after`, like `after 42.1337: foo()`. This tells the run time system (RTS) to schedule the execution of the `foo()` function after `42.1337` seconds. Meanwhile, other methods on the actor can be invoked.

Source:
```python
import time

"""Pace the sending of messages to once a second
"""

actor Receiver():
    def recv(msg):
        print("At " + str(time.now()) + ", I received a message:", msg)

actor main(env):
    var i = 0
    r = Receiver()

    def send_msg():
        # Send a message, increment i
        r.recv("Hello " + str(i))
        i += 1

        # ... and reschedule execution of ourselves in 1 second
        after 1: send_msg()
        
        # Exit after awhile
        if i > 4:
            env.exit(0)
        
    # Kick off the whole thing
    send_msg()
```

Compile and run:
```sh
acton after_pace.md
```

Since the output includes time, you will naturally get a slightly different result if you run this.

Output:
```sh
At 2023-05-16T10:08:59.135806428+02, I received a message: Hello 0
At 2023-05-16T10:09:00.136484032+02, I received a message: Hello 1
At 2023-05-16T10:09:01.135585727+02, I received a message: Hello 2
At 2023-05-16T10:09:02.135695030+02, I received a message: Hello 3
At 2023-05-16T10:09:03.135811176+02, I received a message: Hello 4
```

There is in fact a `sleep` function in Acton, hidden away in the `acton.rts` module. Do **NOT** use it! It is intended only for debugging of the RTS itself and will probably disappear from the standard library before 1.0. Despite it, we consider the language to not have a sleep.

Actors should either be actively processing or at rest. Conceptually, a sleep is an active wait, in the sense that the RTS will just sit there waiting for the sleep to finish, it is **blocked**, while it really could process something else in between, like run a different actor method continuation. Similarly, the actor itself could have had other methods on it invoked instead of being actively blocked on a sleep. Being blocked is very bad, which is why all I/O is asynchronous in Acton and why there is no `sleep`.

`sleep` is evil, use `after`!
