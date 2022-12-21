# Async Method calls

As actors are sequential programs and can only do one thing at a time, it is important not to spend time waiting in a blocking fashion. Acton leverages asynchronous style programming to allow actors to react and run only when necessary. Async is at the core of Acton!

A method is called asynchronously when the return value is not used.

Source:
```python
import acton.rts

actor Simon(idx):
    def say(msg):
        # Simon is sleepy and will take a second before saying anything
        acton.rts.sleep(1)
        print(msg)

actor main(env):
    s1 = Simon(1)

    s1.say("foo")
    print("Simon says...")

    def exit():
        await async env.exit(0)
    after 2: exit()
```

Compile and run:
```sh
actonc async.act
```

Output:
```sh
foo
Simon says...
```

A method call like `s1.say("foo")` does not use the return value of and thus runs asynchronously. We can see how `print("Simon says...")` is run before the `s1.say()` method has finished sleeping and prints its message. This is possible because `s1.say()` was called asynchronously and the execution of `main` continued meanwhile.
