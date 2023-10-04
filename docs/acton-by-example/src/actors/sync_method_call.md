# Sync Method calls

While async is good for performance it makes it somewhat convoluted, forcing use of callbacks, to just return a value. Acton makes it possible to call other actors in a synchronous fashion for ease of use.

A method is called **synchronously** when the return value is used.

Source:
```python
import acton.rts

actor DeepT():
    def compute():
        # some heavy computation going on
        acton.rts.sleep(1)
        return 42

actor main(env):
    d1 = DeepT()

    answer = d1.compute()
    print("The answer is", answer)

    env.exit(0)
```

Compile and run:
```sh
actonc sync.act
```

Output:
```sh
The answer is 42
```

The call flow can be illustrated like this. We can see how the execution of `main` is suspended while it is waiting for the return value from actor `d1`.
```bob
   main    *-------*            *-------*
                    \          ^
                     v        /
   d1                 *------'
```


While synchronous is *bad* because we block waiting for someone else, we are only ever going to wait for another actor to run its method. There is never any wait for I/O or other indefinite waiting, only blocking wait for computation within the Acton system. This is achieved by the lack of blocking calls for I/O, thus even if there is a chain of actors waiting for each other
