# Async Method calls

As actors are sequential programs and can only do one thing at a time, it is important not to spend time waiting in a blocking fashion. Acton leverages asynchronous style programming to allow actors to react and run only when necessary. Async is at the core of Acton!

A method is called asynchronously when the return value is not used.

Source:
```python

def nsieve(n: int):
    """Sieve of Erathostenes to find primes up to n
    """
    count = 0
    flags = [True] * n
    for i in range(2, n, 1):
        if flags[i]:
            count += 1
            for j in range(i, n, i):
                flags[j] = False
    return count

actor Simon(idx):
    def say(msg, n):
        # Simon likes to compute primes and will tell you how many there are under a given number
        count = nsieve(n)
        print("Simon%d says: %s.... oh and there are %d primes under %d" % (idx, msg, count, n))

actor main(env):
    s1 = Simon(1)
    s2 = Simon(2)

    s1.say("foo", 1000000)
    s2.say("bar", 5)

    def exit():
        env.exit(0)
    after 0.2: exit()
```

Compile and run:
```sh
actonc async.act
```

Output:
```sh
Simon2 says: bar.... oh and there are 2 primes under 5
Simon1 says: foo.... oh and there are 78498 primes under 1000000
```

A method call like `s1.say("foo", 100000)` does not use the return value of and is thus called asynchronously. We ask `s1` to compute primes under 1000000 while `s2` only gets to compute primes up to `5` which will invariably run faster. Thus, `s2` despite being called after `s1`, will print out its result before `s1`. The `s1` and `s2` actors are called asynchronously and are executed concurrently and in parallel.

The call flow can be illustrated like this. We can see how `main` asynchronously calls `s1` and `s2` that will be scheduled to run concurrently. The run time system (RTS) will run `s1.say()` and `s2.say()` in parallel if there are 2 worker threads available. Per default, there are as many worker threads as available CPU threads.
```bob
   main    *-------*---*                      *------*
                    \   \                    /
                     \   v                  /
   s2                 \   *--*             /
                       v                  /
   s1                   *--------*       /
                                        /
   after 2                             *
```
In addition we see how the call to `after 2` schedules the `main` actor to run again after 2 seconds, specifically it will run the `main.exit()` method, which in turn exists the whole program.
