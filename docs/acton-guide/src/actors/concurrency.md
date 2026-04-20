# Actor concurrency

Multiple actors can make progress concurrently. In this example, Foo
and Bar both keep ticking while the root actor schedules shutdown.

<div class="advanced-content">
<p>Concurrency here means the actors can make progress independently,
not that their outputs follow a fixed interleaving. The runtime may run
them in parallel on multiple workers, but the semantic guarantee is
still per-actor sequential execution rather than any global ordering
between actors.</p>
</div>

Source:
```python
actor Counter(name):
    var counter = 0

    def periodic():
        print("I am " + name + " and I have counted to " + str(counter))
        counter += 1

        after 1: periodic()

    periodic()


actor main(env):
    foo = Counter("Foo")
    bar = Counter("Bar")

    def exit():
        env.exit(0)

    after 10: exit()
```

Compile and run:
```sh
acton concurrency.act
./concurrency
```

Output:
```sh
I am Foo and I have counted to 0
I am Bar and I have counted to 0
I am Foo and I have counted to 1
I am Bar and I have counted to 1
I am Foo and I have counted to 2
I am Bar and I have counted to 2
I am Foo and I have counted to 3
I am Bar and I have counted to 3
I am Foo and I have counted to 4
I am Bar and I have counted to 4
I am Bar and I have counted to 5
I am Foo and I have counted to 5
I am Bar and I have counted to 6
I am Foo and I have counted to 6
I am Bar and I have counted to 7
I am Foo and I have counted to 7
I am Foo and I have counted to 8
I am Bar and I have counted to 8
I am Foo and I have counted to 9
I am Bar and I have counted to 9
```
