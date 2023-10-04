# Actor concurrency

Multiple actors run concurrently. In this example we can see how the two actors Foo and Bar run concurrently. The main actor is also running concurrently, although it doesn't do anything beyond creating the Foo and Bar actors and exiting after some time.

Source:
```python
actor Counter(name):
    var counter = 0

    def periodic():
        print("I am " + name + " and I have counted to " + str(counter))
        counter += 1

        # 'after 1' tells the run time system to schedule the specified
        # function, in this case periodic(), i.e. ourselves, after 1 second
        after 1: periodic()

    # First invocation of periodic()
    periodic()


actor main(env):
    # Create two instances of the Counter actor, each with a unique name
    foo = Counter("Foo")
    bar = Counter("Bar")
    
    def exit():
        env.exit(0)

    # exit the whole program after 10 seconds
    after 10: exit()
```

Compile and run:
```sh
actonc concurrency.act
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
