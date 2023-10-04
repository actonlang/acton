# while

The `while` construct can be used to run a loop **while** a condition is true.

Source:
```python
import random

def throw_dice():
    number = random.randint(1,6)
    print("Dice:", number)
    return number

actor main(env):
    var luck = True

    while luck:
        if throw_dice() == 4:
            # ran out of luck, nobody likes a 4
            luck = False

    env.exit(0)
```

Compile and run:
```sh
actonc while.act
```

Note that the output is random and you could get a different result.

Output:
```sh
Dice: 3
Dice: 1
Dice: 5
Dice: 2
Dice: 4
```
