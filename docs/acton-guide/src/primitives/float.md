# `float`

Source:
```python
from math import pi

actor main(env):
    # round to 2 decimals
    a = round(pi, 2)

    # print 4 digits of pi
    print("%.4f" % pi)

    env.exit(0)
```

Output:
```sh
3.1416
```
