# Generic Types

Sometimes we want to be able to work with any type. We call such types *generic types*. For example, the builtin *list* type in Acton can store any other type, so we say it is a list of `A`, where `A` is a generic type (from the specification in Acton builtins):

```python
class list[A] (object):
    ...

    def pop(self, n :?int=-1) -> A:
        """Pop and return an item from the list"
```

`[A]` says `A` is a generic type. And for example, the `pop()` function on list returns an element of type `A`.

```python
class list[A] (object):
          ^^^ --- generic types are written here, between []
                  A is a generic type used in `class list`

    def pop(self, n :?int) -> A:
                              ^--- list.pop returns an item of type A
```
