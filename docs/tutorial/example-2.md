# Example 2 - Prime numbers

Here is another function definition, which is valid both in Python and
Acton:

```py
def primesTo(n):
    isPrime = [True] * n
    isPrime[0] = False; isPrime[1] = False
    for i in range(2,int(math.sqrt(float(n)))+1):
        if isPrime[i]:
            for k in range(i*i,n,i): isPrime[k] = False
    return [i for i in range(2,n) if isPrime[i]]
```

This function computes, for given ***n***, the list of all prime numbers
smaller than ***n***, so `primesTo(20)` is
`[2, 3, 5, 7, 11, 13, 17, 19]`. The function uses the
ancient and remarkably efficient sieve algorithm discovered by the Greek
mathematician Erathostenes around 200 BC. On a typical laptop this
function will compute the 78498 primes less than a million in a small
fraction of a second.

But again we are more interested in the data structures used and their
types. In the function body, `isPrime` is bound to a list of $n$
Boolean values, i.e. the elements of the list are `True` or
`False`. Like all lists in Python, `isPrime` will get the builtin type
`list`. Also the function result is a `list`, but this time a list
of integers. The type, however, cannot express this difference; 
this is also just a `list`. We have here an example of
***polymorphism***, i.e. a type of container which may contain elements
of different types. In Python this polymorphism is unrestricted; even a
single list can contain elements of many different types. As an example,
we can form the list `[3,"hi",lambda n: n*n]`, consisting
of one integer, one string and one function.

Examples where this freedom is essential, or even useful, are relatively
scarce. On the other hand, it is easy to see that it may cause trouble:
when we traverse a list we do not know the types of the values we
encounter and hence not which operations we may meaningfully perform on
them. Acton does not allow full polymorphism, but adopts a more limited
form which is both type-safe and has proven very useful
in practice: ***parametric*** polymorphism. This means that we can have
lists of integers, lists of strings, lists of lists of floats etc. but
that in a given list, all elements have the same type. Thus we have not
one all-encompassing type of lists, but a whole family of types
`list[A]` where `A` is a ***type variable***, which can be
***instantiated*** to an arbitrary type. So `isPrime` has type
`list[bool]` and the function `primesTo` has type
`(int) -> list[int]`. The function `primesTo`
itself may be inserted as an element in a list of type
`list[(int) -> list[int]]`. Type safety means that
whenever we encounter an element of such a list, we know that we can
apply it to an integer and get a list of integers as result.

To complete the example, here is an actor which accepts ***n*** on the command
line and prints not all the primes but only the number of primes smaller than ***n***:

```py
actor main(env):
   print(len(primesTo(int(env.argv[1]))))
```

To find the number of primes, we use the function `len`, which
in Acton as in Python computes the length of its argument. So, it
would seem that `len` har type `(list[A]) -> int`,
i.e. it works independently of the type of elements of the list. This
is indeed so, but it doesn't stop there. However, also in this case
you may want to come back to the rest of this discussion in a future reading.

You may recall that in
Python, the function `len` can be applied not only to lists,
but also to strings, sets and dictionaries (and other classes, which
define the method `__len__`). This is also the case in Acton,
and requires a new concept to ensure safe static typing.

We need a general mechanism for
***overloading***, i.e. allowing several functions, with separate
definitions, to be denoted by the same name. In fact, we have already
seen overloading in previous examples:
we have used + to denote addition both of `int`s and
`float`s, quite different operations at the machine level. Many
languages overload artihmetic operator symbols in ad hoc ways.
What we look for is a general method to introduce new
overloadings. 

The general type of `len` in Acton is `(Collection[A]) -> int`,
where `Collection` is a builtin ***protocol***. A protocol
is a collection of ***type signatures***. In order to
***implement*** the protocol a type must implement methods of the
required signatures. The protocol `Collection` specifies two
methods, `__len__` and `__fromiter__`. So, all the
builtin types `list[A]`, `set[A]`, `dict[A, B]`
and `str` implement this protocol and thus we can compute lengths of
objects of these types. The type system, remarkably, allows overloading to be resolved
at compile time, i.e. the compiled code invokes the correct instance
of all overloaded functions at each occurrence.

We cannot here discuss protocols in depth. We just note that
the use of the word 'protocol' for this concept is taken from Swift. 
The concept is also closely related to the notion of ***type classes*** in
Haskell.
