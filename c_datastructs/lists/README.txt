Extensible arrrays, with operations corresponding to Python lists.
For now, extension doubles capacity and capacity never shrinks.
Polymorphism in the sense that elements have type WORD, i.e. *void.

Functions returning a list element (pop, getitem and next) return 
NULL to signal exceptional situation, which means that using e.g. 
unboxed ints as elements does not work well (elements can't be 0...)
To handle that, versions that return results in a by-reference
parameter are provided.

