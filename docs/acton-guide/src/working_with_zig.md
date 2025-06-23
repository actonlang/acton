# Working with Zig / C / C++

Acton has C ABI compatibility which makes it trivial to call C functions and fairly simply to call Zig and C++ using C wrapping functions. If you want to integrate a library written in one of these languages, this page is for you.

Regardless of the foreign language used, we need to consider a few things:
- memory allocation, it must play well with the Acton GC
  - You can allocate memory via classic `malloc` or via Acton GC malloc
    - `acton_malloc` - normal malloc
    - `acton_malloc_atomic` for allocations that are guaranteed to not contain pointers
  - Better safe than sorry, use the GC-malloc when in doubt
  - Always use Acton GC malloc for actor and class attributes and similar
    - object and actor instances are garbage collected by the GC and there is no destructor function, so if you would have used class `malloc` there is no good place for the `free`
  - Within pure functions, you can use class `malloc` - be sure to `free` the allocations before return of the function, even for error paths
- thread safety, the Acton RTS is threaded and actors are concurrently executed by different threads
  - in general, we strive to only keep data per actor and since an actor executes sequentially, we do not need thread safety measures like locks - just make sure you don't try to share data between actors "under the hood"
  - libraries must not use global variables though
- asynchronous I/O, the Acton RTS performs asynchronous I/O and any other library that performs I/O need to conform to this model
