# Actors are sequential

While many actors can run concurrently, each actor is a sequential process and will only do one thing at a time. There are no threads nor POSIX processes in Acton, only actors. In order to scale beyond a single

In order to write Writing performant Acton programs This means an actor can only do one thing at a time and consequently, it is important not to block. All I/O in Acton is asynchronous.

