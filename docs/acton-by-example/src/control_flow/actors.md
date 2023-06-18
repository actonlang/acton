# Control flow in an async actor world

The basic control flow of most programming languages involve a starting point, like a main function, which is run from top to bottom, after which the program implicitly terminates. The basic objective is to feed instructions to the CPU and this goal remains through increasing levels of abstractions. Acton is different. Once created, an actor will simply remain indefinitely, waiting for incoming messages in the form of actor method calls. See [Actor Lifetime](/actors/lifetime.md).

## A mental model of actors

Actors in an Acton program form a vast web of interconnected actors. Some actors are on the edge of the Acton realm, bordering to the external world where they may be doing I/O with external entities, through files, sockets or other means. All I/O is callback based and thus event driven and reactive. When there is an event, an actors reacts, perhaps initiating calls to other actors. A ripple runs across the web of actors, each reacting to incoming messages and acting accordingly.
