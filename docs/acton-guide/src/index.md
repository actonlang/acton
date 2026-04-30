```bob

                                        A Guide to


                  .---.                        .-.
                 /     \                       | |
                /   _   \          .----.   .--' '--.      .----.     .-..-----.
               /   / \   \        / .--._\  '--. .--'     / .--. \    | / .---. \
              /   '---'   \      / /           | |       / /    \ \   |  /    | |
             /             \    ( (            | |      ( (      ) )  | |     | |
            /   .-------.   \    \ \    __     | |  __   \ \    / /   | |     | |
           /   /         \   \    \ '--' /     \ '-' /    \ '--' /    | |     | |
          '---'           `---'    '----'       '---'      '----'     '-'     '-'
     


       -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

```

Acton is a general-purpose programming language with a sharp focus on building
robust reactive systems from many cooperating actors. It is especially well
suited to distributed scale-out logic, where safety comes from combining
actor-local state, explicit communication, and a static type system that checks
the shape of the program before it runs.

Actors are Acton's unit of state, concurrency, and communication: each actor
runs sequentially, owns its state, and interacts with other actors through
method calls, timers, callbacks, and async I/O. The actor model keeps mutation
local and control flow direct while allowing many independent pieces of work to
run concurrently.

The language keeps familiar syntax while adding functional and object-oriented
programming, type inference, capability-based security, and native compilation.
It is practical for everyday scripts and services, but its center of gravity is
actor-heavy systems where correctness, explicit authority, and efficient
compiled code matter.
