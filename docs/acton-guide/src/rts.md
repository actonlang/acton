# Run Time System

The Acton Run Time System is what sets up the environment in which an Acton program runs. It performs bootstrapping of the root actor. The worker threads that carry out actual execution of actor continuations are part of the RTS. It is the RTS that handles scheduling of actors and the timer queue. All I/O is handled between modules in the standard library in conjunction with the RTS.

# Arguments

It is possible to configure the RTS through a number of arguments. All arguments to the RTS start with `--rts-`. Use `--rts-help` to see a list of all arguments:

```sh
$ acton examples/helloworld.act
Building file examples/helloworld.act
  Compiling helloworld.act for release
   Finished compilation in   0.012 s
  Final compilation step
   Finished final compilation step in   0.198 s
$ examples/helloworld --rts-help
The Acton RTS reads and consumes the following options and arguments. All
other parameters are passed verbatim to the Acton application. Option
arguments can be passed either with --rts-option=ARG or --rts-option ARG

  --rts-debug                       RTS debug, requires program to be compiled with --optimize Debug
  --rts-ddb-host=HOST               DDB hostname
  --rts-ddb-port=PORT               DDB port [32000]
  --rts-ddb-replication=FACTOR      DDB replication factor [3]
  --rts-node-id=ID                  RTS node ID
  --rts-rack-id=RACK                RTS rack ID
  --rts-dc-id=DATACENTER            RTS datacenter ID
  --rts-host=RTSHOST                RTS hostname
  --rts-help                        Show this help
  --rts-mon-log-path=PATH           Path to RTS mon stats log
  --rts-mon-log-period=PERIOD       Periodicity of writing RTS mon stats log entry
  --rts-mon-on-exit                 Print RTS mon stats to stdout on exit
  --rts-mon-socket-path=PATH        Path to unix socket to expose RTS mon stats
  --rts-no-bt                       Disable automatic backtrace
  --rts-log-path=PATH               Path to RTS log
  --rts-log-stderr                  Log to stderr in addition to log file
  --rts-verbose                     Enable verbose RTS output
  --rts-wthreads=COUNT              Number of worker threads [#CPU cores]

$
```

# Worker threads

Per default, the RTS starts as many worker threads as there are CPU threads available, although at least 4. This is optimized for server style workloads where it is presumed that the Acton program is the sole program consuming considerable resources. When there are 4 and more CPU threads available, the worker threads are pinned to each respective CPU thread.

It is possible to specify the number of worker threads with `--rts-wthreads=COUNT`.

Actor method continuations run to completion, which is why it is wise not to set this value too low. Per default a minimum of 4 threads are started even when there are fewer CPU threads available, which means the operating system will switch between the threads inducing context switching overhead.
