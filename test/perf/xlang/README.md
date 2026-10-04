# Acton, Go and Tokio

Go and Tokio ports of the tests in `../src/scheduling.act`. `xlang`
(`../src/xlang.act`), a program in the perf project, builds all three, runs
them interleaved on one machine and prints tables of the results.

## Tests

| test | one operation | what it shows |
|---|---|---|
| `ring` | a hop | one token around a ring of 1,000 actors: the cost of one handoff, with nothing in parallel |
| `ring_tokens` | a hop | 64 tokens on that ring. The tokens catch up with each other and travel in groups, so this shows little parallelism in any runtime |
| `pairs` | a hop | 64 pairs passing their own tokens: how handoffs scale with threads |
| `hot_server` | a call and its reply | 64 clients calling one server: requests into one busy actor |
| `fan_out` | a message and 20 µs of work | work items round robin to 64 workers: how CPU-bound work spreads over threads |
| `pipeline` | a message through 8 stages | bursts through a chain of stages: throughput with deep queues |
| `latency_under_load` | a round trip | round trips through an idle actor while busy ones keep every thread occupied: how long a woken actor waits |

The ports keep the sizes and the structure of the Acton tests. An actor is a
goroutine or a task; a mailbox is a channel, buffered in Go so that no send
blocks, and an unbounded mpsc channel in Tokio. A synchronous call sends a
request with a reply channel of its own (a oneshot in Tokio), as an Acton call
creates a future of its own. Busy work is the same integer loop reading the
clock every 128 passes. Tokio runs its multi-thread runtime without the I/O
driver; only `latency_under_load` enables the time driver.

Each run measures as `acton test perf` does: a warmup of a tenth of the time
budget, then four samples, each giving wall time and the CPU time of the whole
process per body of operations. `xlang summary` shows medians over runs.

## Running

From `test/perf`, with Go 1.22 or later and Rust 1.71 or later:

```sh
../../dist/bin/acton build --release
out/bin/xlang build
out/bin/xlang run results.jsonl --reps 3 --threads "4 8 14" --label "main abc1234"
out/bin/xlang summary results.jsonl
out/bin/xlang page results.jsonl other-machine.jsonl --output xlang.html
```

`xlang build` builds Acton's scheduling test binary in release mode for this
machine's CPU (`acton test perf --cpu native`), the Go port with `go build` and
the Tokio port with `cargo build --release` and `-C target-cpu=native`; Tokio
comes from crates.io as pinned in `tokio/Cargo.lock`. `xlang run` refuses an
Acton test binary built in Debug mode, which plain `acton test` makes in the
same place.

`xlang page` writes a web page with charts and tables, one section per result
file, so give it one file per machine. Its style and script are in `page/`.

`xlang run` appends one JSON line per run, and per repeat one line with the
machine, its load and busiest processes, and on Linux the CPU frequency
governor. On even repeats the runtimes run in reverse order. Run it on a quiet
machine, and compare runtimes only within one machine. On Linux,
`--counters perf` also counts cycles for Go and Tokio under `perf stat`, enabled
only for the measured samples; Acton's harness counts them itself.

When `../src/scheduling.act` changes, `xlang run` warns: check that the ports
still match, then update `SCHEDULING_MD5` in `../src/xlang.act`.

## Upstream benchmarks

Go's and Tokio's own scheduler benchmarks are useful to check a machine and a
toolchain, but they count other operations: `go test -run=NONE
-bench='ChanSync|PingPongHog' runtime`, and `ping_pong` and `chained_spawn` in
Tokio's `benches/rt_multi_threaded.rs`.
