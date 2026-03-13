# Stress testing

Stress testing is meant for concurrency bugs, especially race conditions in FFI / C integrations.

Run it with:

```sh
acton test stress
```

## How stress mode runs

- Runs one test function/actor at a time.
- For that test, starts multiple concurrent workers of the same test in one process.
- Worker count defaults to roughly `1.5 * nr_wthreads` so workers must share RTS worker threads.
- Override it with `--stress-workers N` when you want a specific level of oversubscription.
- Stress runs are always fresh (no test-result cache reuse).
- By default, stress runs for up to 5 seconds per test (`--max-time 5000`).
- Continuous mode is available with `--max-time 0`.
- In continuous mode, `--max-iter` is unbounded unless you set it.
- The default stress `--min-time` is 1 second unless overridden (used for calibration; stress run length is controlled by `--max-time`).

## Worker scheduling

Stress workers are split into:

- A no-drift cohort (at least 2 workers, scaling to roughly 25% of workers)
- A staggered-drift cohort (small per-iteration microsecond offsets)

This combines synchronized overlap windows with evolving offsets over time.

## Per-worker live stats

In an interactive terminal, stress mode shows one live status line per worker.

- sync worker line: `wN sync RUN/DONE ... @ RATE/s cur=0us tot=0us`
- drift worker line: `wN drift RUN/DONE ... @ RATE/s cur=DRIFTus tot=TOTALus`

The main test line also carries a compact worker summary:

- total: `... RUNS runs in DURATIONms @ RATE/s`
- worker mix: `workers=N (sync=S drift=D)`
- iteration estimate, granularity and observed coverage: `iter~...ms coarse~...ms sweep=... calib=... cov=SEEN/TOTAL(PCTpct)`
  - `iter~` is a moving estimate of one test iteration duration
  - `coarse~` is the current phase coarseness (lower is finer)
  - in continuous mode (`--max-time 0`), `coarse~` decreases stepwise over time as sweep depth increases
  - `cov` is observed occupied phase bins at current coarseness (resets when coarseness is refined)

Example:

```txt
racy_sum: RUN : 2400 runs in 1500.000ms @ 1600.0/s | workers=8 (sync=2 drift=6) iter~2.340ms coarse~0.037ms sweep=64 cov=37/64(57.8pct)
```

Use normal test flags to tune duration/iterations, for example:

```sh
acton test stress --max-time 30000 --min-iter 100
```

Pin a higher worker count explicitly:

```sh
acton test stress --stress-workers 24 --max-time 30000
```

Run continuously until interrupted:

```sh
acton test stress --max-time 0
```

Press `Ctrl-C` to stop and print the partial stress result collected so far.
