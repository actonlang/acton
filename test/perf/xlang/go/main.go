// Command xlang-go runs Go ports of tests from test/perf/src/scheduling.act,
// so that Acton's runtime can be compared with Go's on the same machine.
// Each test keeps the sizes and the structure of its Acton original; the
// files of this package say how each one maps.
//
// The measurement follows Acton's perf harness: bodies of scale operations,
// a warmup of a tenth of the time budget, then four samples that share the
// rest of it. Each sample gives wall time and process CPU time (getrusage)
// per body. If XLANG_PERF_CTL and XLANG_PERF_ACK name the control and ack
// FIFOs of perf stat --control, the counters run during the samples only.
//
// usage: xlang-go [-threads N] [-scale OPS] [-time MS] TEST
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"runtime"
	"slices"
	"syscall"
	"time"
)

// A test runs one body of scale operations at a time
type test interface {
	body(scale int)
}

// A latency test also reports the round trips of its last body
type latencyTest interface {
	test
	roundTrips() []time.Duration
}

func newTest(name string, scale, threads int) test {
	switch name {
	case "ring":
		return startRings(1, ringSize, 1)
	case "ring_tokens":
		return startRings(1, ringSize, 64)
	case "pairs":
		return startRings(pairs, 2, pairs)
	case "hot_server":
		return startHotServer()
	case "fan_out":
		return startFanOut(scale)
	case "pipeline":
		return startPipeline(scale)
	case "latency_under_load":
		return startLatency(scale, threads)
	}
	return nil
}

// share is part i of total split into parts as evenly as possible
func share(total, parts, i int) int {
	n := total / parts
	if i < total%parts {
		n++
	}
	return n
}

type sample struct {
	Bodies int     `json:"bodies"`
	WallMs float64 `json:"wall_ms_per_body"`
	UserMs float64 `json:"cpu_user_ms_per_body"`
	SysMs  float64 `json:"cpu_sys_ms_per_body"`
}

func cpuTimes() (user, sys int64) {
	var ru syscall.Rusage
	if err := syscall.Getrusage(syscall.RUSAGE_SELF, &ru); err != nil {
		panic(err)
	}
	return ru.Utime.Nano(), ru.Stime.Nano()
}

// percentiles of one body's round trips in ms: p50, p99, max
func percentiles(rtts []time.Duration) [3]float64 {
	s := slices.Clone(rtts)
	slices.Sort(s)
	at := func(p int) float64 { return float64(s[(len(s)-1)*p/100]) / 1e6 }
	return [3]float64{at(50), at(99), at(100)}
}

// measure runs bodies until budget has passed, and at least one
func measure(t test, scale int, budget time.Duration, rtts *[][3]float64) sample {
	u0, s0 := cpuTimes()
	t0 := time.Now()
	n := 0
	for n == 0 || time.Since(t0) < budget {
		t.body(scale)
		n++
		if lt, ok := t.(latencyTest); ok {
			*rtts = append(*rtts, percentiles(lt.roundTrips()))
		}
	}
	wall := time.Since(t0)
	u1, s1 := cpuTimes()
	f := float64(n) * 1e6
	return sample{n, float64(wall) / f, float64(u1-u0) / f, float64(s1-s0) / f}
}

// perfControl sends cmd to perf stat's control FIFO and waits for its ack
func perfControl(cmd string) {
	ctl, ack := os.Getenv("XLANG_PERF_CTL"), os.Getenv("XLANG_PERF_ACK")
	if ctl == "" || ack == "" {
		return
	}
	c, err := os.OpenFile(ctl, os.O_WRONLY, 0)
	if err != nil {
		panic(err)
	}
	if _, err := c.WriteString(cmd + "\n"); err != nil {
		panic(err)
	}
	c.Close()
	a, err := os.Open(ack)
	if err != nil {
		panic(err)
	}
	buf := make([]byte, 64)
	if _, err := a.Read(buf); err != nil {
		panic(err)
	}
	a.Close()
}

func main() {
	threads := flag.Int("threads", runtime.NumCPU(), "GOMAXPROCS")
	scale := flag.Int("scale", 0, "operations per body")
	timeMs := flag.Int("time", 6000, "time budget in milliseconds")
	flag.Parse()
	name := flag.Arg(0)
	runtime.GOMAXPROCS(*threads)
	if *scale <= 0 {
		fmt.Fprintln(os.Stderr, "-scale is required")
		os.Exit(2)
	}

	start := time.Now()
	t := newTest(name, *scale, *threads)
	if t == nil {
		fmt.Fprintln(os.Stderr, "usage: xlang-go [-threads N] -scale OPS [-time MS] ring|ring_tokens|pairs|hot_server|fan_out|pipeline|latency_under_load")
		os.Exit(2)
	}
	budget := time.Duration(*timeMs) * time.Millisecond
	var rtts [][3]float64
	warmup := measure(t, *scale, budget/10, &rtts)

	var m0, m1 runtime.MemStats
	runtime.ReadMemStats(&m0)
	perfControl("enable")
	samples := make([]sample, 0, 4)
	for k := 0; k < 4; k++ {
		remaining := budget - time.Since(start)
		samples = append(samples, measure(t, *scale, remaining/time.Duration(4-k), &rtts))
	}
	perfControl("disable")
	runtime.ReadMemStats(&m1)

	// Like Acton's harness: the mean over samples of the values per body
	var bodies int
	var wall, user, sys float64
	for _, s := range samples {
		bodies += s.Bodies
		wall += s.WallMs / 4
		user += s.UserMs / 4
		sys += s.SysMs / 4
	}
	out := map[string]any{
		"runtime":              "go",
		"version":              runtime.Version(),
		"test":                 name,
		"threads":              *threads,
		"scale":                *scale,
		"time_ms":              *timeMs,
		"warmup":               warmup,
		"samples":              samples,
		"bodies":               bodies,
		"wall_ms_per_body":     wall,
		"cpu_user_ms_per_body": user,
		"cpu_sys_ms_per_body":  sys,
		"alloc_bytes_per_body": float64(m1.TotalAlloc-m0.TotalAlloc) / float64(bodies),
		"allocs_per_body":      float64(m1.Mallocs-m0.Mallocs) / float64(bodies),
		"gc_cycles":            m1.NumGC - m0.NumGC,
	}
	if rtts != nil {
		// p50, p99 and max in ms for every body, warmup included, as Acton prints them
		out["rtt_ms_per_body"] = rtts
	}
	js, _ := json.Marshal(out)
	fmt.Println(string(js))
}
