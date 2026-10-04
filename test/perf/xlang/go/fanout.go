package main

import (
	"fmt"
	"time"
)

// fan_out: scale work items of about 20 us each, round robin to 64
// workers; then each worker reports its count once. One operation is a
// message and about 20 us of work.

const (
	workers = 64
	workUs  = 20
)

// busy works on this thread for about us microseconds, as busy() in
// scheduling.act: integer arithmetic, reading the clock every 128 passes
func busy(us int) int {
	end := time.Now().Add(time.Duration(us) * time.Microsecond)
	x, n := 1, 0
	for n%128 != 0 || time.Now().Before(end) {
		x = (x*1103515245 + 12345) % 2147483648
		n++
	}
	return x
}

type fanOut struct {
	ins    []chan int
	result chan int
}

// startFanOut starts the workers. A channel carries the microseconds of a
// work item, or 0 to ask for the count, and holds a worker's whole share of
// a burst, so no send ever blocks.
func startFanOut(burst int) *fanOut {
	f := &fanOut{result: make(chan int, workers)}
	for i := 0; i < workers; i++ {
		in := make(chan int, burst/workers+2)
		f.ins = append(f.ins, in)
		go func() {
			done := 0
			for us := range in {
				if us > 0 {
					busy(us)
					done++
				} else {
					f.result <- done
					done = 0
				}
			}
		}()
	}
	return f
}

func (f *fanOut) body(scale int) {
	for i := 0; i < scale; i++ {
		f.ins[i%workers] <- workUs
	}
	// Each worker gets the request for its count after its items
	for _, in := range f.ins {
		in <- 0
	}
	items := 0
	for range f.ins {
		items += <-f.result
	}
	if items != scale {
		panic(fmt.Sprintf("workers did %d of %d items", items, scale))
	}
}
