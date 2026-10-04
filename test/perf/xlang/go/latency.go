package main

import (
	"runtime"
	"time"
)

// latency_under_load: twice as many busy goroutines as threads keep every
// thread busy; each works for 100 us and then yields, as a Hog actor runs
// 100 us per message and sends itself the next one. Meanwhile a probe sends
// a round trip through an idle goroutine every millisecond. One operation
// is a round trip; the result is the round-trip time.

const hogUs = 100

type latency struct {
	echo    chan time.Time
	replies chan time.Time
	rtts    []time.Duration
}

func startLatency(roundTrips, threads int) *latency {
	// The channels hold a whole body's round trips, so the probe never blocks
	l := &latency{echo: make(chan time.Time, roundTrips), replies: make(chan time.Time, roundTrips)}
	go func() {
		for t0 := range l.echo {
			l.replies <- t0
		}
	}()
	for i := 0; i < 2*threads; i++ {
		go func() {
			for {
				busy(hogUs)
				runtime.Gosched()
			}
		}()
	}
	return l
}

// body sends scale round trips, one per millisecond, and waits for the replies
func (l *latency) body(scale int) {
	go func() {
		for i := 0; i < scale; i++ {
			l.echo <- time.Now()
			time.Sleep(time.Millisecond)
		}
	}()
	l.rtts = l.rtts[:0]
	for i := 0; i < scale; i++ {
		t0 := <-l.replies
		l.rtts = append(l.rtts, time.Since(t0))
	}
}

func (l *latency) roundTrips() []time.Duration {
	return l.rtts
}
