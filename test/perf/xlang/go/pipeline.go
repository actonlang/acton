package main

import "fmt"

// pipeline: a burst of scale messages through a chain of 8 stages. Every
// stage adds one to the value and passes it on; the last stage hands it to
// the sink. Every channel holds a whole burst, so no send ever blocks, as
// with Acton's unbounded mailboxes. One operation is one message through
// the whole chain: 9 deliveries, 8 stages and the sink.

const stages = 8

type pipeline struct {
	first chan int
	sink  chan int
}

func startPipeline(burst int) *pipeline {
	p := &pipeline{sink: make(chan int, burst)}
	next := p.sink
	// As in scheduling.act, the first stage made is the last in the chain
	for i := 0; i < stages; i++ {
		in := make(chan int, burst)
		go func(in <-chan int, out chan<- int, last bool) {
			for x := range in {
				if last {
					out <- x
				} else {
					out <- x + 1
				}
			}
		}(in, next, i == 0)
		next = in
	}
	p.first = next
	return p
}

// body sends scale messages at once and waits until the sink has them all
func (p *pipeline) body(scale int) {
	for i := 0; i < scale; i++ {
		p.first <- 0
	}
	total := 0
	for i := 0; i < scale; i++ {
		total += <-p.sink
	}
	// Every message leaves the last stage with x = stages - 1
	if total != (stages-1)*scale {
		panic(fmt.Sprintf("pipeline total %d", total))
	}
}
