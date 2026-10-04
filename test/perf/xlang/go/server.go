package main

import "fmt"

// hot_server: 64 clients make synchronous calls to one server, whose
// channel stays full. A call sends a request with a reply channel of its own
// and waits for the answer, as an Acton call creates a future of its own and
// awaits it. One operation is a call and its reply.

const clients = 64

type request struct {
	x     int
	reply chan int
}

type hotServer struct {
	starts []chan int
	done   chan [2]int
	calls  int
}

func startHotServer() *hotServer {
	s := &hotServer{done: make(chan [2]int, clients)}
	// Each client has at most one call in flight
	in := make(chan request, clients)
	go func() {
		for r := range in {
			s.calls++
			r.reply <- r.x + 1
		}
	}()
	for i := 0; i < clients; i++ {
		start := make(chan int, 1)
		s.starts = append(s.starts, start)
		go func() {
			for n := range start {
				acc := 0
				for j := 0; j < n; j++ {
					reply := make(chan int, 1)
					in <- request{j, reply}
					acc += <-reply
				}
				s.done <- [2]int{n, acc}
			}
		}()
	}
	return s
}

// body has every client make its share of scale calls and waits for all
func (s *hotServer) body(scale int) {
	for i, start := range s.starts {
		start <- share(scale, clients, i)
	}
	for range s.starts {
		r := <-s.done
		// Each client adds x + 1 for x in range(n)
		if r[1] != r[0]*(r[0]+1)/2 {
			panic(fmt.Sprintf("client sum %d for %d calls", r[1], r[0]))
		}
	}
}
