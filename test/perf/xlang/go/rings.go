package main

// ring, ring_tokens and pairs: tokens passed around rings of goroutines.
// One hop is one message from a goroutine to the next one over a channel,
// as one hop in RingTest is one message to an actor.

const (
	ringSize = 1000
	pairs    = 64
)

type token struct {
	id   int
	hops int
}

// node passes each token on to the next node, or reports it done when it
// has no hops left
func node(in <-chan token, next chan<- token, done chan<- int) {
	for m := range in {
		if m.hops <= 0 {
			done <- m.id
			continue
		}
		next <- token{m.id, m.hops - 1}
	}
}

type rings struct {
	nodes  []chan token
	tokens int
	done   chan int
}

// startRings starts count rings of size nodes each. A node's channel holds
// as many messages as a ring has tokens, so no send ever blocks, as with
// Acton's unbounded mailboxes.
func startRings(count, size, tokens int) *rings {
	r := &rings{nodes: make([]chan token, count*size), tokens: tokens, done: make(chan int, tokens)}
	for i := range r.nodes {
		r.nodes[i] = make(chan token, tokens/count)
	}
	for i := range r.nodes {
		// The next node in i's own ring
		go node(r.nodes[i], r.nodes[i-i%size+(i+1)%size], r.done)
	}
	return r
}

// body passes scale hops in total, split over the tokens, and waits until
// every token is done
func (r *rings) body(scale int) {
	for i := 0; i < r.tokens; i++ {
		r.nodes[i*len(r.nodes)/r.tokens] <- token{i, share(scale, r.tokens, i)}
	}
	for i := 0; i < r.tokens; i++ {
		<-r.done
	}
}
