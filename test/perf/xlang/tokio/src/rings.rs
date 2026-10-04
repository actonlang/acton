//! ring, ring_tokens and pairs: tokens passed around rings of tasks. One
//! hop is one message from a task to the next one over an unbounded mpsc
//! channel, as one hop in RingTest is one message to an actor.

use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

pub const RING_SIZE: usize = 1000;
pub const PAIRS: usize = 64;

struct Token {
    id: i64,
    hops: i64,
}

/// Pass each token on to the next node, or report it done when it has no
/// hops left
async fn node(mut rx: UnboundedReceiver<Token>, next: UnboundedSender<Token>, done: UnboundedSender<i64>) {
    while let Some(m) = rx.recv().await {
        if m.hops <= 0 {
            let _ = done.send(m.id);
            continue;
        }
        let _ = next.send(Token { id: m.id, hops: m.hops - 1 });
    }
}

pub struct Rings {
    nodes: Vec<UnboundedSender<Token>>,
    tokens: usize,
    done: UnboundedReceiver<i64>,
}

impl Rings {
    /// Start count rings of size nodes each
    pub fn start(count: usize, size: usize, tokens: usize) -> Rings {
        let (done_tx, done) = unbounded_channel();
        let (nodes, receivers): (Vec<_>, Vec<_>) = (0..count * size).map(|_| unbounded_channel()).unzip();
        for (i, rx) in receivers.into_iter().enumerate() {
            // The next node in i's own ring
            let next = nodes[i - i % size + (i + 1) % size].clone();
            tokio::spawn(node(rx, next, done_tx.clone()));
        }
        Rings { nodes, tokens, done }
    }

    /// Pass scale hops in total, split over the tokens, and wait until
    /// every token is done
    pub async fn body(&mut self, scale: i64) {
        let tokens = self.tokens as i64;
        for i in 0..self.tokens {
            let hops = crate::share(scale, tokens, i as i64);
            let _ = self.nodes[i * self.nodes.len() / self.tokens].send(Token { id: i as i64, hops });
        }
        for _ in 0..self.tokens {
            self.done.recv().await;
        }
    }
}
