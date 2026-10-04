//! latency_under_load: twice as many busy tasks as worker threads keep
//! every thread busy; each works for 100 us and then yields, as a Hog actor
//! runs 100 us per message and sends itself the next one. Meanwhile a probe
//! sends a round trip through an idle task every millisecond. One operation
//! is a round trip; the result is the round-trip time.

use std::time::{Duration, Instant};
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

const HOG_US: u64 = 100;

pub struct Latency {
    echo: UnboundedSender<Instant>,
    replies: UnboundedReceiver<Instant>,
    pub rtts: Vec<Duration>,
}

impl Latency {
    pub fn start(threads: usize) -> Latency {
        let (echo, mut echo_rx) = unbounded_channel::<Instant>();
        let (replies_tx, replies) = unbounded_channel();
        tokio::spawn(async move {
            while let Some(t0) = echo_rx.recv().await {
                let _ = replies_tx.send(t0);
            }
        });
        for _ in 0..2 * threads {
            tokio::spawn(async {
                loop {
                    crate::fanout::busy(HOG_US);
                    tokio::task::yield_now().await;
                }
            });
        }
        Latency { echo, replies, rtts: Vec::new() }
    }

    /// Send scale round trips, one per millisecond, and wait for the replies
    pub async fn body(&mut self, scale: i64) {
        let echo = self.echo.clone();
        tokio::spawn(async move {
            for _ in 0..scale {
                let _ = echo.send(Instant::now());
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        });
        self.rtts.clear();
        for _ in 0..scale {
            let t0 = self.replies.recv().await.unwrap();
            self.rtts.push(t0.elapsed());
        }
    }
}
