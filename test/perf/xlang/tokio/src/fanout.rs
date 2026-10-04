//! fan_out: scale work items of about 20 us each, round robin to 64
//! workers; then each worker reports its count once. One operation is a
//! message and about 20 us of work.

use std::time::{Duration, Instant};
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

const WORKERS: usize = 64;
const WORK_US: u64 = 20;

/// Work on this thread for about us microseconds, as busy() in
/// scheduling.act: integer arithmetic, reading the clock every 128 passes
pub fn busy(us: u64) -> u64 {
    let end = Instant::now() + Duration::from_micros(us);
    let (mut x, mut n) = (1u64, 0u64);
    while n % 128 != 0 || Instant::now() < end {
        x = x.wrapping_mul(1103515245).wrapping_add(12345) % 2147483648;
        n += 1;
    }
    std::hint::black_box(x)
}

pub struct FanOut {
    ins: Vec<UnboundedSender<u64>>,
    result: UnboundedReceiver<i64>,
}

impl FanOut {
    /// Start the workers. A message carries the microseconds of a work
    /// item, or 0 to ask for the count.
    pub fn start() -> FanOut {
        let (result_tx, result) = unbounded_channel();
        let mut ins = Vec::new();
        for _ in 0..WORKERS {
            let (tx, mut rx) = unbounded_channel::<u64>();
            let result_tx = result_tx.clone();
            tokio::spawn(async move {
                let mut done = 0i64;
                while let Some(us) = rx.recv().await {
                    if us > 0 {
                        busy(us);
                        done += 1;
                    } else {
                        let _ = result_tx.send(done);
                        done = 0;
                    }
                }
            });
            ins.push(tx);
        }
        FanOut { ins, result }
    }

    pub async fn body(&mut self, scale: i64) {
        for i in 0..scale as usize {
            let _ = self.ins[i % WORKERS].send(WORK_US);
        }
        // Each worker gets the request for its count after its items
        for tx in &self.ins {
            let _ = tx.send(0);
        }
        let mut items = 0;
        for _ in 0..WORKERS {
            items += self.result.recv().await.unwrap();
        }
        assert_eq!(items, scale, "workers did {items} of {scale} items");
    }
}
