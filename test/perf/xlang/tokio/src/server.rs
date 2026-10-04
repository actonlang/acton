//! hot_server: 64 clients make synchronous calls to one server, whose
//! channel stays full. A call sends a request with a oneshot reply channel
//! of its own and awaits the answer, as an Acton call creates a future of
//! its own and awaits it. One operation is a call and its reply.

use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::sync::oneshot;

const CLIENTS: usize = 64;

pub struct HotServer {
    starts: Vec<UnboundedSender<i64>>,
    done: UnboundedReceiver<(i64, i64)>,
}

impl HotServer {
    pub fn start() -> HotServer {
        let (server, mut requests) = unbounded_channel::<(i64, oneshot::Sender<i64>)>();
        tokio::spawn(async move {
            let mut calls = 0u64;
            while let Some((x, reply)) = requests.recv().await {
                calls += 1;
                let _ = reply.send(x + 1);
            }
            std::hint::black_box(calls);
        });
        let (done_tx, done) = unbounded_channel();
        let mut starts = Vec::new();
        for _ in 0..CLIENTS {
            let (start, mut start_rx) = unbounded_channel::<i64>();
            let server = server.clone();
            let done_tx = done_tx.clone();
            tokio::spawn(async move {
                while let Some(n) = start_rx.recv().await {
                    let mut acc = 0;
                    for j in 0..n {
                        let (tx, rx) = oneshot::channel();
                        let _ = server.send((j, tx));
                        acc += rx.await.unwrap();
                    }
                    let _ = done_tx.send((n, acc));
                }
            });
            starts.push(start);
        }
        HotServer { starts, done }
    }

    /// Have every client make its share of scale calls and wait for all
    pub async fn body(&mut self, scale: i64) {
        for (i, start) in self.starts.iter().enumerate() {
            let _ = start.send(crate::share(scale, CLIENTS as i64, i as i64));
        }
        for _ in 0..CLIENTS {
            let (n, acc) = self.done.recv().await.unwrap();
            // Each client adds x + 1 for x in range(n)
            assert_eq!(acc, n * (n + 1) / 2, "client sum for {n} calls");
        }
    }
}
