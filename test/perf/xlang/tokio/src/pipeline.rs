//! pipeline: a burst of scale messages through a chain of 8 stages. Every
//! stage adds one to the value and passes it on; the last stage hands it to
//! the sink. One operation is one message through the whole chain: 9
//! deliveries, 8 stages and the sink.

use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

const STAGES: usize = 8;

pub struct Pipeline {
    first: UnboundedSender<i64>,
    sink: UnboundedReceiver<i64>,
}

impl Pipeline {
    pub fn start() -> Pipeline {
        let (sink_tx, sink) = unbounded_channel();
        let mut next = sink_tx;
        // As in scheduling.act, the first stage made is the last in the chain
        for i in 0..STAGES {
            let (tx, mut rx) = unbounded_channel::<i64>();
            let out = next;
            let last = i == 0;
            tokio::spawn(async move {
                while let Some(x) = rx.recv().await {
                    let _ = out.send(if last { x } else { x + 1 });
                }
            });
            next = tx;
        }
        Pipeline { first: next, sink }
    }

    /// Send scale messages at once and wait until the sink has them all
    pub async fn body(&mut self, scale: i64) {
        for _ in 0..scale {
            let _ = self.first.send(0);
        }
        let mut total = 0;
        for _ in 0..scale {
            total += self.sink.recv().await.unwrap();
        }
        // Every message leaves the last stage with x = STAGES - 1
        assert_eq!(total, (STAGES as i64 - 1) * scale, "pipeline total");
    }
}
