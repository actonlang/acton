//! Tokio ports of tests from test/perf/src/scheduling.act, so that Acton's
//! runtime can be compared with Tokio's on the same machine. Each test keeps
//! the sizes and the structure of its Acton original; the modules say how
//! each one maps. The runtime is Tokio's multi-thread scheduler; only
//! latency_under_load enables the time driver, which it needs for its timer.
//!
//! The measurement follows Acton's perf harness: bodies of scale
//! operations, a warmup of a tenth of the time budget, then four samples
//! that share the rest of it. Each sample gives wall time and process CPU
//! time (getrusage) per body. If XLANG_PERF_CTL and XLANG_PERF_ACK name the
//! control and ack FIFOs of perf stat --control, the counters run during the
//! samples only.
//!
//! usage: xlang-tokio [--threads N] --scale OPS [--time MS] TEST

mod fanout;
mod latency;
mod pipeline;
mod rings;
mod server;

use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::time::{Duration, Instant};

const TESTS: &str = "ring|ring_tokens|pairs|hot_server|fan_out|pipeline|latency_under_load";

/// Part i of total split into parts as evenly as possible
pub fn share(total: i64, parts: i64, i: i64) -> i64 {
    total / parts + if i < total % parts { 1 } else { 0 }
}

enum Test {
    Rings(rings::Rings),
    HotServer(server::HotServer),
    FanOut(fanout::FanOut),
    Pipeline(pipeline::Pipeline),
    Latency(latency::Latency),
}

impl Test {
    /// Start a test's tasks; this must run on the runtime
    fn start(name: &str, threads: usize) -> Option<Test> {
        Some(match name {
            "ring" => Test::Rings(rings::Rings::start(1, rings::RING_SIZE, 1)),
            "ring_tokens" => Test::Rings(rings::Rings::start(1, rings::RING_SIZE, 64)),
            "pairs" => Test::Rings(rings::Rings::start(rings::PAIRS, 2, rings::PAIRS)),
            "hot_server" => Test::HotServer(server::HotServer::start()),
            "fan_out" => Test::FanOut(fanout::FanOut::start()),
            "pipeline" => Test::Pipeline(pipeline::Pipeline::start()),
            "latency_under_load" => Test::Latency(latency::Latency::start(threads)),
            _ => return None,
        })
    }

    async fn body(&mut self, scale: i64) {
        match self {
            Test::Rings(t) => t.body(scale).await,
            Test::HotServer(t) => t.body(scale).await,
            Test::FanOut(t) => t.body(scale).await,
            Test::Pipeline(t) => t.body(scale).await,
            Test::Latency(t) => t.body(scale).await,
        }
    }

    /// The round trips of the last body, for a latency test
    fn round_trips(&self) -> Option<&[Duration]> {
        match self {
            Test::Latency(t) => Some(&t.rtts),
            _ => None,
        }
    }
}

#[repr(C)]
struct Timeval {
    tv_sec: i64,
    #[cfg(target_os = "macos")]
    tv_usec: i32,
    #[cfg(target_os = "macos")]
    _pad: i32,
    #[cfg(not(target_os = "macos"))]
    tv_usec: i64,
}

#[repr(C)]
struct Rusage {
    ru_utime: Timeval,
    ru_stime: Timeval,
    _rest: [i64; 14],
}

extern "C" {
    fn getrusage(who: i32, usage: *mut Rusage) -> i32;
}

/// User and system CPU time of the process in nanoseconds
fn cpu_times() -> (i64, i64) {
    let mut ru: Rusage = unsafe { std::mem::zeroed() };
    // RUSAGE_SELF is 0 on Linux and macOS
    assert_eq!(unsafe { getrusage(0, &mut ru) }, 0);
    let ns = |t: &Timeval| t.tv_sec * 1_000_000_000 + t.tv_usec as i64 * 1000;
    (ns(&ru.ru_utime), ns(&ru.ru_stime))
}

struct Sample {
    bodies: u64,
    wall_ms: f64,
    user_ms: f64,
    sys_ms: f64,
}

impl Sample {
    fn json(&self) -> String {
        format!(
            "{{\"bodies\":{},\"wall_ms_per_body\":{},\"cpu_user_ms_per_body\":{},\"cpu_sys_ms_per_body\":{}}}",
            self.bodies, self.wall_ms, self.user_ms, self.sys_ms
        )
    }
}

/// p50, p99 and max of one body's round trips, in ms
fn percentiles(rtts: &[Duration]) -> [f64; 3] {
    let mut s = rtts.to_vec();
    s.sort();
    let at = |p: usize| s[(s.len() - 1) * p / 100].as_nanos() as f64 / 1e6;
    [at(50), at(99), at(100)]
}

/// Run bodies until budget has passed, and at least one
async fn measure(t: &mut Test, scale: i64, budget: Duration, rtts: &mut Vec<[f64; 3]>) -> Sample {
    let (u0, s0) = cpu_times();
    let t0 = Instant::now();
    let mut n = 0u64;
    while n == 0 || t0.elapsed() < budget {
        t.body(scale).await;
        n += 1;
        if let Some(r) = t.round_trips() {
            rtts.push(percentiles(r));
        }
    }
    let wall = t0.elapsed();
    let (u1, s1) = cpu_times();
    let f = n as f64 * 1e6;
    Sample {
        bodies: n,
        wall_ms: wall.as_nanos() as f64 / f,
        user_ms: (u1 - u0) as f64 / f,
        sys_ms: (s1 - s0) as f64 / f,
    }
}

/// Send cmd to perf stat's control FIFO and wait for its ack
fn perf_control(cmd: &str) {
    let (Ok(ctl), Ok(ack)) = (std::env::var("XLANG_PERF_CTL"), std::env::var("XLANG_PERF_ACK")) else {
        return;
    };
    OpenOptions::new().write(true).open(ctl).unwrap().write_all(format!("{cmd}\n").as_bytes()).unwrap();
    let mut buf = [0u8; 64];
    File::open(ack).unwrap().read(&mut buf).unwrap();
}

#[cfg(feature = "count-allocs")]
mod counting {
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

    pub static ALLOCS: AtomicU64 = AtomicU64::new(0);
    pub static BYTES: AtomicU64 = AtomicU64::new(0);

    struct Counting;

    unsafe impl GlobalAlloc for Counting {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            ALLOCS.fetch_add(1, Relaxed);
            BYTES.fetch_add(layout.size() as u64, Relaxed);
            System.alloc(layout)
        }
        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            System.dealloc(ptr, layout)
        }
        unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
            ALLOCS.fetch_add(1, Relaxed);
            BYTES.fetch_add(new_size as u64, Relaxed);
            System.realloc(ptr, layout, new_size)
        }
    }

    #[global_allocator]
    static GLOBAL: Counting = Counting;
}

/// Allocations and allocated bytes so far, when counted
fn allocations() -> Option<(u64, u64)> {
    #[cfg(feature = "count-allocs")]
    {
        use std::sync::atomic::Ordering::Relaxed;
        return Some((counting::ALLOCS.load(Relaxed), counting::BYTES.load(Relaxed)));
    }
    #[allow(unreachable_code)]
    None
}

async fn run(name: String, threads: usize, scale: i64, time_ms: u64) -> String {
    let start = Instant::now();
    let Some(mut t) = Test::start(&name, threads) else {
        eprintln!("usage: xlang-tokio [--threads N] --scale OPS [--time MS] {TESTS}");
        std::process::exit(2);
    };
    let budget = Duration::from_millis(time_ms);
    let mut rtts = Vec::new();
    let warmup = measure(&mut t, scale, budget / 10, &mut rtts).await;

    let a0 = allocations();
    perf_control("enable");
    let mut samples = Vec::new();
    for k in 0..4u32 {
        let remaining = budget.saturating_sub(start.elapsed());
        samples.push(measure(&mut t, scale, remaining / (4 - k), &mut rtts).await);
    }
    perf_control("disable");
    let a1 = allocations();

    // Like Acton's harness: the mean over samples of the values per body
    let bodies: u64 = samples.iter().map(|s| s.bodies).sum();
    let mean = |f: fn(&Sample) -> f64| samples.iter().map(f).sum::<f64>() / samples.len() as f64;
    let per_body = |x: Option<u64>| match x {
        Some(v) => format!("{}", v as f64 / bodies as f64),
        None => "null".to_string(),
    };
    let (allocs, bytes) = match (a0, a1) {
        (Some(a), Some(b)) => (Some(b.0 - a.0), Some(b.1 - a.1)),
        _ => (None, None),
    };
    let samples_json: Vec<String> = samples.iter().map(Sample::json).collect();
    // p50, p99 and max in ms for every body, warmup included, as Acton prints them
    let rtts_json = if t.round_trips().is_some() {
        let rows: Vec<String> = rtts.iter().map(|r| format!("[{},{},{}]", r[0], r[1], r[2])).collect();
        format!(",\"rtt_ms_per_body\":[{}]", rows.join(","))
    } else {
        String::new()
    };
    format!(
        "{{\"runtime\":\"tokio\",\"version\":\"tokio 1.53.2\",\"test\":\"{}\",\"threads\":{},\"scale\":{},\"time_ms\":{},\
         \"warmup\":{},\"samples\":[{}],\"bodies\":{},\"wall_ms_per_body\":{},\"cpu_user_ms_per_body\":{},\
         \"cpu_sys_ms_per_body\":{},\"alloc_bytes_per_body\":{},\"allocs_per_body\":{}{}}}",
        name,
        threads,
        scale,
        time_ms,
        warmup.json(),
        samples_json.join(","),
        bodies,
        mean(|s| s.wall_ms),
        mean(|s| s.user_ms),
        mean(|s| s.sys_ms),
        per_body(bytes),
        per_body(allocs),
        rtts_json
    )
}

fn main() {
    let mut threads = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1);
    let mut scale: i64 = 0;
    let mut time_ms: u64 = 6000;
    let mut name = String::new();
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--threads" | "-threads" => threads = args.next().expect("--threads N").parse().unwrap(),
            "--scale" | "-scale" => scale = args.next().expect("--scale OPS").parse().unwrap(),
            "--time" | "-time" => time_ms = args.next().expect("--time MS").parse().unwrap(),
            _ => name = a,
        }
    }
    if scale <= 0 {
        eprintln!("--scale is required");
        std::process::exit(2);
    }
    let mut builder = tokio::runtime::Builder::new_multi_thread();
    builder.worker_threads(threads);
    if name == "latency_under_load" {
        builder.enable_time();
    }
    let rt = builder.build().unwrap();
    // The driver runs as a task on the workers, as Acton's test actors do
    let out = rt.block_on(async move { tokio::spawn(run(name, threads, scale, time_ms)).await.unwrap() });
    println!("{out}");
    // Some tests leave tasks running; don't wait for them
    std::process::exit(0);
}
