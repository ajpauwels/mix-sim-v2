use std::{
    fmt,
    fs::File,
    io,
    sync::atomic::AtomicU64,
    time::{Duration, SystemTime},
};

use nexosim::model::{Context, Model};
use prometheus_client::{
    encoding::{
        text::{encode_eof, encode_registry_with_ts},
        EncodeLabelSet, EncodeLabelValue,
    },
    metrics::{counter::Counter, family::Family, gauge::Gauge},
    registry::Registry,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq, EncodeLabelSet)]
pub struct NoLabels {}

#[derive(Clone, Debug, Hash, PartialEq, Eq, EncodeLabelSet)]
pub struct SimulationLambdaLabels {
    pub process: String,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, EncodeLabelSet)]
pub struct MessageInitiatedLabels {
    pub from: String,
    pub r#type: MessageBodyType,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, EncodeLabelSet)]
pub struct MessageForwardedLabels {}

#[derive(Clone, Debug, Hash, PartialEq, Eq, EncodeLabelSet)]
pub struct MessageTerminatedLabels {
    pub by: String,
    pub r#type: MessageBodyType,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, EncodeLabelValue)]
pub enum MessageBodyType {
    String,
    Loop,
    Drop,
    EntryRequest,
    EntryResponse,
}

pub struct MetricFamilies {
    pub messages_initiated: Family<MessageInitiatedLabels, Counter>,
    pub messages_forwarded: Family<MessageForwardedLabels, Counter>,
    pub messages_terminated: Family<MessageTerminatedLabels, Counter>,
    pub simulation_lambdas: Family<SimulationLambdaLabels, Gauge<f64, AtomicU64>>,
    pub simulation_time: Family<NoLabels, Gauge>,
    pub simulation_user_count: Family<NoLabels, Gauge>,
}

// pub fn setup() -> (MetricFamilies, JoinHandle<()>) {
pub fn setup() -> (Registry, MetricFamilies) {
    let mut registry = <Registry>::default();

    let mf = MetricFamilies {
        messages_initiated: Family::<MessageInitiatedLabels, Counter>::default(),
        messages_forwarded: Family::<MessageForwardedLabels, Counter>::default(),
        messages_terminated: Family::<MessageTerminatedLabels, Counter>::default(),
        simulation_lambdas: Family::<SimulationLambdaLabels, Gauge<f64, AtomicU64>>::default(),
        simulation_time: Family::<NoLabels, Gauge>::default(),
        simulation_user_count: Family::<NoLabels, Gauge>::default(),
    };

    registry.register(
        "messages_initiated",
        "Number of messages created and sent through the network",
        mf.messages_initiated.clone(),
    );
    registry.register(
        "messages_forwarded",
        "Number of messages received and forwarded to the next hop",
        mf.messages_forwarded.clone(),
    );
    registry.register(
        "messages_terminated",
        "Number of messages received and terminated at the hop",
        mf.messages_terminated.clone(),
    );
    registry.register(
        "simulation_lambdas",
        "Poisson distribution parameters",
        mf.simulation_lambdas.clone(),
    );
    registry.register(
        "simulation_time",
        "Duration of the simulation",
        mf.simulation_time.clone(),
    );
    registry.register(
        "simulation_user_count",
        "Number of users simulated",
        mf.simulation_user_count.clone(),
    );

    (registry, mf)
}

pub struct MetricsServer {
    registry: Registry,
    output_file: File,
    start_time: SystemTime,
}

impl MetricsServer {
    pub fn new(registry: Registry, output_path: String) -> Self {
        let output_file = File::create(output_path).unwrap();
        Self {
            registry,
            output_file,
            start_time: SystemTime::now(),
        }
    }

    pub fn get_metrics(&mut self, _: (), cx: &mut Context<Self>) {
        let mut writer = FmtToIo(&mut self.output_file);
        // let ts = cx.time().to_system_time(37).unwrap();
        let sim_dur = Duration::from_secs(cx.time().as_secs().try_into().unwrap());
        match encode_registry_with_ts(&mut writer, &self.registry, self.start_time + sim_dur) {
            Ok(_) => {}
            Err(e) => eprintln!("[METRICS] Failed encoding registry: {e}"),
        };
    }

    pub fn write_eof(&mut self, _: ()) {
        let mut writer = FmtToIo(&mut self.output_file);
        if let Err(e) = encode_eof(&mut writer) {
            eprintln!("[METRICS] Failed encoding EOF: {e}");
        }
    }
}

impl Model for MetricsServer {}

struct FmtToIo<'a, W: io::Write>(&'a mut W);

impl<'a, W: io::Write> fmt::Write for FmtToIo<'a, W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        let mut buf = [0u8; 4];
        self.write_str(c.encode_utf8(&mut buf))
    }
}
