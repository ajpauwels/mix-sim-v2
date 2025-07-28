use std::collections::VecDeque;

use serde::{Deserialize, Serialize};
use sim::{
    input_modeling::{dynamic_rng::DynRng, ContinuousRandomVariable},
    models::{
        model_trait::SerializableModel, DevsModel, ModelMessage, ModelRecord, Reportable,
        ReportableModel,
    },
    simulator::Services,
    utils::errors::SimulationError,
};
use sim_derive::SerializableModel;

#[derive(Debug, Clone, Serialize, Deserialize, SerializableModel)]
pub struct DropSender {
    message_interdeparture_time: ContinuousRandomVariable,
    #[serde(default = "max_usize")]
    queue_capacity: usize,
    ports_in: PortsIn,
    ports_out: PortsOut,
    #[serde(default)]
    store_records: bool,
    #[serde(default)]
    state: State,
    #[serde(skip)]
    rng: Option<DynRng>,
}

fn max_usize() -> usize {
    usize::MAX
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PortsIn {
    job: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ArrivalPort {
    Job,
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PortsOut {
    job: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct State {
    phase: Phase,
    until_next_event: f64,
    until_job: f64,
    last_drop_job: usize,
    queue: VecDeque<String>,
    records: Vec<ModelRecord>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
enum Phase {
    Initializing,
    Generating,
}

impl Default for State {
    fn default() -> Self {
        Self {
            phase: Phase::Initializing,
            until_next_event: 0.0,
            until_job: 0.0,
            last_drop_job: 0,
            queue: VecDeque::new(),
            records: Vec::new(),
        }
    }
}

impl DropSender {
    pub fn new(
        message_interdeparture_time: ContinuousRandomVariable,
        queue_capacity: Option<usize>,
        input_job_port: String,
        output_job_port: String,
        store_records: bool,
        rng: Option<DynRng>,
    ) -> Self {
        Self {
            message_interdeparture_time,
            queue_capacity: queue_capacity.unwrap_or(usize::MAX),
            ports_in: PortsIn {
                job: input_job_port,
            },
            ports_out: PortsOut {
                job: output_job_port,
            },
            store_records,
            state: State::default(),
            rng,
        }
    }

    fn arrival_port(&self, message_port: &str) -> ArrivalPort {
        if message_port == self.ports_in.job {
            ArrivalPort::Job
        } else {
            ArrivalPort::Unknown
        }
    }

    fn initialize(
        &mut self,
        services: &mut Services,
    ) -> Result<Vec<ModelMessage>, SimulationError> {
        let interdeparture = match &self.rng {
            Some(rng) => self
                .message_interdeparture_time
                .random_variate(rng.clone())?,
            None => self
                .message_interdeparture_time
                .random_variate(services.global_rng())?,
        };
        self.state.phase = Phase::Generating;
        self.state.until_next_event = interdeparture;
        self.state.until_job = interdeparture;
        self.record(
            services.global_time(),
            String::from("Initialization"),
            String::from(""),
        );
        Ok(Vec::new())
    }

    fn release_job(
        &mut self,
        services: &mut Services,
    ) -> Result<Vec<ModelMessage>, SimulationError> {
        let interdeparture = match &self.rng {
            Some(rng) => self
                .message_interdeparture_time
                .random_variate(rng.clone())?,
            None => self
                .message_interdeparture_time
                .random_variate(services.global_rng())?,
        };
        self.state.phase = Phase::Generating;
        self.state.until_next_event = interdeparture;
        self.state.until_job = interdeparture;
        match self.state.queue.pop_front() {
            Some(job) => {
                self.record(services.global_time(), "Departure".to_owned(), job.clone());
                Ok(vec![ModelMessage {
                    content: job,
                    port_name: self.ports_out.job.clone(),
                }])
            }
            None => {
                self.state.last_drop_job += 1;
                self.record(
                    services.global_time(),
                    "Departure".to_owned(),
                    format!("drop {}", self.state.last_drop_job),
                );
                Ok(vec![ModelMessage {
                    content: format!("drop {}", self.state.last_drop_job),
                    port_name: self.ports_out.job.clone(),
                }])
            }
        }
    }

    fn add_job(&mut self, incoming_message: &ModelMessage, services: &mut Services) {
        self.state.queue.push_back(incoming_message.content.clone());
        self.record(
            services.global_time(),
            String::from("Arrival"),
            incoming_message.content.clone(),
        );
    }

    fn ignore_job(&mut self, incoming_message: &ModelMessage, services: &mut Services) {
        self.record(
            services.global_time(),
            String::from("Drop"),
            incoming_message.content.clone(),
        );
    }

    fn record(&mut self, time: f64, action: String, subject: String) {
        if self.store_records {
            self.state.records.push(ModelRecord {
                time,
                action,
                subject,
            });
        }
    }
}

impl DevsModel for DropSender {
    fn events_ext(
        &mut self,
        incoming_message: &ModelMessage,
        services: &mut Services,
    ) -> Result<(), SimulationError> {
        match (
            self.arrival_port(&incoming_message.port_name),
            self.state.queue.is_empty(),
            self.state.queue.len() == self.queue_capacity,
        ) {
            (ArrivalPort::Job, true, true) => Err(SimulationError::InvalidModelState),
            (ArrivalPort::Job, false, true) => Ok(self.ignore_job(incoming_message, services)),
            (ArrivalPort::Job, _, false) => Ok(self.add_job(incoming_message, services)),
            (ArrivalPort::Unknown, _, _) => Err(SimulationError::InvalidMessage),
        }
    }

    fn events_int(
        &mut self,
        services: &mut Services,
    ) -> Result<Vec<ModelMessage>, SimulationError> {
        match &self.state.phase {
            Phase::Initializing => self.initialize(services),
            Phase::Generating => self.release_job(services),
        }
    }

    fn time_advance(&mut self, time_delta: f64) {
        self.state.until_next_event -= time_delta;
    }

    fn until_next_event(&self) -> f64 {
        self.state.until_next_event
    }
}

impl Reportable for DropSender {
    fn status(&self) -> String {
        format!("Sending or generating {}s", self.ports_out.job)
    }

    fn records(&self) -> &Vec<ModelRecord> {
        &self.state.records
    }
}

impl ReportableModel for DropSender {}
