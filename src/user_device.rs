use std::{
    collections::{HashMap, HashSet, VecDeque},
    time::Duration,
};

use ct_merkle::RootHash;
use nexosim::{
    model::{BuildContext, Context, Model, ProtoModel},
    ports::{Output, Requestor},
    simulation::Mailbox,
    time::MonotonicTime,
};
use prometheus_client::metrics::{counter::Counter, family::Family, histogram::Histogram};
use rand::{
    seq::{IndexedRandom, IteratorRandom, SliceRandom},
    Rng, RngCore, SeedableRng,
};
use rand_chacha::ChaCha12Rng;
use rand_distr::{Distribution, Exp, Uniform};
use sha2::Sha256;

use crate::{
    directory_entry::DirectoryEntry,
    message::{
        DropMessage, EntryRequestMessage, EntryResponseMessage, LoopMessage, Message, MessageBody,
        UnprocessedMessage, UserMessage,
    },
    poisson_generator::PoissonGenerator,
    poisson_queue::{PoissonQueue, ProtoPoissonQueue},
    prometheus::{
        MessageBodyType, MessageDroppedLabels, MessageForwardedLabels, MessageInitiatedLabels,
        MessageLatencyHistogramBuilder, MessageLatencyLabels, MessageTerminatedLabels,
        MetricFamilies,
    },
};

pub struct Metrics {
    messages_initiated: Family<MessageInitiatedLabels, Counter>,
    messages_forwarded: Family<MessageForwardedLabels, Counter>,
    messages_terminated: Family<MessageTerminatedLabels, Counter>,
    messages_dropped: Family<MessageDroppedLabels, Counter>,
    message_latency: Family<MessageLatencyLabels, Histogram, MessageLatencyHistogramBuilder>,
}

pub struct ProtoUserDevice<'a> {
    pub message_out: Output<Message>,
    pub user_out: Output<Message>,
    pub root_req: Requestor<(), RootHash<Sha256>>,
    pub entries_req: Requestor<(RootHash<Sha256>, Vec<usize>), Vec<DirectoryEntry>>,
    name: String,
    availability: Option<f64>,
    lambda_p: Option<f64>,
    lambda_l: Option<f64>,
    lambda_d: Option<f64>,
    lambda_e: Option<f64>,
    lambda_mu: Option<f64>,
    mix_directory: Vec<DirectoryEntry>,
    chain_length: usize,
    rng: Option<ChaCha12Rng>,
    mf: Option<&'a MetricFamilies>,
}

impl<'a> ProtoUserDevice<'a> {
    pub fn new(name: &str, mf: Option<&'a MetricFamilies>) -> Self {
        Self {
            message_out: Default::default(),
            user_out: Default::default(),
            root_req: Default::default(),
            entries_req: Default::default(),
            name: name.to_owned(),
            availability: None,
            lambda_p: None,
            lambda_l: None,
            lambda_d: None,
            lambda_e: None,
            lambda_mu: None,
            mix_directory: Vec::new(),
            chain_length: 0,
            rng: None,
            mf,
        }
    }

    pub fn availability(mut self, availability: f64) -> Self {
        self.availability = Some(availability);
        self
    }

    pub fn lambda_p(mut self, lambda_p: f64) -> Self {
        self.lambda_p = Some(lambda_p);
        self
    }

    pub fn lambda_l(mut self, lambda_l: f64) -> Self {
        self.lambda_l = Some(lambda_l);
        self
    }

    pub fn lambda_d(mut self, lambda_d: f64) -> Self {
        self.lambda_d = Some(lambda_d);
        self
    }

    pub fn lambda_e(mut self, lambda_e: f64) -> Self {
        self.lambda_e = Some(lambda_e);
        self
    }

    pub fn lambda_mu(mut self, lambda_mu: f64) -> Self {
        self.lambda_mu = Some(lambda_mu);
        self
    }

    pub fn mix_directory(mut self, mix_directory: Vec<DirectoryEntry>) -> Self {
        self.mix_directory = mix_directory;
        self
    }

    pub fn chain_length(mut self, chain_length: usize) -> Self {
        self.chain_length = chain_length;
        self
    }

    pub fn rng(mut self, rng: Option<ChaCha12Rng>) -> Self {
        self.rng = rng;
        self
    }
}

impl<'a> ProtoModel for ProtoUserDevice<'a> {
    type Model = UserDevice;

    fn build(mut self, cx: &mut BuildContext<Self>) -> Self::Model {
        let mut ud = UserDevice::new(
            &self.name,
            self.availability.unwrap_or(1.0),
            self.mix_directory,
            self.chain_length,
            self.lambda_mu.unwrap_or(0.0),
            self.lambda_p.unwrap_or(0.0) == 0.0,
            self.rng.as_mut().map(ChaCha12Rng::from_rng),
            self.mf,
        );

        // Create the payload message, loop message, and drop message
        // generators
        let mut payload = ProtoPoissonQueue::new(
            self.lambda_p.unwrap_or(0.0),
            32,
            move |_name, ts, rng| {
                let mut uuid_bytes = [0u8; 16];
                rng.fill_bytes(&mut uuid_bytes);
                let id = uuid::Builder::from_random_bytes(uuid_bytes)
                    .into_uuid()
                    .hyphenated()
                    .to_string();
                UnprocessedMessage::Drop(DropMessage { id, ts })
            },
            self.rng.as_mut().map(ChaCha12Rng::from_rng),
        );
        let payload_mbox = Mailbox::new();
        let mut r#loop = PoissonGenerator::new(
            self.lambda_l.unwrap_or(0.0),
            move |_name, ts, rng| {
                let mut uuid_bytes = [0u8; 16];
                rng.fill_bytes(&mut uuid_bytes);
                let id = uuid::Builder::from_random_bytes(uuid_bytes)
                    .into_uuid()
                    .hyphenated()
                    .to_string();
                UnprocessedMessage::Loop(LoopMessage { id, ts })
            },
            self.rng.as_mut().map(ChaCha12Rng::from_rng),
        );
        let loop_mbox = Mailbox::new();
        let mut drop = PoissonGenerator::new(
            self.lambda_d.unwrap_or(0.0),
            move |_name, ts, rng| {
                let mut uuid_bytes = [0u8; 16];
                rng.fill_bytes(&mut uuid_bytes);
                let id = uuid::Builder::from_random_bytes(uuid_bytes)
                    .into_uuid()
                    .hyphenated()
                    .to_string();
                UnprocessedMessage::Drop(DropMessage { id, ts })
            },
            self.rng.as_mut().map(ChaCha12Rng::from_rng),
        );
        let drop_mbox = Mailbox::new();
        let mut entries = PoissonGenerator::new(
            self.lambda_e.unwrap_or(0.0),
            move |_name, ts, rng| {
                let mut uuid_bytes = [0u8; 16];
                rng.fill_bytes(&mut uuid_bytes);
                let id = uuid::Builder::from_random_bytes(uuid_bytes)
                    .into_uuid()
                    .hyphenated()
                    .to_string();
                UnprocessedMessage::EntryRequest(EntryRequestMessage { id, ts })
            },
            self.rng.as_mut().map(ChaCha12Rng::from_rng),
        );
        let entries_mbox = Mailbox::new();

        // Connect model inputs to submodels
        ud.queue_out
            .connect(PoissonQueue::message_in, &payload_mbox);
        ud.lambda_p.connect(PoissonQueue::lambda, &payload_mbox);
        ud.lambda_l.connect(PoissonGenerator::lambda, &loop_mbox);
        ud.lambda_d.connect(PoissonGenerator::lambda, &drop_mbox);
        ud.lambda_e.connect(PoissonGenerator::lambda, &entries_mbox);

        // Connect submodel outputs to user device output
        payload
            .message_out
            .connect(UserDevice::message_out, cx.address());
        r#loop
            .message_out
            .connect(UserDevice::message_out, cx.address());
        drop.message_out
            .connect(UserDevice::message_out, cx.address());
        entries
            .message_out
            .connect(UserDevice::message_out, cx.address());

        // Move the prototype's outputs to the submodel
        ud.message_out = self.message_out;
        ud.user_out = self.user_out;
        ud.root_req = self.root_req;
        ud.entries_req = self.entries_req;

        // Add submodels to the simulation
        cx.add_submodel(payload, payload_mbox, "payload".to_owned());
        cx.add_submodel(r#loop, loop_mbox, "loop".to_owned());
        cx.add_submodel(drop, drop_mbox, "drop".to_owned());
        cx.add_submodel(entries, entries_mbox, "entries".to_owned());

        ud
    }
}

pub struct UserDevice {
    // Output to internal queue
    queue_out: Output<UnprocessedMessage>,

    // Output to user
    user_out: Output<Message>,

    // Outputs to transport
    message_out: Output<Message>,
    root_req: Requestor<(), RootHash<Sha256>>,
    entries_req: Requestor<(RootHash<Sha256>, Vec<usize>), Vec<DirectoryEntry>>,

    // Outputs to internal generators
    lambda_p: Output<f64>,
    lambda_l: Output<f64>,
    lambda_d: Output<f64>,
    lambda_e: Output<f64>,

    // Per-hop Poisson parameter
    lambda_mu: f64,

    // True if user messages should be sent immediately without
    // Poisson-distributed delay
    disable_lambda_p: bool,

    // Percent of time device is available when receiving a message
    // from server
    availability: f64,

    // Name of the device
    name: String,

    // Current directory root
    directory_root: Option<RootHash<Sha256>>,

    // Directory of mix users
    mix_directory: Vec<DirectoryEntry>,

    // Length of the mix chain
    chain_length: usize,

    // Loop message IDs
    loop_messages: HashMap<String, MonotonicTime>,

    // Deterministic rng
    rng: ChaCha12Rng,

    // Optional metrics
    metrics: Option<Metrics>,
}

impl UserDevice {
    fn new(
        name: &str,
        availability: f64,
        mix_directory: Vec<DirectoryEntry>,
        chain_length: usize,
        lambda_mu: f64,
        disable_lambda_p: bool,
        rng: Option<ChaCha12Rng>,
        mf: Option<&MetricFamilies>,
    ) -> Self {
        Self {
            message_out: Default::default(),
            queue_out: Default::default(),
            user_out: Default::default(),
            root_req: Default::default(),
            entries_req: Default::default(),
            lambda_p: Default::default(),
            lambda_l: Default::default(),
            lambda_d: Default::default(),
            lambda_e: Default::default(),
            lambda_mu,
            disable_lambda_p,
            availability,
            name: name.to_owned(),
            directory_root: None,
            mix_directory,
            chain_length,
            loop_messages: HashMap::new(),
            rng: rng.unwrap_or(ChaCha12Rng::from_os_rng()),
            metrics: mf.as_ref().map(|mf| Metrics {
                messages_initiated: mf.messages_initiated.clone(),
                messages_forwarded: mf.messages_forwarded.clone(),
                messages_terminated: mf.messages_terminated.clone(),
                messages_dropped: mf.messages_dropped.clone(),
                message_latency: mf.message_latency.clone(),
            }),
        }
    }

    pub async fn user_message_in(&mut self, msg: Option<UserMessage>, cx: &mut Context<Self>) {
        if let Some(msg) = msg {
            if self.disable_lambda_p {
                self.message_out(UnprocessedMessage::User(msg), cx).await;
            } else {
                self.queue_out.send(UnprocessedMessage::User(msg)).await;
            }
        } else {
            panic!("UserDevice received an input from the user but it was None");
        }
    }

    pub async fn server_message_in(&mut self, mut msg: Message, cx: &mut Context<Self>) {
        if msg.to != self.name {
            panic!(
                "User device received a message from the server not meant for the device, to: {}",
                &msg.to
            );
        }
        match msg.chain.pop_front() {
            Some((to, interval)) => {
                let rand_float = self.rng.random_range(0.0..1.0);
                if rand_float < self.availability {
                    msg.to = to;
                    if interval == 0.0 {
                        self.message_out(UnprocessedMessage::Mix(msg), cx).await;
                    } else {
                        let interval = Duration::from_secs_f64(interval);
                        cx.schedule_event(
                            interval,
                            Self::message_out,
                            UnprocessedMessage::Mix(msg),
                        )
                        .unwrap();
                    }
                } else if let Some(ref metrics) = self.metrics {
                    metrics
                        .messages_dropped
                        .get_or_create(&MessageDroppedLabels {
                            r#type: (&msg.body).into(),
                        })
                        .inc();
                }
            }
            None => {
                match msg.body {
                    MessageBody::EntryRequest { idxs, mut surb } => {
                        if let Some(ref metrics) = self.metrics {
                            metrics
                                .messages_terminated
                                .get_or_create(&MessageTerminatedLabels {
                                    by: self.name.clone(),
                                    r#type: MessageBodyType::EntryRequest,
                                })
                                .inc();
                            metrics
                                .message_latency
                                .get_or_create(&MessageLatencyLabels {
                                    r#type: MessageBodyType::EntryRequest,
                                })
                                .observe(cx.time().duration_since(msg.ts).as_secs_f64());
                        }
                        let directory_root = self.get_directory_root().await.clone();
                        let entries = self
                            .entries_req
                            .send((directory_root.clone(), idxs))
                            .await
                            .next();
                        if let Some(entries) = entries {
                            let mut uuid_bytes = [0u8; 16];
                            self.rng.fill_bytes(&mut uuid_bytes);
                            let id = uuid::Builder::from_random_bytes(uuid_bytes)
                                .into_uuid()
                                .hyphenated()
                                .to_string();
                            if let Some((to, interval)) = surb.pop_front() {
                                if interval == 0.0 {
                                    self.message_out(
                                        UnprocessedMessage::EntryResponse(EntryResponseMessage {
                                            id,
                                            ts: cx.time(),
                                            to,
                                            entries,
                                            chain: surb,
                                        }),
                                        cx,
                                    )
                                    .await;
                                } else {
                                    let interval = Duration::from_secs_f64(interval);
                                    cx.schedule_event(
                                        interval,
                                        Self::message_out,
                                        UnprocessedMessage::EntryResponse(EntryResponseMessage {
                                            id,
                                            ts: cx.time(),
                                            to,
                                            entries,
                                            chain: surb,
                                        }),
                                    )
                                    .unwrap();
                                }
                            } else {
                                panic!("User device received an entry request but SURB was empty");
                            }
                        } else {
                            panic!(
                                "User device tried to get entries from the transport but received none"
                            );
                        }
                    }
                    MessageBody::EntryResponse { mut entries } => {
                        if let Some(ref metrics) = self.metrics {
                            metrics
                                .messages_terminated
                                .get_or_create(&MessageTerminatedLabels {
                                    by: self.name.clone(),
                                    r#type: MessageBodyType::EntryResponse,
                                })
                                .inc();
                            metrics
                                .message_latency
                                .get_or_create(&MessageLatencyLabels {
                                    r#type: MessageBodyType::EntryResponse,
                                })
                                .observe(cx.time().duration_since(msg.ts).as_secs_f64());
                        }
                        entries.retain(|entry| entry.id != self.name);
                        let current_len = self.mix_directory.len();
                        let new_len = entries.len();
                        if current_len == new_len {
                            self.mix_directory = entries;
                        } else if current_len < new_len {
                            self.mix_directory = entries
                                .into_iter()
                                .choose_multiple(&mut self.rng, current_len);
                        } else {
                            let die = match Uniform::new(0, current_len) {
                                Ok(die) => die,
                                Err(e) => panic!("{}", e),
                            };
                            let mut taken_idxs = HashSet::<usize>::new();
                            for entry in entries {
                                let mut idx: usize = die.sample(&mut self.rng);
                                while taken_idxs.contains(&idx) {
                                    idx = die.sample(&mut self.rng);
                                }
                                taken_idxs.insert(idx);
                                self.mix_directory[idx] = entry;
                            }
                        }
                    }
                    MessageBody::Loop => {
                        if let Some(ref metrics) = self.metrics {
                            metrics
                                .messages_terminated
                                .get_or_create(&MessageTerminatedLabels {
                                    by: self.name.clone(),
                                    r#type: MessageBodyType::Loop,
                                })
                                .inc();
                            metrics
                                .message_latency
                                .get_or_create(&MessageLatencyLabels {
                                    r#type: MessageBodyType::Loop,
                                })
                                .observe(cx.time().duration_since(msg.ts).as_secs_f64());
                        }
                        if self.loop_messages.remove(&msg.id).is_none() {
                            panic!("User device received a loop message it did not send");
                        }
                    }
                    MessageBody::String(_) => {
                        if let Some(ref metrics) = self.metrics {
                            metrics
                                .messages_terminated
                                .get_or_create(&MessageTerminatedLabels {
                                    by: self.name.clone(),
                                    r#type: MessageBodyType::String,
                                })
                                .inc();
                            metrics
                                .message_latency
                                .get_or_create(&MessageLatencyLabels {
                                    r#type: MessageBodyType::String,
                                })
                                .observe(cx.time().duration_since(msg.ts).as_secs_f64());
                        }
                    }
                    MessageBody::Drop => {
                        if let Some(ref metrics) = self.metrics {
                            metrics
                                .messages_terminated
                                .get_or_create(&MessageTerminatedLabels {
                                    by: self.name.clone(),
                                    r#type: MessageBodyType::Drop,
                                })
                                .inc();
                            metrics
                                .message_latency
                                .get_or_create(&MessageLatencyLabels {
                                    r#type: MessageBodyType::Drop,
                                })
                                .observe(cx.time().duration_since(msg.ts).as_secs_f64());
                        }
                    }
                };
            }
        }
    }

    // pub async fn bootstrap(&mut self, _: ()) {
    //     if let Some(root) = self.root_req.send(()).await.next() {
    //         let n = root.num_leaves();
    //         let die = match Uniform::new(0, n) {
    //             Ok(die) => die,
    //             Err(e) => panic!("{}", e),
    //         };
    //         let idxs: Vec<usize> = (0..5)
    //             .map(|_| die.sample(&mut self.rng).try_into().unwrap())
    //             .collect();
    //         if let Some(entries) = self.entries_req.send((root, idxs)).await.next() {
    //             self.mix_directory = entries;
    //         }
    //     }
    // }

    pub async fn lambda_p(&mut self, lambda: f64) {
        self.disable_lambda_p = lambda == 0.0;
        self.lambda_p.send(lambda).await;
    }

    pub async fn lambda_l(&mut self, lambda: f64) {
        self.lambda_l.send(lambda).await;
    }

    pub async fn lambda_d(&mut self, lambda: f64) {
        self.lambda_d.send(lambda).await;
    }

    pub async fn lambda_e(&mut self, lambda: f64) {
        self.lambda_e.send(lambda).await;
    }

    pub async fn lambda_mu(&mut self, lambda: f64) {
        self.lambda_mu = lambda;
    }

    async fn get_directory_root(&mut self) -> &RootHash<Sha256> {
        match self.directory_root {
            Some(ref directory_root) => directory_root,
            None => {
                self.directory_root = self.root_req.send(()).await.next();
                if let Some(ref directory_root) = self.directory_root {
                    directory_root
                } else {
                    panic!("User device requested directory root from transport but received none");
                }
            }
        }
    }

    async fn message_out(&mut self, msg: UnprocessedMessage, cx: &mut Context<Self>) {
        let directory_root = self.get_directory_root().await.clone();

        // Convert the UnprocessedMessage into a Message and send it
        match msg {
            // Add a send chain that ends at intended recipient
            UnprocessedMessage::User(msg) => {
                let mut mixes: VecDeque<_> = self
                    .mix_directory
                    .iter()
                    .filter(|entry| entry.id != msg.to)
                    .collect::<Vec<&DirectoryEntry>>()
                    .choose_multiple(&mut self.rng, self.chain_length)
                    .map(|entry| {
                        if self.lambda_mu == 0.0 {
                            (entry.id.clone(), 0.0)
                        } else {
                            match Exp::new(self.lambda_mu) {
                                Ok(dist) => (entry.id.clone(), dist.sample(&mut self.rng)),
                                Err(e) => panic!("{}", e),
                            }
                        }
                    })
                    .collect();
                let to = match (mixes.pop_front(), self.chain_length) {
                    (Some((to, interval)), _) => {
                        mixes.push_back((msg.to, interval));
                        to
                    }
                    (None, 0) => msg.to,
                    _ => {
                        panic!(
                            "Tried to generate mixes for a user message but the mix vector was empty"
                        );
                    }
                };
                if let Some(ref metrics) = self.metrics {
                    metrics
                        .messages_initiated
                        .get_or_create(&MessageInitiatedLabels {
                            from: self.name.clone(),
                            r#type: MessageBodyType::String,
                        })
                        .inc();
                }
                self.message_out
                    .send(Message {
                        id: msg.id,
                        ts: msg.ts,
                        to,
                        directory_root,
                        body: MessageBody::String(msg.body),
                        chain: mixes,
                    })
                    .await;
            }
            // Add a send chain that ends back at this user
            UnprocessedMessage::Loop(msg) => {
                let mut mixes: VecDeque<_> = self
                    .mix_directory
                    .iter()
                    .collect::<Vec<&DirectoryEntry>>()
                    .choose_multiple(&mut self.rng, self.chain_length)
                    .map(|entry| {
                        if self.lambda_mu == 0.0 {
                            (entry.id.clone(), 0.0)
                        } else {
                            match Exp::new(self.lambda_mu) {
                                Ok(dist) => (entry.id.clone(), dist.sample(&mut self.rng)),
                                Err(e) => panic!("{}", e),
                            }
                        }
                    })
                    .collect();
                let to = match (mixes.pop_front(), self.chain_length) {
                    (Some((to, interval)), _) => {
                        mixes.push_back((self.name.clone(), interval));
                        to
                    }
                    (None, 0) => self.name.clone(),
                    _ => {
                        panic!(
                            "Tried to generate mixes for a loop message but the mix vector was empty"
                        );
                    }
                };
                self.loop_messages.insert(msg.id.clone(), cx.time());
                if let Some(ref metrics) = self.metrics {
                    metrics
                        .messages_initiated
                        .get_or_create(&MessageInitiatedLabels {
                            from: self.name.clone(),
                            r#type: MessageBodyType::Loop,
                        })
                        .inc();
                }
                self.message_out
                    .send(Message {
                        id: msg.id,
                        ts: msg.ts,
                        to,
                        directory_root,
                        body: MessageBody::Loop,
                        chain: mixes,
                    })
                    .await;
            }
            // Add a send chain that ends at a random user
            UnprocessedMessage::Drop(msg) => {
                let mut mixes: VecDeque<_> = self
                    .mix_directory
                    .iter()
                    .collect::<Vec<&DirectoryEntry>>()
                    .choose_multiple(&mut self.rng, self.chain_length + 1)
                    .map(|entry| {
                        if self.lambda_mu == 0.0 {
                            (entry.id.clone(), 0.0)
                        } else {
                            match Exp::new(self.lambda_mu) {
                                Ok(dist) => (entry.id.clone(), dist.sample(&mut self.rng)),
                                Err(e) => panic!("{}", e),
                            }
                        }
                    })
                    .collect();
                if let Some((to, _)) = mixes.pop_front() {
                    if let Some(ref metrics) = self.metrics {
                        metrics
                            .messages_initiated
                            .get_or_create(&MessageInitiatedLabels {
                                from: self.name.clone(),
                                r#type: MessageBodyType::Drop,
                            })
                            .inc();
                    }
                    self.message_out
                        .send(Message {
                            id: msg.id,
                            ts: msg.ts,
                            to,
                            directory_root,
                            body: MessageBody::Drop,
                            chain: mixes,
                        })
                        .await;
                } else {
                    panic!(
                        "Tried to generate mixes for a drop message but the mix vector was empty"
                    );
                }
            }
            // Add a send chain that ends at a random user
            UnprocessedMessage::EntryRequest(msg) => {
                let mut mixes: VecDeque<_> = self
                    .mix_directory
                    .iter()
                    .collect::<Vec<&DirectoryEntry>>()
                    .choose_multiple(&mut self.rng, self.chain_length + 1)
                    .map(|entry| {
                        if self.lambda_mu == 0.0 {
                            (entry.id.clone(), 0.0)
                        } else {
                            match Exp::new(self.lambda_mu) {
                                Ok(dist) => (entry.id.clone(), dist.sample(&mut self.rng)),
                                Err(e) => panic!("{}", e),
                            }
                        }
                    })
                    .collect();
                let mut surb: VecDeque<_> = self
                    .mix_directory
                    .iter()
                    .collect::<Vec<&DirectoryEntry>>()
                    .choose_multiple(&mut self.rng, self.chain_length)
                    .map(|entry| {
                        if self.lambda_mu == 0.0 {
                            (entry.id.clone(), 0.0)
                        } else {
                            match Exp::new(self.lambda_mu) {
                                Ok(dist) => (entry.id.clone(), dist.sample(&mut self.rng)),
                                Err(e) => panic!("{}", e),
                            }
                        }
                    })
                    .collect();
                let self_entry = if self.lambda_mu == 0.0 {
                    (self.name.clone(), 0.0)
                } else {
                    match Exp::new(self.lambda_mu) {
                        Ok(dist) => (self.name.clone(), dist.sample(&mut self.rng)),
                        Err(e) => panic!("{}", e),
                    }
                };
                surb.push_back(self_entry);
                let n = directory_root.num_leaves();
                let die = match Uniform::new(0, n) {
                    Ok(die) => die,
                    Err(e) => panic!("{}", e),
                };
                let idxs = if 5 < n {
                    let mut taken_idxs = HashSet::<usize>::new();
                    (0..5)
                        .map(|_| {
                            let mut sample: usize = die.sample(&mut self.rng).try_into().unwrap();
                            while taken_idxs.contains(&sample) {
                                sample = die.sample(&mut self.rng).try_into().unwrap();
                            }
                            taken_idxs.insert(sample);
                            sample
                        })
                        .collect()
                } else {
                    let mut v: Vec<usize> = (0..n as usize).collect();
                    v.shuffle(&mut self.rng);
                    v
                };
                if let Some((to, _)) = mixes.pop_front() {
                    if let Some(ref metrics) = self.metrics {
                        metrics
                            .messages_initiated
                            .get_or_create(&MessageInitiatedLabels {
                                from: self.name.clone(),
                                r#type: MessageBodyType::EntryRequest,
                            })
                            .inc();
                    }
                    self.message_out
                        .send(Message {
                            id: msg.id,
                            ts: msg.ts,
                            to,
                            directory_root,
                            body: MessageBody::EntryRequest { idxs, surb },
                            chain: mixes,
                        })
                        .await;
                } else {
                    panic!(
                        "Tried to generate mixes for an entry request message but the mix vector was empty"
                    );
                }
            }
            // Forward directly through
            UnprocessedMessage::EntryResponse(msg) => {
                if let Some(ref metrics) = self.metrics {
                    metrics
                        .messages_initiated
                        .get_or_create(&MessageInitiatedLabels {
                            from: self.name.clone(),
                            r#type: MessageBodyType::EntryResponse,
                        })
                        .inc();
                }
                let msg = Message {
                    id: msg.id,
                    ts: msg.ts,
                    to: msg.to,
                    directory_root,
                    body: MessageBody::EntryResponse {
                        entries: msg.entries,
                    },
                    chain: msg.chain,
                };
                self.message_out.send(msg).await;
            }
            // Forward directly through
            UnprocessedMessage::Mix(msg) => {
                if let Some(ref metrics) = self.metrics {
                    metrics
                        .messages_forwarded
                        .get_or_create(&MessageForwardedLabels {
                            r#type: (&msg.body).into(),
                        })
                        .inc();
                }
                self.message_out.send(msg).await;
            }
        };
    }
}

impl Model for UserDevice {}
