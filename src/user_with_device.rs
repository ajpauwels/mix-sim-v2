use ct_merkle::RootHash;
use nexosim::{
    model::{BuildContext, Context, Model, ProtoModel},
    ports::{Output, Requestor},
    simulation::Mailbox,
};
use rand::{seq::IndexedRandom, RngCore, SeedableRng};
use rand_chacha::ChaCha12Rng;
use sha2::Sha256;

use crate::{
    directory_entry::DirectoryEntry,
    message::{Message, UserMessage},
    poisson_generator::PoissonGenerator,
    prometheus::MetricFamilies,
    user_device::{ProtoUserDevice, UserDevice},
};

pub struct ProtoUserWithDevice<'a> {
    pub message_out: Output<Message>,
    pub user_out: Output<Message>,
    pub root_req: Requestor<(), RootHash<Sha256>>,
    pub entries_req: Requestor<(RootHash<Sha256>, Vec<usize>), Vec<DirectoryEntry>>,
    name: String,
    availability: Option<f64>,
    lambda_u: Option<f64>,
    lambda_p: Option<f64>,
    lambda_l: Option<f64>,
    lambda_d: Option<f64>,
    lambda_e: Option<f64>,
    lambda_mu: Option<f64>,
    user_directory: Vec<DirectoryEntry>,
    mix_directory: Vec<DirectoryEntry>,
    chain_length: usize,
    rng: Option<ChaCha12Rng>,
    metrics: Option<&'a MetricFamilies>,
}

impl<'a> ProtoUserWithDevice<'a> {
    pub fn new(name: &str, mf: Option<&'a MetricFamilies>) -> Self {
        Self {
            message_out: Default::default(),
            user_out: Default::default(),
            root_req: Default::default(),
            entries_req: Default::default(),
            name: name.to_owned(),
            availability: None,
            lambda_u: None,
            lambda_p: None,
            lambda_l: None,
            lambda_d: None,
            lambda_e: None,
            lambda_mu: None,
            user_directory: Vec::new(),
            mix_directory: Vec::new(),
            chain_length: 0,
            rng: None,
            metrics: mf,
        }
    }

    pub fn availability(mut self, availability: f64) -> Self {
        self.availability = Some(availability);
        self
    }

    pub fn lambda_u(mut self, lambda_u: f64) -> Self {
        self.lambda_u = Some(lambda_u);
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

    pub fn user_directory(mut self, user_directory: Vec<DirectoryEntry>) -> Self {
        self.user_directory = user_directory;
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

impl<'a> ProtoModel for ProtoUserWithDevice<'a> {
    type Model = UserWithDevice;

    fn build(mut self, cx: &mut BuildContext<Self>) -> Self::Model {
        // Create user with device
        let mut uwd = UserWithDevice::new();

        // Create the user sending messages to random destinations
        let mut user = PoissonGenerator::new(
            self.lambda_u.unwrap_or(0.0),
            move |name, ts, rng| {
                if let Some(to) = self.user_directory.choose(rng) {
                    let mut uuid_bytes = [0u8; 16];
                    rng.fill_bytes(&mut uuid_bytes);
                    let id = uuid::Builder::from_random_bytes(uuid_bytes)
                        .into_uuid()
                        .hyphenated()
                        .to_string();

                    UserMessage {
                        id,
                        ts,
                        to: to.id.clone(),
                        body: name.to_owned(),
                    }
                } else {
                    panic!("User tried to send a message but its user directory was empty");
                }
            },
            self.rng.as_mut().map(ChaCha12Rng::from_rng),
        );
        let user_mbox = Mailbox::new();

        // Create the user device
        let mut device = ProtoUserDevice::new(&self.name, self.metrics)
            .availability(self.availability.unwrap_or(1.0))
            .lambda_p(self.lambda_p.unwrap_or(0.0))
            .lambda_l(self.lambda_l.unwrap_or(0.0))
            .lambda_d(self.lambda_d.unwrap_or(0.0))
            .lambda_e(self.lambda_e.unwrap_or(0.0))
            .lambda_mu(self.lambda_mu.unwrap_or(0.0))
            .mix_directory(self.mix_directory)
            .chain_length(self.chain_length)
            .rng(self.rng.as_mut().map(ChaCha12Rng::from_rng));
        let device_mbox = Mailbox::new();

        // Send the user's output to the user device
        user.message_out
            .connect(UserDevice::user_message_in, &device_mbox);

        // Connect model inputs to submodels
        uwd.message_in
            .connect(UserDevice::server_message_in, &device_mbox);
        uwd.lambda_u.connect(PoissonGenerator::lambda, &user_mbox);
        uwd.lambda_p.connect(UserDevice::lambda_p, &device_mbox);
        uwd.lambda_l.connect(UserDevice::lambda_l, &device_mbox);
        uwd.lambda_d.connect(UserDevice::lambda_d, &device_mbox);
        uwd.lambda_e.connect(UserDevice::lambda_e, &device_mbox);
        uwd.lambda_mu.connect(UserDevice::lambda_mu, &device_mbox);

        // Move the prototype's outputs to the submodel
        device.message_out = self.message_out.clone();
        device.user_out = self.user_out.clone();
        device.root_req = self.root_req.clone();
        device.entries_req = self.entries_req.clone();
        uwd.message_out = self.message_out;
        uwd.user_out = self.user_out;
        uwd.root_req = self.root_req;
        uwd.entries_req = self.entries_req;

        // Add submodels to the simulation
        cx.add_submodel(user, user_mbox, "user".to_owned());
        cx.add_submodel(device, device_mbox, "device".to_owned());

        uwd
    }
}

pub struct UserWithDevice {
    // Output to internal user device
    message_in: Output<Message>,

    // Outputs to transport
    message_out: Output<Message>,
    user_out: Output<Message>,
    root_req: Requestor<(), RootHash<Sha256>>,
    entries_req: Requestor<(RootHash<Sha256>, Vec<usize>), Vec<DirectoryEntry>>,

    // Outputs to internal generators
    lambda_u: Output<f64>,
    lambda_p: Output<f64>,
    lambda_l: Output<f64>,
    lambda_d: Output<f64>,
    lambda_e: Output<f64>,
    lambda_mu: Output<f64>,
}

impl UserWithDevice {
    fn new() -> Self {
        Self {
            message_in: Default::default(),
            message_out: Default::default(),
            user_out: Default::default(),
            root_req: Default::default(),
            entries_req: Default::default(),
            lambda_u: Default::default(),
            lambda_p: Default::default(),
            lambda_l: Default::default(),
            lambda_d: Default::default(),
            lambda_e: Default::default(),
            lambda_mu: Default::default(),
        }
    }

    pub async fn server_message_in(&mut self, msg: Message, _cx: &mut Context<Self>) {
        self.message_in.send(msg).await;
    }

    // pub async fn lambda_u(&mut self, lambda: f64) {
    //     self.lambda_u.send(lambda).await;
    // }

    // pub async fn lambda_p(&mut self, lambda: f64) {
    //     self.lambda_p.send(lambda).await;
    // }

    // pub async fn lambda_l(&mut self, lambda: f64) {
    //     self.lambda_l.send(lambda).await;
    // }

    // pub async fn lambda_d(&mut self, lambda: f64) {
    //     self.lambda_d.send(lambda).await;
    // }

    // pub async fn lambda_e(&mut self, lambda: f64) {
    //     self.lambda_e.send(lambda).await;
    // }

    // pub async fn lambda_mu(&mut self, lambda: f64) {
    //     self.lambda_mu.send(lambda).await;
    // }
}

impl Model for UserWithDevice {}
