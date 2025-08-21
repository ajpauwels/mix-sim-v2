use std::collections::VecDeque;

use nexosim::{
    model::{BuildContext, Model, ProtoModel},
    ports::Output,
    simulation::Mailbox,
    time::MonotonicTime,
};
use rand::SeedableRng;
use rand_chacha::ChaCha12Rng;

use crate::poisson_generator::PoissonGenerator;

pub struct ProtoPoissonQueue<
    T: Clone + Send + 'static,
    U: FnMut(&str, MonotonicTime, &mut ChaCha12Rng) -> T + Send + 'static,
> {
    pub message_out: Output<T>,
    lambda: f64,
    capacity: usize,
    generate: U,
    rng: ChaCha12Rng,
}

impl<
        T: Clone + Send + 'static,
        U: FnMut(&str, MonotonicTime, &mut ChaCha12Rng) -> T + Send + 'static,
    > ProtoPoissonQueue<T, U>
{
    /// Create a queue that accepts a type T and outputs it according
    /// to some given Poisson-distributed rate. If the queue is empty,
    /// a type T is generated used the given closured and outputted in
    /// order to maintain the constant rate.
    ///
    /// Accepts an optional source of rng for reproducible simulation.
    pub fn new(lambda: f64, capacity: usize, generate: U, rng: Option<ChaCha12Rng>) -> Self {
        Self {
            message_out: Default::default(),
            lambda,
            capacity,
            generate,
            rng: rng.unwrap_or(ChaCha12Rng::from_os_rng()),
        }
    }
}

impl<
        T: Clone + Send + 'static,
        U: FnMut(&str, MonotonicTime, &mut ChaCha12Rng) -> T + Send + 'static,
    > ProtoModel for ProtoPoissonQueue<T, U>
{
    type Model = PoissonQueue<T>;

    fn build(mut self, cx: &mut BuildContext<Self>) -> PoissonQueue<T> {
        let mut pq = PoissonQueue::new(self.capacity);

        // Message generator clocks queue sending, sending the
        // generated message if the queue is empty
        let mut generator = PoissonGenerator::new(
            self.lambda,
            self.generate,
            Some(ChaCha12Rng::from_rng(&mut self.rng)),
        );
        let generator_mbox = Mailbox::new();

        // Lambda input to queue should be forwarded to the generator
        pq.lambda.connect(PoissonGenerator::lambda, &generator_mbox);

        // Message outputted from generator should trigger the queue's
        // send message routine
        generator
            .message_out
            .connect(PoissonQueue::send_message, cx.address());

        // Move the prototype's output to the submodel
        pq.message_out = self.message_out;

        // Add generator to the simulation
        cx.add_submodel(generator, generator_mbox, "generator".to_owned());

        pq
    }
}

pub struct PoissonQueue<T: Clone + Send + 'static> {
    message_out: Output<T>,
    lambda: Output<f64>,
    capacity: usize,
    queue: VecDeque<T>,
}

impl<T: Clone + Send + 'static> PoissonQueue<T> {
    /// Create a sender that receives messages on a queue and forwards
    /// them out according to a Poisson-distributed rate; if the queue
    /// is empty, it sends drop messages to ensure the output rate
    /// remains constant.
    ///
    /// Accepts an optional source of rng for reproducible simulation.
    fn new(capacity: usize) -> Self {
        Self {
            message_out: Default::default(),
            lambda: Default::default(),
            capacity,
            queue: VecDeque::new(),
        }
    }

    /// Send rate (msg/s) -- input port.
    pub async fn lambda(&mut self, lambda: f64) {
        self.lambda.send(lambda).await;
    }

    /// Message to queue -- input port.
    pub fn message_in(&mut self, msg: T) {
        if self.queue.len() < self.capacity {
            self.queue.push_back(msg);
        }
    }

    /// Sends a message by either pulling one from the queue if there
    /// is one or sending the provided message. If a message is pulled
    /// from the queue, the provided message is dropped.
    async fn send_message(&mut self, msg: T) {
        self.message_out
            .send(self.queue.pop_front().unwrap_or(msg))
            .await;
    }
}

impl<T: Clone + Send + 'static> Model for PoissonQueue<T> {}
