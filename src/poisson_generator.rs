use std::time::Duration;

use nexosim::{
    model::{Context, InitializedModel, Model},
    ports::Output,
    simulation::ActionKey,
    time::MonotonicTime,
};
use rand::SeedableRng;
use rand_chacha::ChaCha12Rng;
use rand_distr::{Distribution, Exp};

pub struct PoissonGenerator<
    T: Clone + Send + 'static,
    U: FnMut(&str, MonotonicTime, &mut ChaCha12Rng) -> T + Send + 'static,
> {
    pub message_out: Output<T>,
    lambda: f64,
    generate: U,
    send_key: Option<ActionKey>,
    rng: ChaCha12Rng,
}

impl<
        T: Clone + Send + 'static,
        U: FnMut(&str, MonotonicTime, &mut ChaCha12Rng) -> T + Send + 'static,
    > PoissonGenerator<T, U>
{
    /// Create a generator that outputs a type T created by the given
    /// closure according to the given Poisson-distributed rate.
    ///
    /// Accepts an optional source of rng for reproducible simulation.
    pub fn new(lambda: f64, generate: U, rng: Option<ChaCha12Rng>) -> Self {
        Self {
            message_out: Default::default(),
            lambda,
            generate,
            send_key: None,
            rng: rng.unwrap_or(ChaCha12Rng::from_os_rng()),
        }
    }

    /// Send rate (msg/s) -- input port.
    pub fn lambda(&mut self, lambda: f64, cx: &mut Context<Self>) {
        // Restrict send rate to between 0 and 100 messages per unit
        // time
        let lambda = lambda.clamp(0.0, 100.0);

        // Do nothing if the requested rate is unchanged
        if lambda == self.lambda {
            return;
        }

        // Set the new rate internally
        self.lambda = lambda;

        // Cancel the next message send since we need to resample the
        // delay based on the new rate
        if let Some(k) = self.send_key.take() {
            k.cancel();
        }

        // If the new rate is non-zero, schedule the next message
        if self.lambda != 0.0 {
            match Exp::new(self.lambda) {
                Ok(dist) => {
                    let interval = Duration::from_secs_f64(dist.sample(&mut self.rng));
                    self.send_key = Some(
                        cx.schedule_keyed_event(interval, Self::send_message, ())
                            .unwrap(),
                    );
                }
                Err(e) => panic!("{}", e),
            }
        }
    }

    /// Sends an output and schedules the next one.
    fn send_message<'a>(
        &'a mut self,
        _: (),
        cx: &'a mut Context<Self>,
    ) -> impl Future<Output = ()> + Send + 'a {
        async move {
            // If a race condition causes the internal send rate to be
            // 0.0 at the exact same time a message is meant to be
            // sent, cancel sending the messsage
            if self.lambda == 0.0 {
                return;
            }

            // Generate and send the output
            let msg = (self.generate)(cx.name(), cx.time(), &mut self.rng);
            self.message_out.send(msg).await;

            // Schedule the next message by sampling a random interval
            // from the exponential distribution defined by the
            // internal lambda value
            match Exp::new(self.lambda) {
                Ok(dist) => {
                    let interval = Duration::from_secs_f64(dist.sample(&mut self.rng));
                    self.send_key = Some(
                        cx.schedule_keyed_event(interval, Self::send_message, ())
                            .unwrap(),
                    );
                }
                Err(e) => panic!("{}", e),
            }
        }
    }
}

impl<
        T: Clone + Send + 'static,
        U: FnMut(&str, MonotonicTime, &mut ChaCha12Rng) -> T + Send + 'static,
    > Model for PoissonGenerator<T, U>
{
    async fn init(mut self, cx: &mut Context<Self>) -> InitializedModel<Self> {
        let lambda = self.lambda;
        self.lambda = 0.0;
        self.lambda(lambda, cx);
        self.into()
    }
}
