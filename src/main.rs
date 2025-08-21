use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use config::{load_config, Config};
use directory_entry::DirectoryEntry;
use message::Message;
use nexosim::{
    ports::{EventSource, Output},
    simulation::{Mailbox, SimInit, SimulationError},
    time::MonotonicTime,
};
use prometheus::{MetricFamilies, MetricsServer, NoLabels, SimulationLambdaLabels};
use prometheus_client::registry::Registry;
use rand::{
    seq::{IndexedRandom, IteratorRandom},
    RngCore, SeedableRng,
};
use rand_chacha::{ChaCha12Rng, ChaCha20Rng};
use transport::{ProtoTransport, Transport};
use user_with_device::{ProtoUserWithDevice, UserWithDevice};

mod config;
mod directory_entry;
mod message;
mod poisson_generator;
mod poisson_queue;
mod prometheus;
mod transport;
mod user_device;
mod user_with_device;

#[tokio::main]
async fn main() -> Result<(), SimulationError> {
    // Get app config
    let config_path = "./config";
    let config_env_prefix = "APPCFG";
    let config = load_config(config_path, config_env_prefix).unwrap();

    // Turn on metrics if enabled
    // let (mf, _) = if config.metrics.enable {
    //     Some(prometheus::setup())
    // } else {
    //     None
    // }
    // .unzip();
    let (metric_registry, metric_families) = if config.metrics.poll_interval.is_some() {
        Some(prometheus::setup())
    } else {
        None
    }
    .unzip();

    // Run simulation
    simulation(config, metric_registry, metric_families.as_ref())
}

fn simulation(
    config: Config,
    metric_registry: Option<Registry>,
    metric_families: Option<&MetricFamilies>,
) -> Result<(), SimulationError> {
    // Deterministic seed-based rng
    let seed: u64 = config.seed;
    let mut rng = ChaCha20Rng::seed_from_u64(seed);

    let before_setup = Instant::now();

    // Simulation config
    let simulation_duration = config
        .simulation
        .duration
        .unwrap_or(Duration::from_secs(1200));

    // User config
    let user_count = config.simulation.users.count;
    let user_directory_size = config.simulation.users.user_directory_size;
    let mix_directory_size = config.simulation.users.mix_directory_size;
    let chain_length = config.simulation.users.chain_length;
    let lambda_u = config.simulation.users.lambda_u;
    let lambda_p = config.simulation.users.lambda_p;
    let lambda_l = config.simulation.users.lambda_l;
    let lambda_d = config.simulation.users.lambda_d;
    let lambda_e = config.simulation.users.lambda_e;
    let lambda_mu = config.simulation.users.lambda_mu;
    let user_entries: Vec<_> = (0..user_count)
        .map(|_| {
            let mut uuid_bytes = [0u8; 16];
            rng.fill_bytes(&mut uuid_bytes);
            DirectoryEntry {
                id: uuid::Builder::from_random_bytes(uuid_bytes)
                    .into_uuid()
                    .hyphenated()
                    .to_string(),
            }
        })
        .collect();

    // Create transport
    let mut transport = ProtoTransport::new();
    let transport_mbox = Mailbox::new();

    // Create simulation prototype
    let mut proto_sim = SimInit::new();

    // Create rng for generating directories
    let mut directory_rng = ChaCha12Rng::from_rng(&mut rng);

    // Create each user and add to the simulation
    for entry in user_entries.iter() {
        // Create a directory for the user (actual recipients, does
        // not include this user)
        let user_directory: Vec<_> = user_entries
            .iter()
            .filter(|user| entry.id != user.id)
            .choose_multiple(&mut directory_rng, user_directory_size)
            .into_iter()
            .cloned()
            .collect::<Vec<DirectoryEntry>>();

        // Create a directory for the user device (mix users, does
        // include this user)
        let mix_directory: Vec<_> = user_entries
            .choose_multiple(&mut directory_rng, mix_directory_size)
            .cloned()
            .collect();

        // Create a user with device
        let mut uwd = ProtoUserWithDevice::new(&entry.id, metric_families)
            .lambda_u(lambda_u)
            .lambda_p(lambda_p)
            .lambda_l(lambda_l)
            .lambda_d(lambda_d)
            .lambda_e(lambda_e)
            .lambda_mu(lambda_mu)
            .user_directory(user_directory)
            .mix_directory(mix_directory)
            .chain_length(chain_length)
            .rng(Some(ChaCha12Rng::from_rng(&mut rng)));
        let uwd_mbox = Mailbox::new();

        // Send the user device's output to transport
        uwd.message_out
            .connect(Transport::message_in, &transport_mbox);
        uwd.root_req.connect(Transport::root_req, &transport_mbox);
        uwd.entries_req
            .connect(Transport::entries_req, &transport_mbox);

        // Send transport's output to the user device
        if config.topology.use_broadcast_server_output {
            transport =
                transport.add_broadcast_out(entry, UserWithDevice::server_message_in, &uwd_mbox);
        } else {
            let mut user_device_input: Output<Message> = Default::default();
            user_device_input.connect(UserWithDevice::server_message_in, &uwd_mbox);
            transport = transport.add_unicast_out(entry, user_device_input);
        }

        // Add user with device to simulation
        proto_sim = proto_sim.add_model(uwd, uwd_mbox, &entry.id);
    }

    // Add transport to simulation
    proto_sim = proto_sim.add_model(transport, transport_mbox, "transport");

    // Create simulation
    let t0 = MonotonicTime::EPOCH;
    let (mut simu, _scheduler, metrics_server_address) =
        if let (Some(metric_registry), Some(metric_families), Some(poll_interval)) = (
            metric_registry,
            metric_families,
            config.metrics.poll_interval,
        ) {
            // Set the lambda gauges
            metric_families
                .simulation_lambdas
                .get_or_create(&SimulationLambdaLabels {
                    process: "u".to_owned(),
                })
                .set(lambda_u);
            metric_families
                .simulation_lambdas
                .get_or_create(&SimulationLambdaLabels {
                    process: "p".to_owned(),
                })
                .set(lambda_p);
            metric_families
                .simulation_lambdas
                .get_or_create(&SimulationLambdaLabels {
                    process: "l".to_owned(),
                })
                .set(lambda_l);
            metric_families
                .simulation_lambdas
                .get_or_create(&SimulationLambdaLabels {
                    process: "d".to_owned(),
                })
                .set(lambda_d);
            metric_families
                .simulation_lambdas
                .get_or_create(&SimulationLambdaLabels {
                    process: "e".to_owned(),
                })
                .set(lambda_e);
            metric_families
                .simulation_time
                .get_or_create(&NoLabels {})
                .set(simulation_duration.as_secs().try_into().unwrap());
            metric_families
                .simulation_user_count
                .get_or_create(&NoLabels {})
                .set(user_count.try_into().unwrap());

            let metrics_server = MetricsServer::new(metric_registry, config.metrics.output_path);
            let metrics_server_mbox = Mailbox::new();
            let metrics_server_address = metrics_server_mbox.address();
            let mut prometheus_requester = EventSource::<()>::new();
            prometheus_requester.connect(MetricsServer::get_metrics, &metrics_server_mbox);
            let metric_request_event =
                Arc::new(prometheus_requester).periodic_event(poll_interval, ());
            proto_sim = proto_sim.add_model(metrics_server, metrics_server_mbox, "metrics");
            let (simu, scheduler) = proto_sim.init(t0)?;
            scheduler
                .schedule(poll_interval, metric_request_event)
                .unwrap();
            (simu, scheduler, Some(metrics_server_address))
        } else {
            let (simu, scheduler) = proto_sim.init(t0)?;
            (simu, scheduler, None)
        };

    let mut t = t0;

    println!(
        "[SIMULATION] Setup time: {}s",
        before_setup.elapsed().as_secs_f64()
    );

    // Ensure simulation begins as expected
    assert_eq!(simu.time(), t);

    // Run simulation and acquire messages
    let before_sim = Instant::now();
    simu.step_until(simulation_duration)?;
    println!(
        "[SIMULATION] Simulation-time: {}s, real-time: {}s",
        simulation_duration.as_secs_f64(),
        before_sim.elapsed().as_secs_f64(),
    );
    t += simulation_duration;
    assert_eq!(simu.time(), t);

    // Close metrics file if metrics were enabled
    if let Some(address) = metrics_server_address {
        simu.process_event(MetricsServer::write_eof, (), address)
            .unwrap();
    }

    Ok(())
}
