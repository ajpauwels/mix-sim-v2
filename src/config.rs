use std::{error::Error, fmt::Display, time::Duration};

use config::{Config as ExternalConfig, ConfigError as ExternalConfigError};
use duration_str::deserialize_option_duration;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Clone, Default)]
pub struct Topology {
    #[serde(default)]
    pub use_broadcast_server_output: bool,
}

#[derive(Deserialize, Serialize, Clone, Default)]
pub struct Simulation {
    #[serde(default, deserialize_with = "deserialize_option_duration")]
    pub duration: Option<Duration>,
    #[serde(default)]
    pub users: Users,
}

#[derive(Deserialize, Serialize, Clone, Default)]
pub struct Users {
    #[serde(default)]
    pub count: u64,
    #[serde(default)]
    pub user_directory_size: usize,
    #[serde(default)]
    pub mix_directory_size: usize,
    #[serde(default)]
    pub chain_length: usize,
    #[serde(default)]
    pub availability: f64,
    #[serde(default)]
    pub lambda_u: f64,
    #[serde(default)]
    pub lambda_p: f64,
    #[serde(default)]
    pub lambda_l: f64,
    #[serde(default)]
    pub lambda_d: f64,
    #[serde(default)]
    pub lambda_e: f64,
    #[serde(default)]
    pub lambda_mu: f64,
}

#[derive(Deserialize, Serialize, Clone, Default)]
pub struct Metrics {
    #[serde(default, deserialize_with = "deserialize_option_duration")]
    pub poll_interval: Option<Duration>,
    #[serde(default)]
    pub output_path: String,
}

#[derive(Deserialize, Serialize, Clone, Default)]
pub struct Config {
    #[serde(default)]
    pub seed: u64,
    #[serde(default)]
    pub topology: Topology,
    #[serde(default)]
    pub simulation: Simulation,
    #[serde(default)]
    pub metrics: Metrics,
}

#[derive(Debug)]
pub struct ConfigError {
    parent: ExternalConfigError,
}

impl Error for ConfigError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.parent)
    }
}

impl Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parent)
    }
}

pub fn load_config(path: &str, prefix: &str) -> Result<Config, ExternalConfigError> {
    let mut config_builder = ExternalConfig::builder();
    let paths = std::fs::read_dir(path)
        .map_err(|_| ExternalConfigError::Message(format!("Could not read directory at {path}")))?
        .filter_map(|result| result.ok())
        .map(|de| de.path());
    for path in paths {
        config_builder = match path.as_path().to_str() {
            Some(s) => {
                if s.ends_with(".yaml") {
                    config_builder.add_source(config::File::with_name(s))
                } else {
                    config_builder
                }
            }
            None => config_builder,
        };
    }
    config_builder
        .add_source(config::Environment::with_prefix(prefix).separator("_"))
        .build()?
        .try_deserialize()
}
