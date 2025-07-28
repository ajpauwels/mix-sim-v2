use sim::{
    input_modeling::ContinuousRandomVariable,
    models::{Generator, Model, Processor, Storage},
    simulator::{Connector, Simulation},
};

mod drop_sender;

fn main() {
    let user = Model::new(
        "user".to_owned(),
        Box::new(Generator::new(
            ContinuousRandomVariable::Uniform {
                min: 1.0,
                max: 10.0,
            },
            None,
            "message".to_owned(),
            false,
            None,
        )),
    );
    let looper = Model::new(
        "looper".to_owned(),
        Box::new(Generator::new(
            ContinuousRandomVariable::Exp { lambda: 2.0 },
            None,
            "message".to_owned(),
            false,
            None,
        )),
    );
    let sender_storer = Model::new(
        "sender_storer".to_owned(),
        Box::new(Storage::new(
            "store".to_owned(),
            "read".to_owned(),
            "stored".to_owned(),
            false,
        )),
    );

    let whatsapp = Model::new(
        "processor01".to_owned(),
        Box::new(Processor::new(
            ContinuousRandomVariable::Exp { lambda: 1.0 },
            None,
            "job".to_owned(),
            "processed".to_owned(),
            false,
            None,
        )),
    );
    let generator_processor = Connector::new(
        "connector01".to_owned(),
        "generator01".to_owned(),
        "processor01".to_owned(),
        "job".to_owned(),
        "job".to_owned(),
    );
    let processor_storer = Connector::new(
        "connector02".to_owned(),
        "processor01".to_owned(),
        "storer01".to_owned(),
        "processed".to_owned(),
        "store".to_owned(),
    );
    let models = vec![generator01, processor01, storer01];
    let connectors = vec![generator_processor, processor_storer];
    let mut simulation = Simulation::post(models, connectors);
    let records = simulation.step_n(100).unwrap();
    records.iter().for_each(|record| println!("{record:?}"));
}
