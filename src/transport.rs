use std::{
    collections::HashMap,
    hash::{BuildHasherDefault, DefaultHasher},
};

use ct_merkle::{mem_backed_tree::MemoryBackedTree, RootHash};
use nexosim::{
    model::{BuildContext, Model, ProtoModel},
    ports::{InputFn, Output},
    simulation::Address,
};
use sha2::Sha256;

use crate::{directory_entry::DirectoryEntry, message::Message};

pub struct ProtoTransport {
    broadcast_out: Option<Output<Message>>,
    unicast_outs: Option<HashMap<String, Output<Message>, BuildHasherDefault<DefaultHasher>>>,
    mt: MemoryBackedTree<Sha256, DirectoryEntry>,
}

impl ProtoTransport {
    pub fn new() -> Self {
        ProtoTransport {
            broadcast_out: None,
            unicast_outs: None,
            mt: MemoryBackedTree::<Sha256, DirectoryEntry>::new(),
        }
    }

    pub fn add_broadcast_out<M, F, S, A: Into<Address<M>>>(
        mut self,
        entry: &DirectoryEntry,
        input: F,
        mbox: A,
    ) -> Self
    where
        M: Model,
        F: for<'a> InputFn<'a, M, Message, S> + Clone,
        S: Send + 'static,
    {
        let id = entry.id.clone();
        if let Some(ref mut broadcast_out) = self.broadcast_out {
            broadcast_out.filter_map_connect(
                move |msg| {
                    if msg.to == id {
                        Some(msg.to_owned())
                    } else {
                        None
                    }
                },
                input,
                mbox,
            );
        } else {
            let mut broadcast_out: Output<Message> = Default::default();
            broadcast_out.filter_map_connect(
                move |msg| {
                    if msg.to == id {
                        Some(msg.to_owned())
                    } else {
                        None
                    }
                },
                input,
                mbox,
            );
            self.broadcast_out = Some(broadcast_out);
        }
        self.mt.push(entry.clone());
        self
    }

    pub fn add_unicast_out(mut self, entry: &DirectoryEntry, output: Output<Message>) -> Self {
        if let Some(ref mut unicast_outs) = self.unicast_outs {
            unicast_outs.insert(entry.id.clone(), output);
        } else {
            let mut unicast_outs = HashMap::with_hasher(BuildHasherDefault::new());
            unicast_outs.insert(entry.id.clone(), output);
            self.unicast_outs = Some(unicast_outs);
        }
        self.mt.push(entry.clone());
        self
    }
}

impl ProtoModel for ProtoTransport {
    type Model = Transport;

    fn build(self, _cx: &mut BuildContext<Self>) -> Self::Model {
        Transport::new(self.broadcast_out, self.unicast_outs, self.mt)
    }
}

pub struct Transport {
    broadcast_out: Option<Output<Message>>,
    unicast_outs: Option<HashMap<String, Output<Message>, BuildHasherDefault<DefaultHasher>>>,
    mt: MemoryBackedTree<Sha256, DirectoryEntry>,
}

impl Transport {
    pub fn new(
        broadcast_out: Option<Output<Message>>,
        unicast_outs: Option<HashMap<String, Output<Message>, BuildHasherDefault<DefaultHasher>>>,
        mt: MemoryBackedTree<Sha256, DirectoryEntry>,
    ) -> Self {
        Self {
            broadcast_out,
            unicast_outs,
            mt,
        }
    }

    pub async fn message_in(&mut self, msg: Message) {
        if let Some(ref mut unicast_outs) = self.unicast_outs {
            if let Some(output) = unicast_outs.get_mut(&msg.to) {
                output.send(msg.clone()).await;
            } else {
                panic!("Transport received a message addressed to a non-existent recipient");
            }
        }
        if let Some(ref mut broadcast_out) = self.broadcast_out {
            broadcast_out.send(msg).await;
        }
        if self.unicast_outs.is_none() && self.broadcast_out.is_none() {
            panic!("Transport had neither unicast nor broadcast outputs");
        }
    }

    pub async fn root_req(&mut self, _: ()) -> RootHash<Sha256> {
        self.mt.root()
    }

    pub async fn entries_req(
        &mut self,
        (root, idxs): (RootHash<Sha256>, Vec<usize>),
    ) -> Vec<DirectoryEntry> {
        if root == self.mt.root() {
            let entries: Vec<DirectoryEntry> = idxs
                .iter()
                .filter_map(|idx| self.mt.get(*idx))
                .cloned()
                .collect();
            if entries.len() != idxs.len() {
                panic!("Transport received a request for entries with invalid indices");
            } else {
                entries
            }
        } else {
            panic!("Transport received a request for entries with non-matching root");
        }
    }
}

impl Model for Transport {}
