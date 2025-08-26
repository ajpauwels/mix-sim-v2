use std::{collections::VecDeque, fmt::Display};

use ct_merkle::RootHash;
use nexosim::time::MonotonicTime;
use sha2::Sha256;

use crate::directory_entry::DirectoryEntry;

#[derive(Clone)]
pub struct UserMessage {
    pub id: String,
    pub ts: MonotonicTime,
    pub to: String,
    pub body: String,
}

#[derive(Clone)]
pub struct LoopMessage {
    pub id: String,
    pub ts: MonotonicTime,
}

#[derive(Clone)]
pub struct DropMessage {
    pub id: String,
    pub ts: MonotonicTime,
}

#[derive(Clone)]
pub struct EntryRequestMessage {
    pub id: String,
    pub ts: MonotonicTime,
}

#[derive(Clone)]
pub struct EntryResponseMessage {
    pub id: String,
    pub ts: MonotonicTime,
    pub to: String,
    pub entries: Vec<DirectoryEntry>,
    pub chain: VecDeque<(String, f64)>,
}

#[derive(Clone)]
pub enum UnprocessedMessage {
    User(UserMessage),
    Loop(LoopMessage),
    Drop(DropMessage),
    EntryRequest(EntryRequestMessage),
    EntryResponse(EntryResponseMessage),
    Mix(Message),
}

#[derive(Clone)]
pub struct Message {
    pub id: String,
    pub ts: MonotonicTime,
    pub to: String,
    pub directory_root: RootHash<Sha256>,
    pub body: MessageBody,
    pub chain: VecDeque<(String, f64)>,
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{:x}:{}",
            &self.id,
            &self.ts,
            &self.directory_root.as_bytes(),
            &self.body
        )
    }
}

#[derive(Clone)]
pub enum MessageBody {
    Drop,
    Loop,
    String(String),
    EntryRequest {
        idxs: Vec<usize>,
        surb: VecDeque<(String, f64)>,
    },
    EntryResponse {
        entries: Vec<DirectoryEntry>,
    },
}

impl Display for MessageBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageBody::Drop => write!(f, "Drop"),
            MessageBody::Loop => write!(f, "Loop"),
            MessageBody::String(s) => write!(f, "String({s})"),
            MessageBody::EntryRequest { idxs, surb: _ } => write!(
                f,
                "EntryRequest(idxs: [{}])",
                idxs.iter()
                    .fold("".to_owned(), |acc, idx| acc + &idx.to_string() + " ")
                    .trim()
            ),
            MessageBody::EntryResponse { entries } => write!(
                f,
                "EntryResponse(entries: [{}])",
                entries
                    .iter()
                    .fold("".to_owned(), |acc, entry| acc + &entry.id + " ")
                    .trim()
            ),
        }
    }
}
