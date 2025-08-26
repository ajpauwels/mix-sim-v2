#[derive(Clone, Debug)]
pub struct DirectoryEntry {
    pub id: String,
}

impl AsRef<[u8]> for DirectoryEntry {
    fn as_ref(&self) -> &[u8] {
        self.id.as_bytes()
    }
}
