use std::fmt::Display;
use std::hash::Hash;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use crate::converter::file_writer::ConstantSet;
use crate::util::usize_to_bytes;

use super::{BytecodeFmt, BytecodeType};

#[derive(Debug, Clone)]
pub struct Label {
    position: Arc<AtomicUsize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocationBytecode {
    value: Label,
}

impl Label {
    pub fn new() -> Self {
        Self {
            position: Arc::new(AtomicUsize::new(usize::MAX)),
        }
    }

    pub fn get_value(&self) -> usize {
        self.position.load(Ordering::Relaxed)
    }

    pub fn set_value(&self, pos: usize) {
        self.position.store(pos, Ordering::Relaxed)
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::new()
    }
}

impl LocationBytecode {
    pub const fn new(value: Label) -> Self {
        Self { value }
    }

    pub fn get_label(&self) -> &Label {
        &self.value
    }
}

impl BytecodeType for LocationBytecode {
    const SIZE: usize = 4;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _info: BytecodeFmt<'_>,
    ) -> std::fmt::Result {
        Display::fmt(&self.value.get_value(), f)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &ConstantSet) {
        buffer.extend(usize_to_bytes(self.value.get_value()))
    }
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.position, &other.position)
    }
}

impl Eq for Label {}

impl Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.position).hash(state)
    }
}

impl From<Label> for LocationBytecode {
    fn from(x: Label) -> Self {
        Self::new(x)
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexSet;

    use crate::converter::bytecode::BytecodeType;

    use super::{Label, LocationBytecode};

    #[test]
    fn assemble_location() {
        let values = &[
            0u32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 1000, 9999, 10000, 12345, 65535,
        ];
        for &value in values {
            let mut buf = Vec::new();
            let label = Label::new();
            label.set_value(value as usize);
            LocationBytecode::new(label).assemble(&mut buf, &IndexSet::new().into());
            assert_eq!(buf.len(), LocationBytecode::SIZE);
            assert_eq!(buf, &value.to_be_bytes());
        }
    }
}
