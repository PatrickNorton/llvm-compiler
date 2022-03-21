use std::fmt::Display;
use std::hash::Hash;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use indexmap::IndexSet;

use crate::converter::constant::LangConstant;
use crate::converter::function::Function;
use crate::util::usize_to_bytes;

use super::BytecodeType;

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
}

impl BytecodeType for LocationBytecode {
    const SIZE: usize = 4;

    fn write_str(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _functions: &[&Function],
    ) -> std::fmt::Result {
        Display::fmt(&self.value.get_value(), f)
    }

    fn assemble(&self, buffer: &mut Vec<u8>, _constants: &IndexSet<LangConstant>) {
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
