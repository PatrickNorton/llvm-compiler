use std::io::Write;

use super::bytecode::{Bytecode, Label};
use super::constant::LangConstant;
use super::function::Function;

#[derive(Debug, Clone)]
pub struct BytecodeList {
    values: Vec<BytecodeValue>,
}

#[derive(Debug, Clone)]
pub enum BytecodeValue {
    Bytecode(Bytecode),
    Label(Label),
}

impl BytecodeList {
    pub const fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            values: Vec::with_capacity(cap),
        }
    }

    pub fn of(bytecode: Bytecode) -> Self {
        Self {
            values: vec![BytecodeValue::Bytecode(bytecode)],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    #[doc(hidden)]
    pub fn from_values(values: Vec<BytecodeValue>) -> Self {
        Self { values }
    }

    pub fn add(&mut self, bytecode: Bytecode) {
        self.values.push(BytecodeValue::Bytecode(bytecode))
    }

    pub fn add_first(&mut self, bytecode: Bytecode) {
        self.values.insert(0, BytecodeValue::Bytecode(bytecode))
    }

    pub fn extend(&mut self, other: BytecodeList) {
        self.values.extend(other.values)
    }

    pub fn extend_with(&mut self, other: impl IntoIterator<Item = Bytecode>) {
        self.values
            .extend(other.into_iter().map(BytecodeValue::Bytecode))
    }

    pub fn add_label(&mut self, label: Label) {
        self.values.push(BytecodeValue::Label(label))
    }

    pub fn disassemble_to<W: Write>(
        &self,
        functions: &[&Function],
        stream: &mut W,
    ) -> std::io::Result<()> {
        self.set_labels();
        let mut i = 0;
        for value in &self.values {
            if let BytecodeValue::Bytecode(by) = value {
                writeln!(stream, "{:<7}{}", i, by.display(functions))?;
                i += by.size();
            }
        }
        stream.flush()
    }

    pub fn convert_to_bytes(&self) -> Vec<u8> {
        self.set_labels();
        let mut result = Vec::new();
        for value in &self.values {
            if let BytecodeValue::Bytecode(b) = value {
                result.extend(b.assemble());
            }
        }
        result
    }

    pub fn find_constants(&self, constants: &mut IndexSet<LangConstant>) {
        for value in &self.values {
            if let BytecodeValue::Bytecode(by) = value {
                if let Option::Some(constant) = by.get_constant() {
                    constants.insert(constant);
                }
            }
        }
    }

    fn set_labels(&self) {
        let mut index = 0;
        for value in &self.values {
            match value {
                BytecodeValue::Bytecode(b) => index += b.size(),
                BytecodeValue::Label(lbl) => lbl.set_value(index),
            }
        }
    }
}

macro_rules! bytecode_list {
    ($($x:expr),+ $(,)?) => {
        BytecodeList::from_values(
            vec![
                $($crate::converter::bytecode_list::BytecodeValue::Bytecode($x)),+
            ]
        )
    };
}

pub(super) use bytecode_list;
use indexmap::IndexSet;
