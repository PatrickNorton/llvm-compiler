use std::collections::HashSet;
use std::io::Write;

use indexmap::IndexSet;

use super::builtins::Builtins;
use super::bytecode::{Bytecode, Label};
use super::constant::LangConstant;
use super::file_writer::ConstantSet;
use super::function::Function;

/// A list of executable bytecode.
///
/// This is intentionally opaque, due to the fact that it contains labels.
///
/// # Examples
/// ```
/// let mut list = BytecodeList::new();
/// list.add(Bytecode::Null());
/// ```
#[derive(Debug, Clone)]
pub struct BytecodeList {
    values: Vec<BytecodeValue>,
}

#[derive(Debug, Clone)]
pub enum BytecodeValue {
    Bytecode(Bytecode),
    Label(Label),
}

/// An index into a [`BytecodeList`].
///
/// This is intentionally an opaque wrapper, and should only be created through
/// methods on [`BytecodeList`].
///
/// # Examples
/// ```
/// let list = BytecodeList::of(Bytecode::Null());
/// let first_index = list.enumerate().next().unwrap().0;
/// assert!(matches!(first_index, Bytecode::Null()));
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Index {
    value: usize,
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

    pub fn remove_all(&mut self, indices: HashSet<Index>) {
        let mut i = 0;
        self.values.retain(|_| {
            i += 1;
            !indices.contains(&Index::new(i - 1))
        })
    }

    pub fn remove_all_ranges(&mut self, indices: &[(Index, Option<Index>)]) {
        let mut i = 0;
        self.values.retain(|_| {
            let old_i = i;
            i += 1;
            !indices.iter().any(|(start, end)| match end {
                Option::Some(end) => old_i >= start.value && old_i < end.value,
                Option::None => old_i >= start.value,
            })
        })
    }

    pub fn disassemble_to<W: Write>(
        &self,
        functions: &[&Function],
        constants: &ConstantSet,
        builtins: &Builtins,
        stream: &mut W,
    ) -> std::io::Result<()> {
        self.set_labels();
        let mut i = 0;
        for value in &self.values {
            if let BytecodeValue::Bytecode(by) = value {
                writeln!(
                    stream,
                    "{:<7}{}",
                    i,
                    by.display(functions, constants, builtins)
                )?;
                i += by.size();
            }
        }
        stream.flush()
    }

    pub fn convert_to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        self.set_labels();
        let mut result = Vec::new();
        for value in &self.values {
            if let BytecodeValue::Bytecode(b) = value {
                result.extend(b.assemble(constants));
            }
        }
        result
    }

    pub fn find_constants(&self, constants: &mut IndexSet<LangConstant>) {
        for value in &self.values {
            if let BytecodeValue::Bytecode(by) = value {
                if let Option::Some(constant) = by.get_constant() {
                    Self::add_recursive_constants(constant.clone(), constants);
                }
            }
        }
    }

    fn add_recursive_constants(constant: LangConstant, constants: &mut IndexSet<LangConstant>) {
        for c in constant.constituent_values() {
            Self::add_recursive_constants(c.clone(), constants);
        }
        constants.insert(constant);
    }

    pub fn next_label(&self, start: Index) -> Option<Index> {
        self.values[start.value..]
            .iter()
            .position(|x| matches!(x, BytecodeValue::Label(_)))
            .map(|i| Index::new(i + start.value))
    }

    pub fn enumerate(&self) -> impl Iterator<Item = (Index, &Bytecode)> {
        self.values.iter().enumerate().filter_map(|(i, x)| match x {
            BytecodeValue::Label(_) => None,
            BytecodeValue::Bytecode(b) => Some((Index::new(i), b)),
        })
    }

    pub fn retain(&mut self, predicate: impl FnMut(&mut BytecodeValue) -> bool) {
        self.values.retain_mut(predicate)
    }

    pub(super) fn set_labels(&self) {
        let mut index = 0;
        for value in &self.values {
            match value {
                BytecodeValue::Bytecode(b) => index += b.size(),
                BytecodeValue::Label(lbl) => lbl.set_value(index),
            }
        }
    }
}

impl Index {
    fn new(value: usize) -> Self {
        Self { value }
    }

    pub fn next(&self) -> Self {
        Self::new(self.value + 1)
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

#[cfg(test)]
mod tests {
    use crate::converter::builtins::Builtins;
    use crate::converter::bytecode::{Bytecode, Label};
    use crate::converter::file_writer::ConstantSet;

    use super::BytecodeList;

    #[test]
    fn empty_disassemble() {
        let list = BytecodeList::new();
        let mut text = Vec::new();
        list.disassemble_to(
            &[],
            &ConstantSet::default(),
            &Builtins::test_builtins(),
            &mut text,
        )
        .unwrap();
        assert_eq!(text, &[]);
    }

    #[test]
    fn of_disassemble() {
        let list = BytecodeList::of(Bytecode::Nop());
        let mut text = Vec::new();
        list.disassemble_to(
            &[],
            &ConstantSet::default(),
            &Builtins::test_builtins(),
            &mut text,
        )
        .unwrap();
        assert_eq!(text, "0      NOP\n".as_bytes());
    }

    #[test]
    fn one_disassemble() {
        let mut list = BytecodeList::new();
        list.add(Bytecode::Nop());
        let mut text = Vec::new();
        list.disassemble_to(
            &[],
            &ConstantSet::default(),
            &Builtins::test_builtins(),
            &mut text,
        )
        .unwrap();
        assert_eq!(text, "0      NOP\n".as_bytes());
    }

    #[test]
    fn two_disassemble() {
        let list = bytecode_list!(Bytecode::Nop(), Bytecode::LoadNull());
        let mut text = Vec::new();
        list.disassemble_to(
            &[],
            &ConstantSet::default(),
            &Builtins::test_builtins(),
            &mut text,
        )
        .unwrap();
        assert_eq!(text, "0      NOP\n1      LOAD_NULL\n".as_bytes());
    }

    #[test]
    fn set_label() {
        let label = Label::new();
        let mut list = BytecodeList::new();
        list.add(Bytecode::Jump(label.clone().into()));
        list.add(Bytecode::LoadNull());
        list.add_label(label.clone());
        list.set_labels();
        assert_eq!(label.get_value(), 6);
    }

    #[test]
    fn label_disassemble() {
        let label = Label::new();
        let mut list = BytecodeList::new();
        list.add(Bytecode::Jump(label.clone().into()));
        list.add(Bytecode::LoadNull());
        list.add_label(label);
        let mut text = Vec::new();
        list.disassemble_to(
            &[],
            &ConstantSet::default(),
            &Builtins::test_builtins(),
            &mut text,
        )
        .unwrap();
        assert_eq!(
            text,
            "0      JUMP            6\n5      LOAD_NULL\n".as_bytes()
        );
    }
}
