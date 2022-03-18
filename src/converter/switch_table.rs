use std::collections::HashMap;
use std::io::Write;

use derive_new::new;
use num::BigInt;

use crate::util::string_escape::escape;
use crate::util::{usize_to_bytes, U32_BYTES};

use super::bytecode::Label;
use super::constant::{BigintConstant, CharConstant, StringConstant};

#[derive(Debug)]
pub enum SwitchTable {
    Big(BigSwitchTable),
    Char(CharSwitchTable),
    Compact(CompactSwitchTable),
    String(StringSwitchTable),
}

#[derive(Debug, new)]
pub struct BigSwitchTable {
    values: HashMap<BigInt, Label>,
    default_val: Label,
}

#[derive(Debug, new)]
pub struct CharSwitchTable {
    values: HashMap<char, Label>,
    default_val: Label,
}

#[derive(Debug, new)]
pub struct CompactSwitchTable {
    values: Vec<Label>,
    default_val: Label,
}

#[derive(Debug, new)]
pub struct StringSwitchTable {
    values: HashMap<String, Label>,
    default_val: Label,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u8)]
enum TableBytes {
    Compact = 0,
    Big = 1,
    String = 2,
    Char = 3,
}

impl SwitchTable {
    /// Converts the table to its byte representation.
    ///
    /// The representation of a switch table is a [byte](u8) representing the
    /// type of switch table, followed by table-specific bytes.
    ///
    /// For table-specific information, see:
    /// * [`BigSwitchTable::to_bytes`](BigSwitchTable::to_bytes)
    /// * [`CharSwitchTable::to_bytes`](CharSwitchTable::to_bytes)
    /// * [`CompactSwitchTable::to_bytes`](CompactSwitchTable::to_bytes)
    /// * [`StringSwitchTable::to_bytes`](StringSwitchTable::to_bytes)
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            SwitchTable::Big(b) => b.to_bytes(),
            SwitchTable::Char(c) => c.to_bytes(),
            SwitchTable::Compact(c) => c.to_bytes(),
            SwitchTable::String(s) => s.to_bytes(),
        }
    }

    /// Write the text disassembly of the table to the given output stream.
    pub fn disassemble_to<W: Write>(&self, stream: &mut W) -> std::io::Result<()> {
        match self {
            SwitchTable::Big(b) => b.disassemble_to(stream),
            SwitchTable::Char(c) => c.disassemble_to(stream),
            SwitchTable::Compact(c) => c.disassemble_to(stream),
            SwitchTable::String(s) => s.disassemble_to(stream),
        }
    }
}

impl BigSwitchTable {
    /// Converts the table into its byte representation.
    ///
    /// The representation is as follows:
    /// ```text
    /// [byte] 1
    /// The number of values
    /// For each value:
    ///     The number
    ///     The index to jump to
    /// The default index
    /// ```
    ///
    /// # See also:
    /// [`BigintConstant::convert_bigint`](BigintConstant::convert_bigint)
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![TableBytes::Big as u8];
        bytes.extend(usize_to_bytes(self.values.len()));
        for (key, val) in &self.values {
            bytes.extend(BigintConstant::convert_bigint(key));
            bytes.extend(usize_to_bytes(val.get_value()));
        }
        bytes.extend(usize_to_bytes(self.default_val.get_value()));
        bytes
    }

    pub fn disassemble_to<W: Write>(&self, stream: &mut W) -> std::io::Result<()> {
        for (val, label) in &self.values {
            writeln!(stream, "{}: {}", val, label.get_value())?;
        }
        writeln!(stream, "default: {}", self.default_val.get_value())
    }
}

impl CharSwitchTable {
    /// Converts the table into its byte representation.
    ///
    /// The representation is as follows:
    /// ```text
    /// [byte] 1
    /// The number of values
    /// For each value:
    ///     The character
    ///     The index to jump to
    /// The default index
    /// ```
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(self.byte_len());
        bytes.push(TableBytes::Char as u8);
        bytes.extend(usize_to_bytes(self.values.len()));
        for (&key, val) in &self.values {
            bytes.extend((key as u32).to_be_bytes());
            bytes.extend(usize_to_bytes(val.get_value()));
        }
        bytes.extend(usize_to_bytes(self.default_val.get_value()));
        bytes
    }

    /// The expected length in bytes of [`Self::to_bytes`](Self::to_bytes)
    fn byte_len(&self) -> usize {
        // Table type + value length + (key + label) * self.values + default
        1 + U32_BYTES + 2 * U32_BYTES * self.values.len() + U32_BYTES
    }

    pub fn disassemble_to<W: Write>(&self, stream: &mut W) -> std::io::Result<()> {
        for (&val, label) in &self.values {
            writeln!(stream, "{}: {}", CharConstant::name(val), label.get_value())?;
        }
        writeln!(stream, "default: {}", self.default_val.get_value())
    }
}

impl CompactSwitchTable {
    /// Converts the table into its byte representation.
    ///
    /// ```text
    /// [byte] 0
    /// The number of values
    /// For each i < max:
    ///     The label associated with i
    /// The default index
    /// ```
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(self.byte_len());
        bytes.push(TableBytes::Compact as u8);
        bytes.extend(usize_to_bytes(self.values.len()));
        for val in &self.values {
            bytes.extend(usize_to_bytes(val.get_value()));
        }
        bytes.extend(usize_to_bytes(self.default_val.get_value()));
        bytes
    }

    /// The expected length in bytes of [`Self::to_bytes`](Self::to_bytes)
    fn byte_len(&self) -> usize {
        // Table type + value length + label * self.values + default
        1 + U32_BYTES + U32_BYTES * self.values.len() + U32_BYTES
    }

    pub fn disassemble_to<W: Write>(&self, stream: &mut W) -> std::io::Result<()> {
        for (i, label) in self.values.iter().enumerate() {
            writeln!(stream, "{}: {}", i, label.get_value())?;
        }
        writeln!(stream, "default: {}", self.default_val.get_value())
    }
}

impl StringSwitchTable {
    /// Converts the table into its byte representation.
    ///
    /// The representation is as follows:
    /// ```text
    /// [byte] 2
    /// The number of values
    /// For each value:
    ///     The string value
    ///     The associated label
    /// The default label
    /// ```
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = vec![TableBytes::String as u8];
        bytes.extend(usize_to_bytes(self.values.len()));
        for (key, val) in &self.values {
            bytes.extend(StringConstant::str_bytes(key));
            bytes.extend(usize_to_bytes(val.get_value()));
        }
        bytes.extend(usize_to_bytes(self.default_val.get_value()));
        bytes
    }

    pub fn disassemble_to<W: Write>(&self, stream: &mut W) -> std::io::Result<()> {
        for (val, label) in &self.values {
            writeln!(stream, "{}: {}", escape(val), label.get_value())?;
        }
        writeln!(stream, "default: {}", self.default_val.get_value())
    }
}

macro_rules! impl_from {
    ($variant:ident, $value:ty) => {
        impl From<$value> for SwitchTable {
            fn from(x: $value) -> Self {
                Self::$variant(x)
            }
        }
    };
}

impl_from!(Big, BigSwitchTable);
impl_from!(Char, CharSwitchTable);
impl_from!(Compact, CompactSwitchTable);
impl_from!(String, StringSwitchTable);