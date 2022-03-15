use derive_new::new;

use crate::parser::line_info::{LineInfo, Lined};

use super::bytecode_list::BytecodeList;
use super::fn_info::FunctionInfo;

#[derive(Debug, new)]
pub struct Function {
    line_info: LineInfo,
    info: FunctionInfo,
    bytes: BytecodeList,
    #[new(default)]
    max: u16,
}

impl Function {
    pub fn get_name(&self) -> &str {
        self.info.get_name()
    }

    pub fn get_info(&self) -> &FunctionInfo {
        &self.info
    }

    pub fn get_bytes(&self) -> &BytecodeList {
        &self.bytes
    }

    pub fn is_generator(&self) -> bool {
        self.info.is_generator()
    }

    pub fn get_max(&self) -> u16 {
        self.max
    }

    pub fn set_max(&mut self, max: u16) {
        self.max = max
    }

    pub fn set_bytes(&mut self, bytes: BytecodeList) {
        assert!(self.bytes.is_empty());
        self.bytes = bytes;
    }
}

impl Lined for Function {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}
