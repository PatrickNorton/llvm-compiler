use std::sync::Arc;

use crate::parser::test_node::TestNode;

use super::argument::DefaultValue;
use super::compiler_info::CompilerInfo;
use super::CompileResult;

#[derive(Debug)]
pub struct DefaultHolder<'a> {
    values: Vec<(Arc<DefaultValue>, &'a TestNode)>,
}

impl<'a> DefaultHolder<'a> {
    pub const fn new() -> Self {
        Self { values: Vec::new() }
    }

    pub fn add_argument(&mut self, value: Arc<DefaultValue>, node: &'a TestNode) {
        self.values.push((value, node));
    }

    pub fn compile(self, info: &mut CompilerInfo) -> CompileResult<()> {
        for (value, node) in self.values {
            value.compile(info, node)?;
        }
        Ok(())
    }
}
