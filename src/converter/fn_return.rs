use crate::parser::line_info::LineInfo;

use super::type_obj::TypeObject;

#[derive(Debug)]
pub struct FunctionReturnInfo {
    levels: Vec<ReturnLevel>,
}

#[derive(Debug)]
struct ReturnLevel {
    returns: Vec<TypeObject>,
    is_generator: bool,
    line_info: LineInfo,
    name: Option<String>,
}

impl FunctionReturnInfo {
    pub fn new() -> Self {
        Self { levels: Vec::new() }
    }

    pub fn is_generator(&self) -> bool {
        self.levels
            .last()
            .expect("Should be in a function")
            .is_generator
    }

    pub fn add_fn_returns(
        &mut self,
        is_generator: bool,
        returns: Vec<TypeObject>,
        line_info: LineInfo,
        name: Option<String>,
    ) {
        self.levels.push(ReturnLevel {
            returns,
            is_generator,
            line_info,
            name,
        })
    }

    pub fn pop_fn_returns(&mut self) {
        self.levels.pop();
    }

    pub fn current_fn_returns(&self) -> &[TypeObject] {
        &self.levels.last().expect("Should be in a function").returns
    }

    pub fn current_fn_info(&self) -> &LineInfo {
        &self
            .levels
            .last()
            .expect("Should be in a function")
            .line_info
    }

    pub fn current_fn_name(&self) -> Option<&str> {
        self.levels
            .last()
            .expect("Should be in a function")
            .name
            .as_deref()
    }

    pub fn not_in_function(&self) -> bool {
        self.levels.is_empty()
    }
}

impl Default for FunctionReturnInfo {
    fn default() -> Self {
        Self::new()
    }
}
