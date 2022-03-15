use super::type_obj::TypeObject;

#[derive(Debug)]
pub struct FunctionReturnInfo {
    levels: Vec<ReturnLevel>,
}

#[derive(Debug)]
struct ReturnLevel {
    returns: Vec<TypeObject>,
    is_generator: bool,
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

    pub fn add_fn_returns(&mut self, is_generator: bool, returns: Vec<TypeObject>) {
        self.levels.push(ReturnLevel {
            returns,
            is_generator,
        })
    }

    pub fn pop_fn_returns(&mut self) {
        self.levels.pop();
    }

    pub fn current_fn_returns(&self) -> &[TypeObject] {
        &self
            .levels
            .first()
            .expect("Should be in a function")
            .returns
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
