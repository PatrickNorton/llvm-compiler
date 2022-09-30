use super::bytecode::Label;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::convertible::ConverterBase;
use super::diverge::DivergingInfo;
use super::{CompileBytes, CompileResult};

pub trait LoopConverter {
    const HAS_CONTINUE: bool;

    fn true_convert(&mut self, info: &mut CompilerInfo) -> CompileBytes;

    fn true_convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.true_convert(info).map(|x| (x, DivergingInfo::new()))
    }
}

#[derive(Debug)]
pub struct LoopManager {
    entries: Vec<LoopEntry>,
}

#[derive(Debug)]
struct LoopEntry {
    break_label: Label,
    continue_label: Option<Label>,
}

impl LoopManager {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn continue_label(&self) -> &Label {
        self.entries
            .last()
            .expect("Should be in a loop")
            .continue_label
            .as_ref()
            .expect("Should have continue")
    }

    pub fn break_label(&self, level: usize) -> &Label {
        &self.entries[self.entries.len() - level].break_label
    }

    fn enter_loop(&mut self, has_continue: bool) {
        let break_label = Label::new();
        let continue_label = has_continue.then(Label::new);
        self.entries.push(LoopEntry {
            break_label,
            continue_label,
        });
    }

    fn exit_loop(&mut self, bytes: &mut BytecodeList) {
        let entry = self.entries.pop().unwrap();
        bytes.add_label(entry.break_label);
    }
}

impl<T: LoopConverter> ConverterBase for T {
    // FIXME? Deal with true_convert() returning Err

    fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        info.loop_manager().enter_loop(Self::HAS_CONTINUE);
        info.add_stack_frame();
        let mut bytes = self.true_convert(info)?;
        info.loop_manager().exit_loop(&mut bytes);
        info.remove_stack_frame();
        Ok(bytes)
    }

    fn convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        info.loop_manager().enter_loop(Self::HAS_CONTINUE);
        info.add_stack_frame();
        let (mut bytes, mut div) = self.true_convert_with_return(info)?;
        info.loop_manager().exit_loop(&mut bytes);
        info.remove_stack_frame();
        div.decrement_breaks();
        if Self::HAS_CONTINUE {
            div.clear_continue();
        }
        Ok((bytes, div))
    }
}

impl Default for LoopManager {
    fn default() -> Self {
        Self::new()
    }
}
