use crate::parser::func_def::FunctionDefinitionNode;
use crate::parser::line_info::LineInfo;

use super::bytecode::Bytecode;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::error::CompilerException;
use super::fn_def::FunctionDefConverter;
use super::fn_info::FunctionInfo;
use super::function::Function;
use super::global_info::GlobalCompilerInfo;
use super::CompileResult;

pub fn convert_test_fn(
    info: &mut CompilerInfo,
    node: &FunctionDefinitionNode,
) -> CompileResult<()> {
    if info.global_info().is_test() {
        let constant = FunctionDefConverter::convert_with_constant(info, node)?;
        let fn_info = info.fn_info(constant.get_name()).unwrap();
        if !fn_info.matches(&[]) {
            Err(
                CompilerException::of("Test functions must have an empty argument list", node)
                    .into(),
            )
        } else {
            info.global_info().add_test_function(constant);
            Ok(())
        }
    } else {
        Ok(())
    }
}

pub fn convert_test_start(info: &mut GlobalCompilerInfo) -> u16 {
    let test_constant = info.global_builtins().unwrap().test_const().clone();
    let test_functions = info.get_test_functions();
    let mut bytes = BytecodeList::new();
    bytes.add(Bytecode::LoadConst(test_constant.into()));
    for constant in test_functions {
        bytes.add(Bytecode::LoadConst(constant.clone().into()));
    }
    bytes.add(Bytecode::CallTos((test_functions.len() as u16).into()));
    let fn_info = FunctionInfo::named("__default__$test");
    let function = Function::new(LineInfo::empty(), fn_info, bytes);
    info.add_function(function)
}
