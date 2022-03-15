use itertools::process_results;

use crate::parser::fn_call::FunctionCallNode;
use crate::parser::name::NameNode;
use crate::parser::operator::{OperatorNode, OperatorTypeNode};
use crate::parser::test_node::TestNode;
use crate::util::version::{Version, CURRENT_VERSION};

use super::builtins::STABLE_FEATURES;
use super::compiler_info::CompilerInfo;
use super::error::{CompilerException, CompilerTodoError};
use super::CompileResult;

pub fn convert_cfg(info: &mut CompilerInfo, node: &FunctionCallNode) -> CompileResult<bool> {
    assert_eq!(node.get_variable().map(|x| x.get_name()), Some("cfg"));
    if let [param] = node.get_parameters() {
        let argument = param.get_argument();
        if !param.is_vararg() && param.get_variable().is_empty() {
            value_of(info, argument)
        } else {
            Err(
                CompilerException::of("'cfg' annotations do not support named arguments", node)
                    .into(),
            )
        }
    } else {
        Err(CompilerException::of("'cfg' annotations only support one value", node).into())
    }
}

pub fn value_of(info: &mut CompilerInfo, value: &TestNode) -> CompileResult<bool> {
    match value {
        TestNode::Name(NameNode::Variable(val)) => Ok(match val.get_name() {
            "true" => true,
            "false" => false,
            "test" => info.global_info().is_test(),
            "debug" => info.global_info().is_debug(),
            name => info.global_info().cfg_values().contains(name),
        }),
        TestNode::Name(NameNode::Function(func)) => match func.get_variable().unwrap().get_name() {
            "feature" => convert_feature(func),
            "version" => convert_version(func),
            "all" => convert_all(info, func),
            "any" => convert_any(info, func),
            name => Err(CompilerException::of(
                format!("Unknown cfg function predicate {}", name),
                func,
            )
            .into()),
        },
        TestNode::Operator(op) => convert_bool(info, op),
        _ => Err(CompilerTodoError::of("Cfg with non-variables not supported", value).into()),
    }
}

fn convert_bool(info: &mut CompilerInfo, value: &OperatorNode) -> CompileResult<bool> {
    let operands = value.get_operands();
    match value.get_operator() {
        OperatorTypeNode::BoolNot => {
            assert_eq!(operands.len(), 1);
            value_of(info, operands[0].get_argument()).map(|x| !x)
        }
        OperatorTypeNode::BoolAnd => process_results(
            operands.iter().map(|x| value_of(info, x.get_argument())),
            |mut x| x.all(|x| x),
        ),
        OperatorTypeNode::BoolOr => process_results(
            operands.iter().map(|x| value_of(info, x.get_argument())),
            |mut x| x.any(|x| x),
        ),
        OperatorTypeNode::BoolXor => {
            assert_eq!(operands.len(), 2);
            let a = value_of(info, operands[0].get_argument())?;
            let b = value_of(info, operands[1].get_argument())?;
            Ok(a ^ b)
        }
        _ => Err(CompilerException::of("Non-boolean operands not supported in cfg", value).into()),
    }
}

fn convert_feature(value: &FunctionCallNode) -> CompileResult<bool> {
    assert_eq!(value.get_variable().map(|x| x.get_name()), Some("feature"));
    let args = value.get_parameters();
    if args.len() != 1 {
        return Err(
            CompilerException::of("Invalid format for 'feature' cfg attribute", value).into(),
        );
    }
    let str_value = get_string(args[0].get_argument())?;
    Ok(STABLE_FEATURES.contains(&str_value))
}

fn convert_version(value: &FunctionCallNode) -> CompileResult<bool> {
    assert_eq!(value.get_variable().map(|x| x.get_name()), Some("version"));
    let args = value.get_parameters();
    if args.len() != 1 {
        return Err(
            CompilerException::of("Invalid format for 'version' cfg attribute", value).into(),
        );
    }
    let str_value = get_string(args[0].get_argument())?;
    match str_value.parse::<Version>() {
        Result::Err(err) => Err(CompilerException::of(
            format!("Invalid version format: \n{}", err),
            &args[0],
        )
        .into()),
        Result::Ok(version) => Ok(version <= CURRENT_VERSION),
    }
}

fn convert_all(info: &mut CompilerInfo, value: &FunctionCallNode) -> CompileResult<bool> {
    assert_eq!(value.get_variable().map(|x| x.get_name()), Some("all"));
    for arg in value.get_parameters() {
        if arg.is_vararg() || !arg.get_variable().is_empty() {
            return Err(CompilerException::of("Invalid format for cfg(all)", value).into());
        } else if !value_of(info, arg.get_argument())? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn convert_any(info: &mut CompilerInfo, value: &FunctionCallNode) -> CompileResult<bool> {
    assert_eq!(value.get_variable().map(|x| x.get_name()), Some("any"));
    for arg in value.get_parameters() {
        if arg.is_vararg() || !arg.get_variable().is_empty() {
            return Err(CompilerException::of("Invalid format for cfg(any)", value).into());
        } else if value_of(info, arg.get_argument())? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn get_string(value: &TestNode) -> CompileResult<&str> {
    match value {
        TestNode::String(s) => Ok(s.get_contents()),
        _ => Err(CompilerException::of("Expected string literal here", value).into()),
    }
}
