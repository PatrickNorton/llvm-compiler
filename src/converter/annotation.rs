use std::collections::HashSet;

use derive_new::new;
use num::ToPrimitive;

use crate::parser::annotation::AnnotatableRef;
use crate::parser::argument::ArgumentNode;
use crate::parser::definition::BaseClassRef;
use crate::parser::fn_call::FunctionCallNode;
use crate::parser::line_info::Lined;
use crate::parser::name::NameNode;
use crate::parser::operator_fn::OpFuncTypeNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;

use super::builtins::STABLE_FEATURES;
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::config::{self, convert_cfg};
use super::convertible::BaseConvertible;
use super::diverge::DivergingInfo;
use super::error::{CompilerException, CompilerInternalError, CompilerTodoError};
use super::permission::PermissionLevel;
use super::test_fn::convert_test_fn;
use super::warning::{self, WarningHolder, WarningType, WARNING_TYPES};
use super::CompileResult;

pub trait AnnotatableConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a>;

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)>;
}

#[derive(Debug, new)]
pub struct BuiltinInfo {
    pub name: String,
    pub index: Option<usize>,
    pub hidden: bool,
}

macro_rules! impl_annotatable {
    ($name:ident < $($exprs: tt),+ >) => {
        impl <$($exprs),+> $crate::converter::convertible::ConverterBase for $name<$($exprs),+> {
            fn convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
                self.convert_with_return(info).map(|x| x.0)
            }

            fn convert_with_return(
                &mut self,
                info: &mut CompilerInfo,
            ) -> CompileResult<(BytecodeList, DivergingInfo)> {
                $crate::converter::annotation::convert_annotatable(self, info)
            }
        }
    };
}

pub(super) use impl_annotatable;

// TODO? Make more of these statically-dispatched

pub fn convert_annotatable<'a, T>(
    converter: &mut T,
    info: &mut CompilerInfo,
) -> CompileResult<(BytecodeList, DivergingInfo)>
where
    T: AnnotatableConverter<'a>,
{
    let annotations = converter.get_annotatable();
    match annotations.get_annotations().len() {
        0 => converter.convert_without_annotations(info),
        1 => convert_name(converter, info, &annotations.get_annotations()[0]),
        x => Err(CompilerTodoError::of(
            format!("Multiple attributes on one statement (got {})", x),
            annotations,
        )
        .into()),
    }
}

fn convert_name<'a, T>(
    converter: &mut T,
    info: &mut CompilerInfo,
    name: &NameNode,
) -> CompileResult<(BytecodeList, DivergingInfo)>
where
    T: AnnotatableConverter<'a>,
{
    match name {
        NameNode::Variable(v) => convert_variable(converter, info, v),
        NameNode::Function(f) => convert_function(converter, info, f),
        _ => Err(CompilerTodoError::of("Other annotations", name).into()),
    }
}

fn convert_variable<'a, T>(
    converter: &mut T,
    info: &mut CompilerInfo,
    name: &VariableNode,
) -> CompileResult<(BytecodeList, DivergingInfo)>
where
    T: AnnotatableConverter<'a>,
{
    let node = converter.get_annotatable();
    match name.get_name() {
        x @ ("cfg" | "allow" | "deny" | "forbid") => {
            Err(CompilerException::of(format!("{} attributes require arguments", x), name).into())
        }
        "hot" | "cold" | "inline" => {
            if !node.is_definition() {
                return Err(CompilerException::of(
                    "Frequency hints may only be used in definitions",
                    name,
                )
                .into());
            }
            warning::warn(
                "Frequency hints do not do anything yet",
                WarningType::Todo,
                info,
                name,
            )?;
            converter.convert_without_annotations(info)
        }
        "test" => {
            if let AnnotatableRef::Function(f) = node {
                convert_test_fn(info, f)?;
                Ok((BytecodeList::new(), DivergingInfo::new()))
            } else {
                Err(CompilerException::with_note(
                    "Only functions may be used as tests",
                    "You may want $cfg(test)",
                    node,
                )
                .into())
            }
        }
        "nonExhaustive" => {
            if let AnnotatableRef::BaseClass(BaseClassRef::Enum(_) | BaseClassRef::Union(_)) = node
            {
                warning::warn(
                    "Non-exhaustive enums/unions are not yet supported",
                    WarningType::Todo,
                    info,
                    name,
                )?;
                converter.convert_without_annotations(info)
            } else {
                Err(
                    CompilerException::of("$nonExhaustive is only valid on enums and unions", name)
                        .into(),
                )
            }
        }
        "deprecated" => {
            if let AnnotatableRef::Function(f) = node {
                f.base_converter()
                    .convert_deprecated(info)
                    .map(|x| (x, DivergingInfo::new()))
            } else {
                warning::warn(
                    "Deprecation notices not yet implemented",
                    WarningType::Todo,
                    info,
                    name,
                )?;
                converter.convert_without_annotations(info)
            }
        }
        "native" => Err(CompilerTodoError::of("'native' annotation", name).into()),
        "mustUse" => match node {
            AnnotatableRef::Function(f) => f
                .base_converter()
                .convert_must_use(info, String::new())
                .map(|x| (x, DivergingInfo::new())),
            AnnotatableRef::Method(_) => {
                warning::warn(
                    "'mustUse' annotation does not work on methods",
                    WarningType::Todo,
                    info,
                    node,
                )?;
                converter.convert_without_annotations(info)
            }
            _ => Err(CompilerException::of(
                "'mustUse' annotation is only valid on function definitions",
                node,
            )
            .into()),
        },
        x => Err(CompilerException::of(format!("Unknown annotation '{}'", x), name).into()),
    }
}

fn convert_function<'a, T>(
    converter: &mut T,
    info: &mut CompilerInfo,
    name: &FunctionCallNode,
) -> CompileResult<(BytecodeList, DivergingInfo)>
where
    T: AnnotatableConverter<'a>,
{
    let node = converter.get_annotatable();
    match name.get_variable().unwrap().get_name() {
        "cfg" => {
            if convert_cfg(info, name)? {
                converter.convert_without_annotations(info)
            } else {
                Ok((BytecodeList::new(), DivergingInfo::new()))
            }
        }
        "inline" => convert_inline(converter, info, name),
        "deprecated" => {
            warning::warn(
                "Deprecation notices not yet implemented",
                WarningType::Todo,
                info,
                name,
            )?;
            converter.convert_without_annotations(info)
        }
        "allow" | "deny" | "forbid" => {
            change_warnings(name, info.warning_holder_mut())?;
            let result = converter.convert_without_annotations(info);
            info.warning_holder_mut().pop_warnings();
            result
        }
        "builtin" => {
            if !info.permissions().is_builtin() {
                Err(CompilerException::of("'builtin' is an internal-only annotation", name).into())
            } else if let AnnotatableRef::BaseClass(_) | AnnotatableRef::Function(_) = node {
                Ok((BytecodeList::new(), DivergingInfo::new()))
            } else {
                Err(CompilerException::of(
                    "'builtin' annotation is only valid on class and function definitions",
                    node,
                )
                .into())
            }
        }
        "native" => {
            if !info.permissions().is_stdlib() {
                return Err(
                    CompilerException::of("'native' is an internal-only annotation", name).into(),
                );
            }
            let param = name.get_parameters()[0].get_argument();
            if let Result::Ok("sys") = get_string(param) {
                if let AnnotatableRef::Function(def) = node {
                    def.base_converter()
                        .convert_sys(info)
                        .map(|x| (x, DivergingInfo::new()))
                } else {
                    Err(CompilerTodoError::of("'sys' annotation on non-functions", node).into())
                }
            } else {
                Err(CompilerException::of("Unknown native function type", name).into())
            }
        }
        "derive" => {
            if let AnnotatableRef::BaseClass(_) = node {
                converter.convert_without_annotations(info)
            } else {
                Err(CompilerException::of(
                    "'derive' annotation only works on class definitions",
                    node,
                )
                .into())
            }
        }
        "stable" | "unstable" => {
            if info.permissions().is_stdlib() {
                Err(CompilerTodoError::of("Stable/unstable annotations", name).into())
            } else {
                Err(CompilerException::of(
                    "'stable' and 'unstable' annotations are only \
                     available for the standard library",
                    name,
                )
                .into())
            }
        }
        "feature" => {
            let features = get_strings(name.get_parameters())?;
            if info.permissions().is_stdlib() {
                info.add_features(features.iter().map(|x| x.to_string()));
                let result = converter.convert_without_annotations(info);
                info.remove_features(features);
                return result;
            }
            for feature in &features {
                if STABLE_FEATURES.contains(feature) {
                    warning::warn(
                        format!("Use of '$feature' attribute for stable feature {}", feature),
                        WarningType::Todo,
                        info,
                        name,
                    )?;
                } else {
                    return Err(
                        CompilerTodoError::of("Proper unstable feature annotations", name).into(),
                    );
                }
                info.add_feature(feature.to_string())
            }
            let result = converter.convert_without_annotations(info);
            info.remove_features(features);
            result
        }
        "cfgAttr" => {
            if let [config, attr] = name.get_parameters() {
                if <&NameNode>::try_from(attr.get_argument()).is_err() {
                    Err(
                        CompilerException::of("Invalid format for attribute", attr.get_argument())
                            .into(),
                    )
                } else if config::value_of(info, config.get_argument())? {
                    convert_name(converter, info, attr.get_argument().try_into().unwrap())
                } else {
                    converter.convert_without_annotations(info)
                }
            } else {
                Err(CompilerException::of(
                    format!(
                        "'cfgAttr' annotaion takes precisely 2 arguments, not {}",
                        name.get_parameters().len()
                    ),
                    name,
                )
                .into())
            }
        }
        "extern" => {
            Err(CompilerTodoError::of("'extern' functions are not yet implemented", name).into())
        }
        "mustUse" => {
            if let [use_msg] = name.get_parameters() {
                let message = match use_msg.get_argument() {
                    TestNode::String(s) => s.get_contents(),
                    _ => {
                        return Err(CompilerException::of(
                            "'mustUse' annotation argument must be a string literal",
                            name,
                        )
                        .into())
                    }
                };
                match node {
                    AnnotatableRef::Function(f) => f
                        .base_converter()
                        .convert_must_use(info, message.to_string())
                        .map(|x| (x, DivergingInfo::new())),
                    AnnotatableRef::Method(_) => {
                        warning::warn(
                            "'mustUse' annotations does not work on methods yet",
                            WarningType::Todo,
                            info,
                            node,
                        )?;
                        converter.convert_without_annotations(info)
                    }
                    _ => Err(CompilerException::of(
                        "'mustUse' annotation is only valid on function definitions",
                        node,
                    )
                    .into()),
                }
            } else {
                Err(CompilerException::of(
                    format!(
                        "'mustUse' annotation takes exactly 1 argument, not {}",
                        name.get_parameters().len()
                    ),
                    name,
                )
                .into())
            }
        }
        x => Err(CompilerException::of(format!("Unknown annotation '{}'", x), name).into()),
    }
}

fn convert_inline<'a, T>(
    converter: &mut T,
    info: &mut CompilerInfo,
    inline: &FunctionCallNode,
) -> CompileResult<(BytecodeList, DivergingInfo)>
where
    T: AnnotatableConverter<'a>,
{
    assert_eq!(inline.get_variable().map(|x| x.get_name()), Some("inline"));
    let node = converter.get_annotatable();
    if !node.is_definition() {
        return Err(
            CompilerException::of("Frequency hints may only be used on definitions", node).into(),
        );
    }
    if let [param] = inline.get_parameters() {
        let argument = param.get_argument();
        if !param.is_vararg() && param.get_variable().is_empty() {
            if let TestNode::Name(NameNode::Variable(arg)) = argument {
                match arg.get_name() {
                    "always" | "never" => {
                        warning::warn(
                            "Frequency hints do not do anything yet",
                            WarningType::Todo,
                            info,
                            inline,
                        )?;
                        return converter.convert_without_annotations(info);
                    }
                    _ => {}
                }
            }
        }
    }
    Err(CompilerException::with_note(
        "Invalid format for inline attribute",
        "The only valid forms are $inline, $inline(always), and $inline(never)",
        inline,
    )
    .into())
}

fn change_warnings(
    annotation: &FunctionCallNode,
    warning_holder: &mut WarningHolder,
) -> CompileResult<()> {
    let name = annotation.get_variable().unwrap().get_name();
    assert!(matches!(name, "allow" | "deny" | "forbid"));
    let mut allowed_types = HashSet::new();
    for param in annotation.get_parameters() {
        if !param.get_variable().is_empty() || !param.get_vararg().is_empty() {
            return Err(CompilerException::of(
                format!("Illegal format for {} annotation", name),
                annotation,
            )
            .into());
        }
        let arg_name = match param.get_argument() {
            TestNode::Name(NameNode::Variable(var)) => var.get_name(),
            _ => {
                return Err(CompilerException::of(
                    format!("Illegal format for {} annotation", name),
                    annotation,
                )
                .into())
            }
        };
        if arg_name == "all" {
            if annotation.get_parameters().len() > 1 {
                return Err(CompilerException::of(
                    format!(
                        "'all' used in conjunction with other parameters in '{}' statement",
                        name
                    ),
                    annotation,
                )
                .into());
            }
            warn_all(name, warning_holder, annotation)?;
            return Ok(());
        } else if let Result::Ok(warning) = arg_name.parse() {
            add_warning(warning, &mut allowed_types, annotation, warning_holder)?
        } else {
            return Err(CompilerException::of(
                format!("Unknown warning type {}", arg_name),
                annotation,
            )
            .into());
        }
    }
    warn_some(name, allowed_types, warning_holder, annotation)
}

fn warn_all(
    name: &str,
    warning_holder: &mut WarningHolder,
    annotation: impl Lined,
) -> CompileResult<()> {
    match name {
        "allow" => {
            for &allowed in WARNING_TYPES {
                if warning_holder.is_forbidden(allowed) {
                    return Err(CompilerException::of(
                        format!(
                            "Cannot allow forbidden warning {}",
                            allowed.annotation_name().unwrap()
                        ),
                        annotation,
                    )
                    .into());
                }
            }
            warning_holder.allow_all();
        }
        "deny" => warning_holder.deny_all(),
        "forbid" => warning_holder.forbid_all(),
        _ => {
            return Err(CompilerInternalError::of(
                format!(
                    "Expected 'allow', 'deny', or 'forbid' for name, got {}",
                    name
                ),
                annotation,
            )
            .into())
        }
    }
    Ok(())
}

fn warn_some(
    name: &str,
    allowed_types: HashSet<WarningType>,
    warning_holder: &mut WarningHolder,
    annotation: impl Lined,
) -> CompileResult<()> {
    match name {
        "allow" => {
            for &allowed in &allowed_types {
                if warning_holder.is_forbidden(allowed) {
                    return Err(CompilerException::of(
                        format!(
                            "Cannot allow forbidden warning {}",
                            allowed.annotation_name().unwrap()
                        ),
                        annotation,
                    )
                    .into());
                }
            }
            warning_holder.allow(allowed_types)
        }
        "deny" => warning_holder.deny(allowed_types),
        "forbid" => warning_holder.forbid(allowed_types),
        _ => {
            return Err(CompilerInternalError::of(
                format!(
                    "Expected 'allow', 'deny', or 'forbid' for name, got {}",
                    name
                ),
                annotation,
            )
            .into())
        }
    }
    Ok(())
}

fn add_warning(
    ty: WarningType,
    values: &mut HashSet<WarningType>,
    lined: impl Lined,
    warning_holder: &mut WarningHolder,
) -> CompileResult<()> {
    if values.insert(ty) {
        warning::warn_if(
            "Duplicated lint for warning",
            WarningType::Unused,
            warning_holder,
            lined,
        )
    } else {
        Ok(())
    }
}

fn derive_operator(node: &FunctionCallNode) -> CompileResult<Vec<OpSpTypeNode>> {
    assert_eq!(node.get_variable().map(|x| x.get_name()), Some("derive"));
    node.get_parameters()
        .iter()
        .map(|arg| {
            if arg.is_vararg() {
                Err(
                    CompilerException::of("Varargs are not allowed in 'derive' attributes", node)
                        .into(),
                )
            } else {
                derive_op(arg.get_argument(), node)
            }
        })
        .collect()
}

pub fn derive_operators(nodes: &[NameNode]) -> CompileResult<Vec<OpSpTypeNode>> {
    for node in nodes {
        if let NameNode::Function(f) = node {
            if f.get_variable().map(|x| x.get_name()) == Some("derive") {
                return derive_operator(f);
            }
        }
    }
    Ok(Vec::new())
}

fn derive_op(op: &TestNode, node: impl Lined) -> CompileResult<OpSpTypeNode> {
    match op {
        TestNode::Name(NameNode::EscapedOp(escaped)) => match escaped.get_operator() {
            OpFuncTypeNode::Equals => Ok(OpSpTypeNode::Equals),
            OpFuncTypeNode::Compare => Ok(OpSpTypeNode::Compare),
            _ => Err(invalid_op_exception(node).into()),
        },
        TestNode::Name(NameNode::Variable(var)) => match var.get_name() {
            "hash" => Ok(OpSpTypeNode::Hash),
            "repr" => Ok(OpSpTypeNode::Repr),
            _ => Err(invalid_op_exception(node).into()),
        },
        _ => Err(CompilerException::of(
            "Invalid derived operator: \
             Only escaped operators and references are valid in a derive statement",
            node,
        )
        .into()),
    }
}

fn invalid_op_exception(node: impl Lined) -> CompilerException {
    CompilerException::of(
        "Invalid derived operator: Can only derive ==, <=>, repr, and hash operators",
        node,
    )
}

pub fn should_compile(
    stmt: impl Lined,
    info: &mut CompilerInfo,
    annotations: &[NameNode],
) -> CompileResult<bool> {
    match annotations {
        [] => Ok(true),
        [annotation] => should_compile_single(info, annotation),
        _ => Err(CompilerTodoError::of("Multiple annotations on one statement", stmt).into()),
    }
}

fn should_compile_single(info: &mut CompilerInfo, annotation: &NameNode) -> CompileResult<bool> {
    if let NameNode::Function(stmt) = annotation {
        match stmt.get_variable().map(|x| x.get_name()) {
            Some("test") => Ok(info.global_info().is_test()),
            Some("cfg") => convert_cfg(info, stmt),
            _ => Ok(true),
        }
    } else {
        Ok(true)
    }
}

fn get_strings(values: &[ArgumentNode]) -> CompileResult<Vec<&str>> {
    values
        .iter()
        .map(|x| get_string(x.get_argument()))
        .collect()
}

fn get_string(node: &TestNode) -> CompileResult<&str> {
    if let TestNode::String(s) = node {
        Ok(s.get_contents())
    } else {
        Err(CompilerException::of("Expected string literal here", node).into())
    }
}

pub fn is_builtin(
    line_info: impl Lined,
    permissions: PermissionLevel,
    annotations: &[NameNode],
) -> CompileResult<Option<BuiltinInfo>> {
    match annotations {
        [] => Ok(None),
        [annotation] => builtin_values(permissions, annotation),
        _ => Err(CompilerTodoError::of("Multiple annotations on one statement", line_info).into()),
    }
}

fn builtin_values(
    permissions: PermissionLevel,
    annotation: &NameNode,
) -> CompileResult<Option<BuiltinInfo>> {
    let func = match annotation {
        NameNode::Function(func) => func,
        _ => return Ok(None),
    };
    if func.get_variable().map(|x| x.get_name()) != Some("builtin") {
        return Ok(None);
    }
    if !permissions.is_builtin() {
        return Err(
            CompilerException::of("'builtin' is an internal-only annotation", annotation).into(),
        );
    }
    match &*map_arguments(func.get_parameters()) {
        [] => Err(CompilerException::with_note(
            "Ill formed 'builtin' annotation",
            "'builtin' annotation must have at least 1 parameter",
            annotation,
        )
        .into()),
        [TestNode::String(s)] => Ok(Some(BuiltinInfo::new(
            s.get_contents().to_string(),
            None,
            false,
        ))),
        [TestNode::String(s), TestNode::Number(n)] => Ok(Some(BuiltinInfo::new(
            s.get_contents().to_string(),
            Some(n.get_value().to_usize().unwrap()),
            false,
        ))),
        [TestNode::String(s), TestNode::Number(n), TestNode::Name(NameNode::Variable(v))] => {
            if v.get_name() != "hidden" {
                return Err(CompilerException::with_note(
                    "Ill-formed 'builtin' annotation",
                    "Third parameter of 'builtin' annotation must be the literal 'hidden'",
                    annotation,
                )
                .into());
            }
            Ok(Some(BuiltinInfo::new(
                s.get_contents().to_string(),
                Some(n.get_value().to_usize().unwrap()),
                true,
            )))
        }
        _ => Err(CompilerException::of("Ill-formed 'builtin' annotation", annotation).into()),
    }
}

fn map_arguments(args: &[ArgumentNode]) -> Vec<&TestNode> {
    args.iter().map(|x| x.get_argument()).collect()
}
