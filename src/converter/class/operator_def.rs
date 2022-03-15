use std::collections::{HashMap, HashSet};

use derive_new::new;

use crate::converter::access_handler::AccessLevel;
use crate::converter::annotation::{self, AnnotatableConverter};
use crate::converter::argument::ArgumentInfo;
use crate::converter::builtins::{Builtins, OBJECT};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::default_holder::DefaultHolder;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::{CompilerException, CompilerInternalError, CompilerTodoError};
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::type_obj::TypeObject;
use crate::converter::{CompileResult, CompileTypes};
use crate::parser::annotation::AnnotatableRef;
use crate::parser::base::IndependentNode;
use crate::parser::derived_op::DerivedOperatorNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::generic_stmt::GenericOperatorNode;
use crate::parser::line_info::Lined;
use crate::parser::operator_def::OperatorDefinitionNode;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::stmt_body::StatementBodyNode;

use super::method::{MethodInfo, RawMethod};

type OpMap<'a> = HashMap<OpSpTypeNode, (MethodInfo, RawMethod<'a>)>;

#[derive(Debug)]
pub struct OperatorDefConverter<'a> {
    operators: OpMap<'a>,
    static_operators: OpMap<'a>,
}

#[derive(Debug, new)]
struct OperatorConvInner<'a, 'b> {
    inner: &'a mut OperatorDefConverter<'b>,
    node: &'b OperatorDefinitionNode,
}

impl<'a> OperatorDefConverter<'a> {
    pub fn new() -> Self {
        Self {
            operators: OpMap::new(),
            static_operators: OpMap::new(),
        }
    }

    pub fn get_operator_infos(&self) -> HashMap<OpSpTypeNode, MethodInfo> {
        self.operators
            .iter()
            .map(|(&x, (y, _))| (x, y.clone()))
            .collect()
    }

    pub fn static_operator_infos(&self) -> HashMap<OpSpTypeNode, MethodInfo> {
        self.static_operators
            .iter()
            .map(|(&x, (y, _))| (x, y.clone()))
            .collect()
    }

    pub fn parse(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a OperatorDefinitionNode,
    ) -> CompileResult<()> {
        annotation::convert_annotatable(&mut OperatorConvInner::new(self, node), info).map(|_| ())
    }

    pub fn parse_inner(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a OperatorDefinitionNode,
    ) -> CompileResult<()> {
        let op = node.get_op_code().get_operator();
        let args = ArgumentInfo::of(node.get_args(), info, None)?;
        let returns = info.types_of(node.get_ret_types())?;
        let line_info = node
            .get_ret_types()
            .first()
            .map_or_else(|| node.line_info(), |x| x.line_info());
        let is_generator = ALWAYS_GENERATOR.contains(&op);
        let ret_values = validate_returns(info, line_info, is_generator, op, returns)?;
        let fn_info = FunctionInfo::new(
            node.line_info().clone(),
            op.to_string(),
            is_generator,
            GenericInfo::empty(),
            args,
            ret_values,
        );
        let is_static = node.get_descriptors().contains(&DescriptorNode::Static);
        let ops = if is_static {
            &mut self.static_operators
        } else {
            &mut self.operators
        };
        add_to_ops(
            node,
            node.get_descriptors(),
            op,
            fn_info,
            ops,
            node.get_body(),
        )
    }

    pub fn parse_generic(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a GenericOperatorNode,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        let op = node.get_op_code().get_operator();
        let args = ArgumentInfo::of(node.get_args(), info, defaults)?;
        let returns = info.types_of(node.get_ret_types())?;
        let line_info = node
            .get_ret_types()
            .first()
            .map_or_else(|| node.line_info(), |x| x.line_info());
        let is_generator = ALWAYS_GENERATOR.contains(&op);
        let ret_values = validate_returns(info, line_info, is_generator, op, returns)?;
        let fn_info = FunctionInfo::new(
            node.line_info().clone(),
            op.to_string(),
            is_generator,
            GenericInfo::empty(),
            args,
            ret_values,
        );
        static EMPTY: StatementBodyNode = StatementBodyNode::empty();
        add_to_ops(
            node,
            node.get_descriptors(),
            op,
            fn_info,
            &mut self.operators,
            &EMPTY,
        )
    }

    pub fn parse_derived(
        &mut self,
        info: &mut CompilerInfo,
        op: OpSpTypeNode,
        line_info: impl Lined + Copy,
    ) -> CompileResult<()> {
        let args = derived_args(op, line_info)?;
        let returns = derived_rets(op, line_info, info.builtins())?;
        let fn_info = FunctionInfo::new(
            line_info.line_info().clone(),
            op.to_string(),
            false,
            GenericInfo::empty(),
            args,
            returns,
        );
        let body = StatementBodyNode::new(
            line_info.line_info().clone(),
            vec![IndependentNode::Derived(DerivedOperatorNode::new(
                line_info.line_info().clone(),
                op,
            ))],
        );
        check_ops(line_info, op, &fn_info, &self.operators)?;
        let method_info = MethodInfo::new(
            line_info.line_info().clone(),
            AccessLevel::Public,
            false,
            fn_info.clone(),
        );
        let raw_method = RawMethod::new(
            AccessLevel::Public,
            false,
            fn_info,
            body.into(),
            line_info.line_info().clone(),
        );
        self.operators.insert(op, (method_info, raw_method));
        Ok(())
    }

    pub fn take_ops(self) -> (OpMap<'a>, OpMap<'a>) {
        (self.operators, self.static_operators)
    }

    pub fn mut_operators(&mut self) -> &mut OpMap<'a> {
        &mut self.operators
    }
}

impl<'a, 'b> AnnotatableConverter<'b> for OperatorConvInner<'a, 'b> {
    fn get_annotatable(&self) -> AnnotatableRef<'b> {
        AnnotatableRef::Operator(self.node)
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.inner.parse_inner(info, self.node)?;
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

fn validate_returns(
    info: &mut CompilerInfo,
    line_info: impl Lined,
    is_generator: bool,
    op: OpSpTypeNode,
    returns: Vec<TypeObject>,
) -> CompileTypes {
    if let Option::Some(ret_count) = mandatory_returns(op) {
        if ret_count == 1 {
            Err(CompilerException::of(
                format!("{} must specify a return value", op.sequence()),
                line_info,
            )
            .into())
        } else {
            Err(CompilerException::of(
                format!(
                    "{} must return at least {} values, only {} were declared",
                    op,
                    ret_count,
                    returns.len()
                ),
                line_info,
            )
            .into())
        }
    } else if !returns.is_empty() {
        if let Option::Some(ret) = default_return(op, info.builtins()) {
            if !ret.is_superclass(&returns[0]) {
                return Err(CompilerException::of(
                    format!(
                        "{} must return '{}', which clashes with the given type '{}'",
                        op,
                        returns[0].name(),
                        ret.name()
                    ),
                    line_info,
                )
                .into());
            }
        }
        Ok(if is_generator {
            vec![info.builtins().iterable().generify(&line_info, returns)?]
        } else {
            returns
        })
    } else {
        Ok(default_return(op, info.builtins()).map_or_else(Vec::new, |x| vec![x.clone()]))
    }
}

fn add_to_ops<'a>(
    lined: impl Lined,
    descriptors: &HashSet<DescriptorNode>,
    op: OpSpTypeNode,
    fn_info: FunctionInfo,
    ops: &mut OpMap<'a>,
    body: &'a StatementBodyNode,
) -> CompileResult<()> {
    check_ops(&lined, op, &fn_info, ops)?;
    let access_level = AccessLevel::from_descriptors(descriptors);
    let is_mut = op == OpSpTypeNode::New || descriptors.contains(&DescriptorNode::Mut);
    let m_info = MethodInfo::new(
        lined.line_info().clone(),
        access_level,
        is_mut,
        fn_info.clone(),
    );
    let raw = RawMethod::new(
        access_level,
        is_mut,
        fn_info,
        body.into(),
        lined.line_info().clone(),
    );
    ops.insert(op, (m_info, raw));
    Ok(())
}

fn check_ops<'a>(
    node: impl Lined,
    op: OpSpTypeNode,
    fn_info: &FunctionInfo,
    ops: &OpMap<'a>,
) -> CompileResult<()> {
    if let Option::Some(info) = ops.get(&op) {
        Err(CompilerException::double_def_op(op, node, &info.0).into())
    } else if EMPTY_ARGS.contains(&op) && !fn_info.get_args().is_empty() {
        Err(empty_args_error(op, node).into())
    } else {
        Ok(())
    }
}

fn derived_args(op: OpSpTypeNode, line_info: impl Lined) -> CompileResult<ArgumentInfo> {
    match op {
        OpSpTypeNode::Hash | OpSpTypeNode::Repr => Ok(ArgumentInfo::empty()),
        OpSpTypeNode::Equals => Ok(ArgumentInfo::of_types([OBJECT.into()])),
        OpSpTypeNode::Compare => {
            Err(CompilerTodoError::of("compare operator args", line_info).into())
        }
        _ => Err(CompilerInternalError::of(
            format!("Unexpected derived operator: {}", op),
            line_info,
        )
        .into()),
    }
}

fn derived_rets(op: OpSpTypeNode, line_info: impl Lined, builtins: &Builtins) -> CompileTypes {
    match op {
        OpSpTypeNode::Hash => Ok(vec![builtins.int_type().clone()]),
        OpSpTypeNode::Repr => Ok(vec![builtins.str_type().clone()]),
        OpSpTypeNode::Equals => Ok(vec![builtins.bool_type().clone()]),
        OpSpTypeNode::Compare => {
            Err(CompilerTodoError::of("compare operator return", line_info).into())
        }
        _ => Err(CompilerInternalError::of(
            format!("Unexpected derived operator: {}", op),
            line_info,
        )
        .into()),
    }
}

fn empty_args_error(op: OpSpTypeNode, node: impl Lined) -> CompilerException {
    CompilerException::of(
        format!("Operator {} requires an empty argument list", op.sequence()),
        node,
    )
}

fn default_return(op: OpSpTypeNode, builtins: &Builtins) -> Option<&TypeObject> {
    match op {
        OpSpTypeNode::Str => Some(builtins.str_type()),
        OpSpTypeNode::Bool => Some(builtins.bool_type()),
        OpSpTypeNode::Repr => Some(builtins.str_type()),
        OpSpTypeNode::Int => Some(builtins.int_type()),
        OpSpTypeNode::Hash => Some(builtins.int_type()),
        // Boolean operators
        OpSpTypeNode::Equals
        | OpSpTypeNode::LessThan
        | OpSpTypeNode::LessEqual
        | OpSpTypeNode::GreaterThan
        | OpSpTypeNode::GreaterEqual
        | OpSpTypeNode::In => Some(builtins.bool_type()),

        OpSpTypeNode::Compare => Some(builtins.int_type()),
        _ => None,
    }
}

fn mandatory_returns(op: OpSpTypeNode) -> Option<usize> {
    match op {
        OpSpTypeNode::Iter
        | OpSpTypeNode::IterSlice
        | OpSpTypeNode::GetAttr
        | OpSpTypeNode::GetSlice => Some(1),
        _ => None,
    }
}

const EMPTY_ARGS: &[OpSpTypeNode] = &[
    OpSpTypeNode::Int,
    OpSpTypeNode::Str,
    OpSpTypeNode::Bool,
    OpSpTypeNode::Repr,
    OpSpTypeNode::Hash,
    OpSpTypeNode::Iter,
    OpSpTypeNode::Reversed,
];

const ALWAYS_GENERATOR: &[OpSpTypeNode] = &[
    OpSpTypeNode::Iter,
    OpSpTypeNode::IterSlice,
    OpSpTypeNode::Reversed,
];
