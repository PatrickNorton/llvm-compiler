use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::hash::Hash;

use derive_new::new;

use crate::converter::annotation::{self, AnnotatableConverter};
use crate::converter::argument::ArgumentInfo;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::default_holder::DefaultHolder;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::CompilerException;
use crate::converter::generic::GenericInfo;
use crate::converter::CompileResult;
use crate::parser::annotation::AnnotatableRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::generic_stmt::GenericFunctionNode;
use crate::parser::line_info::{LineInfo, Lined};

use crate::converter::access_handler::AccessLevel;
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::fn_info::FunctionInfo;
use crate::parser::method::MethodDefinitionNode;
use crate::parser::stmt_body::StatementBodyNode;
use crate::util::maybe_ref::MaybeRef;
use crate::util::reborrow_option;

#[derive(Debug)]
pub struct Method {
    line_info: LineInfo,
    info: MethodInfo,
    bytes: BytecodeList,
}

#[derive(Debug, new)]
pub struct RawMethod<'a> {
    pub access_level: AccessLevel,
    pub is_mut: bool,
    pub info: FunctionInfo,
    pub body: MaybeRef<'a, StatementBodyNode>,
    pub line_info: LineInfo,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub line_info: LineInfo,
    pub access_level: AccessLevel,
    pub is_mut: bool,
    pub function_info: FunctionInfo,
}

#[derive(Debug)]
pub struct MethodConverter<'a> {
    methods: HashMap<&'a str, RawMethod<'a>>,
    // Needs to be a Cow b/c of variant methods
    static_methods: HashMap<Cow<'a, str>, RawMethod<'a>>,
}

/// A shim to allow annotation conversion through [`annotation::convert_annotatable`].
#[derive(Debug, new)]
struct MethodConvInner<'a, 'b> {
    inner: &'a mut MethodConverter<'b>,
    node: &'b MethodDefinitionNode,
    defaults: Option<&'a mut DefaultHolder<'b>>,
}

impl Method {
    pub fn new(line_info: LineInfo, info: MethodInfo, bytes: BytecodeList) -> Self {
        Self {
            line_info,
            info,
            bytes,
        }
    }

    pub fn get_bytes(&self) -> &BytecodeList {
        &self.bytes
    }

    pub fn get_info(&self) -> &MethodInfo {
        &self.info
    }
}

impl MethodInfo {
    pub fn new(
        line_info: LineInfo,
        access_level: AccessLevel,
        is_mut: bool,
        function_info: FunctionInfo,
    ) -> Self {
        Self {
            line_info,
            access_level,
            is_mut,
            function_info,
        }
    }
}

// TODO: Annotations

impl<'a> MethodConverter<'a> {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
            static_methods: HashMap::new(),
        }
    }

    pub fn parse(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a MethodDefinitionNode,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        annotation::convert_annotatable(&mut MethodConvInner::new(self, node, defaults), info)
            .map(|_| ())
    }

    pub fn parse_inner(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a MethodDefinitionNode,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        let generics = GenericInfo::parse(info, node.get_generics())?;
        info.add_parentless_locals(generics.get_param_map());
        let name = method_name(node);
        let args = ArgumentInfo::of(node.get_args(), info, defaults)?;
        let returns = info.types_of(node.get_retval())?;
        let has_returns = !returns.is_empty();
        let is_gen = node.get_descriptors().contains(&DescriptorNode::Generator);
        let true_ret = if is_gen {
            vec![info.builtins().iterable().generify(node, returns)?]
        } else {
            returns
        };
        let fn_info = FunctionInfo::new(
            node.line_info().clone(),
            name.to_string(),
            is_gen,
            generics,
            args,
            true_ret,
        );
        let access_level = AccessLevel::from_descriptors(node.get_descriptors());
        let is_mut = node.get_descriptors().contains(&DescriptorNode::Mut);
        info.remove_local_types();
        fn_info.set_generic_parent();
        if is_gen && !has_returns {
            return Err(generator_error(node).into());
        }
        check_vars(name, node, &self.methods)?;
        check_vars(name, node, &self.static_methods)?;
        let m_info = RawMethod::new(
            access_level,
            is_mut,
            fn_info,
            node.get_body().into(),
            node.line_info().clone(),
        );
        if !node.get_descriptors().contains(&DescriptorNode::Static) {
            self.methods.insert(name, m_info);
        } else {
            self.static_methods.insert(name.into(), m_info);
        }
        Ok(())
    }

    pub fn parse_generic(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a GenericFunctionNode,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        let name = node.get_name().get_name();
        let args = ArgumentInfo::of(node.get_args(), info, defaults)?;
        let returns = info.types_of(node.get_retvals())?;
        let has_returns = !returns.is_empty();
        let is_gen = node.get_descriptors().contains(&DescriptorNode::Generator);
        let fn_info = FunctionInfo::new(
            node.line_info().clone(),
            name.to_string(),
            is_gen,
            GenericInfo::empty(),
            args,
            returns,
        );
        let descriptors = node.get_descriptors();
        let access_level = AccessLevel::from_descriptors(descriptors);
        let is_mut = descriptors.contains(&DescriptorNode::Mut);
        if is_gen && !has_returns {
            return Err(generator_error(node).into());
        }
        check_vars(name, node, &self.methods)?;
        check_vars(name, node, &self.static_methods)?;
        static EMPTY: StatementBodyNode = StatementBodyNode::empty();
        let m_info = RawMethod::new(
            access_level,
            is_mut,
            fn_info,
            (&EMPTY).into(),
            node.line_info().clone(),
        );
        if !descriptors.contains(&DescriptorNode::Static) {
            self.methods.insert(name, m_info);
        } else {
            self.static_methods.insert(name.into(), m_info);
        }
        Ok(())
    }

    pub fn add_union_methods(
        &mut self,
        variant_methods: HashMap<String, RawMethod<'a>>,
    ) -> CompileResult<()> {
        for (key, val) in &variant_methods {
            check_vars(key, val, &self.static_methods)?;
        }
        self.static_methods
            .extend(variant_methods.into_iter().map(|(x, y)| (x.into(), y)));
        Ok(())
    }

    pub fn get_methods(&self) -> &HashMap<&'a str, RawMethod<'a>> {
        &self.methods
    }

    pub fn get_static_methods(&self) -> &HashMap<Cow<'a, str>, RawMethod<'a>> {
        &self.static_methods
    }

    pub fn take_methods(
        self,
    ) -> (
        HashMap<&'a str, RawMethod<'a>>,
        HashMap<Cow<'a, str>, RawMethod<'a>>,
    ) {
        (self.methods, self.static_methods)
    }
}

impl<'a, 'b> AnnotatableConverter<'b> for MethodConvInner<'a, 'b> {
    fn get_annotatable(&self) -> AnnotatableRef<'b> {
        AnnotatableRef::Method(self.node)
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.inner
            .parse_inner(info, self.node, reborrow_option(&mut self.defaults))?;
        Ok((BytecodeList::new(), DivergingInfo::new()))
    }
}

fn method_name(node: &MethodDefinitionNode) -> &str {
    node.get_name().get_name() // TODO: Distinguish between args
}

fn generator_error(node: impl Lined) -> CompilerException {
    CompilerException::of("Generator functions must have at least one return", node)
}

fn check_vars(
    str_name: &str,
    name: impl Lined,
    vars: &HashMap<impl Borrow<str> + Eq + Hash, impl Lined>,
) -> CompileResult<()> {
    if let Option::Some(line_info) = vars.get(str_name) {
        Err(CompilerException::double_def(str_name, name, line_info).into())
    } else {
        Ok(())
    }
}

impl Lined for Method {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for MethodInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl Lined for RawMethod<'_> {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl From<RawMethod<'_>> for MethodInfo {
    fn from(method_info: RawMethod<'_>) -> Self {
        MethodInfo::new(
            method_info.line_info.clone(),
            method_info.access_level,
            method_info.is_mut,
            method_info.info,
        )
    }
}

impl AsRef<MethodInfo> for MethodInfo {
    fn as_ref(&self) -> &MethodInfo {
        self
    }
}
