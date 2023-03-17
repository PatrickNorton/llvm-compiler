use std::collections::hash_map::Entry;
use std::collections::HashMap;

use derive_new::new;

use crate::converter::access_handler::AccessLevel;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::mutable::MutableType;
use crate::converter::type_obj::{StdTypeObject, TypeObject};
use crate::converter::CompileResult;
use crate::parser::declaration::DeclarationNode;
use crate::parser::declared_assign::DeclaredAssignmentNode;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::enum_def::EnumKeywordNode;
use crate::parser::line_info::{LineInfo, Lined};

use super::method::RawMethod;

#[derive(Debug)]
pub struct AttributeConverter<'a> {
    vars: HashMap<String, AttributeInfo>,
    static_vars: HashMap<String, AttributeInfo>,
    colons: HashMap<String, RawMethod<'a>>,
    static_colons: HashMap<String, RawMethod<'a>>,
}

#[derive(Debug, Clone, new)]
pub struct AttributeInfo {
    is_method: bool,
    access_level: AccessLevel,
    mut_type: MutableType,
    type_val: TypeObject,
    line_info: LineInfo,
}

impl<'a> AttributeConverter<'a> {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            static_vars: HashMap::new(),
            colons: HashMap::new(),
            static_colons: HashMap::new(),
        }
    }

    pub fn parse(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a DeclarationNode,
    ) -> CompileResult<()> {
        let str_name = node.get_name().get_name();
        let descriptors = node.get_descriptors();
        let access_level = AccessLevel::from_descriptors(descriptors);
        let mut_type = MutableType::from_descriptors(descriptors);
        check_vars(str_name, node.get_name(), &self.vars)?;
        check_vars(str_name, node.get_name(), &self.static_vars)?;
        let attr_info = AttributeInfo::new(
            false,
            access_level,
            mut_type,
            info.convert_type(node.get_type().as_type())?,
            node.line_info().clone(),
        );
        if descriptors.contains(&DescriptorNode::Static) {
            self.static_vars.insert(str_name.to_string(), attr_info);
        } else {
            self.vars.insert(str_name.to_string(), attr_info);
        }
        Ok(())
    }

    pub fn parse_assign(
        &mut self,
        info: &mut CompilerInfo,
        stmt: &'a DeclaredAssignmentNode,
    ) -> CompileResult<()> {
        if stmt.is_colon() {
            self.parse_colon(info, stmt)
        } else {
            self.parse_non_colon(info, stmt)
        }
    }

    fn parse_colon(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a DeclaredAssignmentNode,
    ) -> CompileResult<()> {
        todo!("Sort out lifetimes here")
    }

    fn parse_non_colon(
        &mut self,
        info: &mut CompilerInfo,
        node: &'a DeclaredAssignmentNode,
    ) -> CompileResult<()> {
        let attr_type = info.convert_type(node.get_types()[0].get_type().as_type())?;
        let access_level = AccessLevel::from_descriptors(node.get_descriptors());
        let mut_type = MutableType::from_descriptors(node.get_descriptors());
        let attr_info = AttributeInfo::new(
            false,
            access_level,
            mut_type,
            attr_type,
            node.line_info().clone(),
        );
        let name = node.get_types()[0].get_name();
        if node.get_descriptors().contains(&DescriptorNode::Static) {
            check_vars(name, node, &self.static_vars)?;
            check_vars(name, node, &self.static_colons)?;
            self.static_vars.insert(name.to_string(), attr_info);
        } else {
            check_vars(name, node, &self.vars)?;
            check_vars(name, node, &self.colons)?;
            self.vars.insert(name.to_string(), attr_info);
        }
        Ok(())
    }

    pub fn get_vars(&self) -> &HashMap<String, AttributeInfo> {
        &self.vars
    }

    pub fn get_static_vars(&self) -> &HashMap<String, AttributeInfo> {
        &self.static_vars
    }

    pub fn get_colons(&self) -> &HashMap<String, RawMethod<'a>> {
        &self.colons
    }

    pub fn get_static_colons(&self) -> &HashMap<String, RawMethod<'a>> {
        &self.static_colons
    }

    pub fn add_enum_statics(
        &mut self,
        names: &[EnumKeywordNode],
        obj: &StdTypeObject,
    ) -> CompileResult<()> {
        for name in names {
            let str_name = name.get_variable().get_name();
            match self.static_vars.entry(str_name.to_string()) {
                Entry::Vacant(e) => e.insert(AttributeInfo::new(
                    false,
                    AccessLevel::Public,
                    MutableType::Standard,
                    obj.clone().into(),
                    name.line_info().clone(),
                )),
                Entry::Occupied(e) => {
                    return Err(CompilerException::double_def(str_name, e.get(), name).into());
                }
            };
        }
        Ok(())
    }

    pub fn vars_with_ints(self) -> (HashMap<String, u16>, HashMap<String, u16>) {
        let attrs = self.vars.into_keys().map(|x| (x, 0)).collect();
        let static_attrs = self.static_vars.into_keys().map(|x| (x, 0)).collect();
        (attrs, static_attrs)
    }
}

impl AttributeInfo {
    pub fn method(info: FunctionInfo) -> Self {
        Self::new(
            true,
            AccessLevel::Public,
            MutableType::Standard,
            info.to_callable(),
            LineInfo::empty(),
        )
    }

    pub fn get_access_level(&self) -> AccessLevel {
        self.access_level
    }

    pub fn get_type(&self) -> &TypeObject {
        &self.type_val
    }

    pub fn is_method(&self) -> bool {
        self.is_method
    }

    pub fn get_mut_type(&self) -> MutableType {
        self.mut_type
    }
}

fn check_vars(
    str_name: &str,
    name: impl Lined,
    vars: &HashMap<String, impl Lined>,
) -> CompileResult<()> {
    if let Option::Some(line_info) = vars.get(str_name) {
        Err(CompilerException::double_def(str_name, name, line_info).into())
    } else {
        Ok(())
    }
}

impl Lined for AttributeInfo {
    fn line_info(&self) -> &LineInfo {
        &self.line_info
    }
}

impl AsRef<AttributeInfo> for AttributeInfo {
    fn as_ref(&self) -> &AttributeInfo {
        self
    }
}
