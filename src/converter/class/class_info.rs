use std::collections::HashMap;

use crate::converter::constant::{LangConstant, StringConstant};
use crate::converter::file_writer::ConstantSet;
use crate::converter::type_obj::UserType;
use crate::parser::operator_sp::OpSpTypeNode;
use crate::util::{usize_to_bytes, usize_to_short_bytes};

use super::method::Method;

#[derive(Debug)]
pub struct ClassInfo {
    type_val: UserType,
    super_constants: Vec<LangConstant>,
    variables: HashMap<String, u16>,
    static_variables: HashMap<String, u16>,
    operator_defs: HashMap<OpSpTypeNode, Method>,
    static_operators: HashMap<OpSpTypeNode, Method>,
    method_defs: HashMap<String, Method>,
    static_methods: HashMap<String, Method>,
    properties: HashMap<String, (Method, Method)>,
    static_properties: HashMap<String, (Method, Method)>,
    variants: Option<Vec<String>>,
}

#[derive(Debug)]
pub(super) struct Factory {
    type_val: Option<UserType>,
    super_constants: Option<Vec<LangConstant>>,
    variables: Option<HashMap<String, u16>>,
    static_variables: Option<HashMap<String, u16>>,
    operator_defs: Option<HashMap<OpSpTypeNode, Method>>,
    static_operators: Option<HashMap<OpSpTypeNode, Method>>,
    method_defs: Option<HashMap<String, Method>>,
    static_methods: Option<HashMap<String, Method>>,
    properties: Option<HashMap<String, (Method, Method)>>,
    static_properties: Option<HashMap<String, (Method, Method)>>,
    variants: Option<Option<Vec<String>>>,
}

impl ClassInfo {
    pub(super) fn factory() -> Factory {
        Factory::new()
    }

    /// Converts the class into the byte representation to put into a file.
    ///
    /// The file layout is as follows:
    /// ```text
    /// Name of class
    /// Superclasses:
    ///     Index of each class
    /// [byte] If there are generics
    /// Generics (if prev. byte != 0):
    ///     String name
    /// Variables:
    ///     String name of variable
    ///     [short] Type of variable
    /// Static variables:
    ///     String name of variable
    ///     [short] Type of variable
    /// Operators:
    ///     Operator index
    ///     Bytecode of operator
    /// Static operators:
    ///     Operator index
    ///     Bytecode of operator
    /// Methods:
    ///     Name of method
    ///     Bytecode of method
    /// Static methods:
    ///     Name of static method
    ///     Bytecode of method
    /// Properties:
    ///     Name of property
    ///     Bytecode of getter
    ///     Bytecode of setter
    /// ```
    pub fn to_bytes(&self, constants: &ConstantSet) -> Vec<u8> {
        let mut bytes = StringConstant::str_bytes(&self.type_val.name());
        bytes.extend(usize_to_bytes(self.super_constants.len()));
        for super_type in &self.super_constants {
            bytes.extend(usize_to_bytes(constants.get_index_of(super_type).unwrap()))
        }
        bytes.extend(usize_to_short_bytes(self.type_val.get_generic_info().len()));
        match &self.variants {
            Option::None => bytes.push(0),
            Option::Some(variants) => {
                bytes.push(1);
                bytes.extend(usize_to_bytes(variants.len()));
                for variant in variants {
                    bytes.extend(&StringConstant::str_bytes(variant));
                }
            }
        }
        add_variables(&mut bytes, &self.variables);
        add_variables(&mut bytes, &self.static_variables);
        add_operators(&mut bytes, &self.operator_defs);
        add_operators(&mut bytes, &self.static_operators);
        add_methods(&mut bytes, &self.method_defs);
        add_methods(&mut bytes, &self.static_methods);
        add_properties(&mut bytes, &self.properties);
        bytes
    }

    pub fn get_type(&self) -> &UserType {
        &self.type_val
    }

    pub fn get_method_defs(&self) -> &HashMap<String, Method> {
        &self.method_defs
    }

    pub fn get_static_methods(&self) -> &HashMap<String, Method> {
        &self.static_methods
    }

    pub fn get_operator_defs(&self) -> &HashMap<OpSpTypeNode, Method> {
        &self.operator_defs
    }

    pub fn get_properties(&self) -> &HashMap<String, (Method, Method)> {
        &self.properties
    }
}

fn add_variables(bytes: &mut Vec<u8>, byte_map: &HashMap<String, u16>) {
    bytes.extend(usize_to_bytes(byte_map.len()));
    for (name, &index) in byte_map {
        bytes.extend(StringConstant::str_bytes(name));
        bytes.extend(&index.to_be_bytes())
    }
}

fn add_operators(bytes: &mut Vec<u8>, byte_map: &HashMap<OpSpTypeNode, Method>) {
    bytes.extend(usize_to_bytes(byte_map.len()));
    for (name, method) in byte_map {
        bytes.push(*name as u8);
        bytes.push(method.get_info().function_info.is_generator().into());
        let op_bytes = method.get_bytes().convert_to_bytes();
        bytes.extend(usize_to_bytes(op_bytes.len()));
        bytes.extend(op_bytes);
    }
}

fn add_methods(bytes: &mut Vec<u8>, byte_map: &HashMap<String, Method>) {
    bytes.extend(usize_to_bytes(byte_map.len()));
    for (name, method) in byte_map {
        bytes.extend(StringConstant::str_bytes(name));
        bytes.push(method.get_info().function_info.is_generator().into());
        let method_bytes = method.get_bytes().convert_to_bytes();
        bytes.extend(usize_to_bytes(method_bytes.len()));
        bytes.extend(method_bytes);
    }
}

fn add_properties(bytes: &mut Vec<u8>, properties: &HashMap<String, (Method, Method)>) {
    bytes.extend(&usize_to_bytes(properties.len()));
    for (name, (getter, setter)) in properties {
        bytes.extend(StringConstant::str_bytes(name));
        let getter_bytes = getter.get_bytes().convert_to_bytes();
        bytes.push(getter.get_info().function_info.is_generator().into());
        bytes.extend(usize_to_bytes(getter_bytes.len()));
        bytes.extend(getter_bytes);
        let setter_bytes = setter.get_bytes().convert_to_bytes();
        bytes.push(setter.get_info().function_info.is_generator().into());
        bytes.extend(usize_to_bytes(setter_bytes.len()));
        bytes.extend(setter_bytes);
    }
}

macro_rules! add_method {
    ($name:ident, $field:ident, $ty:ty) => {
        #[inline]
        pub fn $name(&mut self, $field: $ty) -> &mut Self {
            match &mut self.$field {
                Option::Some(_) => panic!("Cannot set value twice"),
                x @ Option::None => *x = Some($field),
            };
            self
        }
    };
}

impl Factory {
    pub fn new() -> Self {
        Self {
            type_val: None,
            super_constants: None,
            variables: None,
            static_variables: None,
            operator_defs: None,
            static_operators: None,
            method_defs: None,
            static_methods: None,
            properties: None,
            static_properties: None,
            variants: None,
        }
    }

    pub fn create(self) -> ClassInfo {
        ClassInfo {
            type_val: self.type_val.unwrap(),
            super_constants: self.super_constants.unwrap(),
            variables: self.variables.unwrap(),
            static_variables: self.static_variables.unwrap(),
            operator_defs: self.operator_defs.unwrap(),
            static_operators: self.static_operators.unwrap(),
            method_defs: self.method_defs.unwrap(),
            static_methods: self.static_methods.unwrap(),
            properties: self.properties.unwrap(),
            static_properties: self.static_properties.unwrap(),
            variants: self.variants.unwrap(),
        }
    }

    add_method!(set_type, type_val, UserType);
    add_method!(set_super_constants, super_constants, Vec<LangConstant>);
    add_method!(set_variables, variables, HashMap<String, u16>);
    add_method!(set_static_vars, variables, HashMap<String, u16>);
    add_method!(set_operator_defs, operator_defs, HashMap<OpSpTypeNode, Method>);
    add_method!(set_static_ops, static_operators, HashMap<OpSpTypeNode, Method>);
    add_method!(set_method_defs, method_defs, HashMap<String, Method>);
    add_method!(set_static_methods, static_methods, HashMap<String, Method>);
    add_method!(set_properties, properties, HashMap<String, (Method, Method)>);
    add_method!(set_static_props, static_properties, HashMap<String, (Method, Method)>);
    add_method!(set_variants, variants, Option<Vec<String>>);
}

impl Default for Factory {
    fn default() -> Self {
        Self::new()
    }
}
