use std::borrow::Cow;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use itertools::Itertools;

use crate::converter::fn_info::FunctionInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::macros::{arc_eq_hash, type_obj_from};
use super::TypeObject;

#[derive(Debug, Clone)]
pub struct FunctionInfoType {
    value: Arc<FnInfoInner>,
}

#[derive(Debug, Clone)]
pub struct GenerifiedFnInfoType {
    value: Arc<GenerifiedFnInfoInner>,
}

#[derive(Debug)]
struct FnInfoInner {
    info: FunctionInfo,
    typedef_name: Option<String>,
}

#[derive(Debug)]
struct GenerifiedFnInfoInner {
    info: FunctionInfo,
    typedef_name: Option<String>,
    generics: Vec<TypeObject>,
}

impl FunctionInfoType {
    pub fn new(value: FunctionInfo) -> Self {
        Self {
            value: Arc::new(FnInfoInner {
                info: value,
                typedef_name: None,
            }),
        }
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.value
            .typedef_name
            .as_deref()
            .map_or_else(|| self.base_name(), Cow::Borrowed)
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        if self.value.info.get_returns().is_empty() {
            format!("func({})", self.value.info.get_args()).into()
        } else {
            format!(
                "func({}) -> {}",
                self.value.info.get_args(),
                self.value
                    .info
                    .get_returns()
                    .iter()
                    .map(|x| x.name())
                    .format(", ")
            )
            .into()
        }
    }

    pub fn generify(&self, args: Vec<TypeObject>) -> TypeObject {
        GenerifiedFnInfoType::new(self.value.info.clone(), args).into()
    }

    pub fn generify_with(&self, parent: &TypeObject, values: Vec<TypeObject>) -> TypeObject {
        FunctionInfoType::new(self.value.info.generify(parent, values)).into()
    }

    pub fn set_generic_parent(&self) {
        self.value.info.set_generic_parent();
    }

    pub fn operator_info(&self, op: OpSpTypeNode) -> Option<&FunctionInfo> {
        if op == OpSpTypeNode::Call {
            Some(&self.value.info)
        } else {
            None
        }
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::FnInfo(f) => self.value.info == f.value.info,
            TypeObject::GenerifiedFn(f) => self.value.info == f.value.info,
            _ => false,
        }
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.value.info.hash(state)
    }

    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(FnInfoInner {
                info: self.value.info.clone(),
                typedef_name: Some(name),
            }),
        }
    }
}

impl GenerifiedFnInfoType {
    pub fn new(value: FunctionInfo, generics: Vec<TypeObject>) -> Self {
        Self {
            value: Arc::new(GenerifiedFnInfoInner {
                info: value,
                typedef_name: None,
                generics,
            }),
        }
    }

    pub fn name(&self) -> Cow<'_, str> {
        self.value
            .typedef_name
            .as_deref()
            .map_or_else(|| self.base_name(), Cow::Borrowed)
    }

    pub fn base_name(&self) -> Cow<'_, str> {
        if self.value.info.get_returns().is_empty() {
            format!("func{}", self.value.info.get_args()).into()
        } else {
            format!(
                "func{} -> {}",
                self.value.info.get_args(),
                self.value
                    .info
                    .get_returns()
                    .iter()
                    .map(|x| x.name())
                    .format(", ")
            )
            .into()
        }
    }

    pub fn operator_info(&self, op: OpSpTypeNode) -> Option<FunctionInfo> {
        if op == OpSpTypeNode::Call {
            todo!("Work out lifetimes here")
        } else {
            None
        }
    }

    pub fn same_base_type(&self, other: &TypeObject) -> bool {
        match other {
            TypeObject::FnInfo(f) => self.value.info == f.value.info,
            TypeObject::GenerifiedFn(f) => self.value.info == f.value.info,
            _ => false,
        }
    }

    pub fn base_hash<H: Hasher>(&self, state: &mut H) {
        self.value.info.hash(state)
    }

    #[must_use = "typedef_as returns a new type and doesn't modify the original"]
    pub fn typedef_as(&self, name: String) -> Self {
        Self {
            value: Arc::new(GenerifiedFnInfoInner {
                info: self.value.info.clone(),
                typedef_name: Some(name),
                generics: self.value.generics.clone(),
            }),
        }
    }
}

impl From<FunctionInfo> for TypeObject {
    fn from(x: FunctionInfo) -> Self {
        FunctionInfoType::new(x).into()
    }
}

arc_eq_hash!(FunctionInfoType);
arc_eq_hash!(GenerifiedFnInfoType);

type_obj_from!(FunctionInfoType, FnInfo);
type_obj_from!(GenerifiedFnInfoType, GenerifiedFn);

#[cfg(test)]
mod tests {
    use crate::converter::argument::{Argument, ArgumentInfo};
    use crate::converter::builtins::OBJECT;
    use crate::converter::fn_info::FunctionInfo;
    use crate::converter::type_obj::{ObjectType, TupleType, TypeTypeObject};

    use super::FunctionInfoType;

    #[test]
    fn empty_name() {
        let ty = FunctionInfoType::new(Default::default());
        assert_eq!(ty.name(), "func()");
        assert_eq!(ty.base_name(), "func()");
    }

    #[test]
    fn empty_typedef_name() {
        let ty = FunctionInfoType::new(Default::default()).typedef_as("Test".to_string());
        assert_eq!(ty.name(), "Test");
        assert_eq!(ty.base_name(), "func()");
    }

    fn argument_infos() -> Vec<ArgumentInfo> {
        vec![
            ArgumentInfo::new(Vec::new(), Vec::new(), Vec::new()),
            ArgumentInfo::new(
                Vec::new(),
                vec![Argument::new(
                    "test_normal".to_string(),
                    ObjectType::new().into(),
                )],
                Vec::new(),
            ),
            ArgumentInfo::new(
                vec![Argument::new(
                    "test_keyword".to_string(),
                    ObjectType::new().into(),
                )],
                Vec::new(),
                Vec::new(),
            ),
            ArgumentInfo::new(
                Vec::new(),
                Vec::new(),
                vec![Argument::new(
                    "test_position".to_string(),
                    ObjectType::new().into(),
                )],
            ),
        ]
    }

    #[test]
    fn argument_name() {
        for args in argument_infos() {
            let args_fmt = format!("func({args})");
            let fn_info = FunctionInfo::with_args(args, Vec::new());
            let ty = FunctionInfoType::new(fn_info);
            assert_eq!(ty.name(), args_fmt);
            assert_eq!(ty.base_name(), args_fmt);
        }
    }

    #[test]
    fn return_name() {
        let values = [
            (vec![OBJECT.into()], "object"),
            (
                vec![OBJECT.into(), TupleType::default().into()],
                "object, tuple",
            ),
            (
                vec![
                    OBJECT.into(),
                    TupleType::default().into(),
                    TypeTypeObject::new(OBJECT.into()).into(),
                ],
                "object, tuple, type[object]",
            ),
        ];
        for (ret, ret_str) in values {
            for args in argument_infos() {
                let args_fmt = format!("func({args}) -> {ret_str}");
                let fn_info = FunctionInfo::with_args(args, ret.clone());
                let ty = FunctionInfoType::new(fn_info);
                assert_eq!(ty.name(), args_fmt);
                assert_eq!(ty.base_name(), args_fmt);
            }
        }
    }

    #[test]
    fn argument_typedef_name() {
        for args in argument_infos() {
            let args_fmt = format!("func({args})");
            let fn_info = FunctionInfo::with_args(args, Vec::new());
            let ty = FunctionInfoType::new(fn_info).typedef_as("test".into());
            assert_eq!(ty.name(), "test");
            assert_eq!(ty.base_name(), args_fmt);
        }
    }

    #[test]
    fn return_typedef_name() {
        let values = [
            (vec![OBJECT.into()], "object"),
            (
                vec![OBJECT.into(), TupleType::default().into()],
                "object, tuple",
            ),
            (
                vec![
                    OBJECT.into(),
                    TupleType::default().into(),
                    TypeTypeObject::new(OBJECT.into()).into(),
                ],
                "object, tuple, type[object]",
            ),
        ];
        for (ret, ret_str) in values {
            for args in argument_infos() {
                let args_fmt = format!("func({args}) -> {ret_str}");
                let fn_info = FunctionInfo::with_args(args, ret.clone());
                let ty = FunctionInfoType::new(fn_info).typedef_as("test".into());
                assert_eq!(ty.name(), "test");
                assert_eq!(ty.base_name(), args_fmt);
            }
        }
    }
}
