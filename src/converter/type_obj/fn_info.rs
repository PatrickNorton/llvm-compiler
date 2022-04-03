use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::iter::zip;
use std::sync::Arc;

use itertools::Itertools;

use crate::converter::builtins::CALLABLE;
use crate::converter::fn_info::FunctionInfo;
use crate::parser::operator_sp::OpSpTypeNode;

use super::macros::{arc_eq_hash, arc_partial_eq, type_obj_from};
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

    pub fn is_superclass(&self, other: &TypeObject) -> bool {
        other.is_subclass(&self.clone().into())
    }

    pub fn is_subclass(&self, other: &TypeObject) -> bool {
        // if (other.sameBaseType(Builtins.callable())) {
        //     var generics = other.getGenerics();
        //     assert generics.size() == 2;
        //     var args = generics.get(0);
        //     var rets = generics.get(1);
        //     assert args instanceof ListTypeObject && rets instanceof ListTypeObject;
        //     var arguments = ((ListTypeObject) args).getValues();
        //     var returns = ((ListTypeObject) rets).getValues();
        //     var thisArgs = info.getArgs();
        //     if (thisArgs.getKeywordArgs().length > 0
        //             || thisArgs.size() != arguments.length
        //             || returns.length > info.getReturns().length) {
        //         return false;
        //     }
        //     for (var pair : Zipper.of(arguments, thisArgs)) {
        //         var arg = pair.getKey();
        //         var thisArg = pair.getValue();
        //         if (!thisArg.getType().isSuperclass(arg)) {
        //             return false;
        //         }
        //     }
        //     for (var pair : Zipper.of(returns, info.getReturns())) {
        //         if (!pair.getKey().isSuperclass(pair.getValue())) {
        //             return false;
        //         }
        //     }
        //     return true;
        // }
        // return this.equals(other);
        if other.same_base_type(&CALLABLE) {
            let generics = other.get_generics();
            // NOTE: feature(let_else) (#87335) would make this nicer
            let (args, rets) = if let [TypeObject::List(args), TypeObject::List(rets)] = generics {
                (args.get_values(), rets.get_values())
            } else {
                panic!()
            };
            let this_args = self.value.info.get_args();
            if !this_args.get_keyword_args().is_empty()
                || this_args.len() != args.len()
                || rets.len() > self.value.info.get_returns().len()
            {
                return false;
            }
            for (arg, this_arg) in zip(args.iter(), this_args.iter()) {
                if !this_arg.get_type().is_superclass(arg) {
                    return false;
                }
            }
            for (ret, this_ret) in zip(rets, self.value.info.get_returns()) {
                if !ret.is_superclass(this_ret) {
                    return false;
                }
            }
            true
        } else {
            self == other
        }
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

    pub fn generify_as(
        &self,
        parent: &TypeObject,
        other: &TypeObject,
    ) -> Option<HashMap<u16, TypeObject>> {
        if self.is_superclass(other) {
            return Some(HashMap::new());
        }
        let other = match other {
            TypeObject::FnInfo(f) => f,
            _ => return None,
        };
        let info = &self.value.info;
        let other_info = &other.value.info;
        let mut result = HashMap::new();
        let args = info.get_args();
        let other_args = other_info.get_args();
        if args.len() != other_args.len() {
            return None;
        }
        for (arg, other_arg) in zip(args.iter(), other_args.iter()) {
            result.extend(arg.get_type().generify_as(parent, other_arg.get_type())?);
        }
        for (ret, other_ret) in zip(info.get_returns(), other_info.get_returns()) {
            result.extend(ret.generify_as(parent, other_ret)?);
        }
        Some(result)
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
            Some(
                self.value
                    .info
                    .generify(&self.clone().into(), self.value.generics.clone()),
            )
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
arc_partial_eq!(FunctionInfoType, FnInfo);
arc_partial_eq!(GenerifiedFnInfoType, GenerifiedFn);

type_obj_from!(FunctionInfoType, FnInfo);
type_obj_from!(GenerifiedFnInfoType, GenerifiedFn);

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::converter::argument::{Argument, ArgumentInfo};
    use crate::converter::builtins::OBJECT;
    use crate::converter::fn_info::FunctionInfo;
    use crate::converter::generic::GenericInfo;
    use crate::converter::type_obj::{
        ObjectType, StdTypeObject, TemplateParam, TupleType, TypeTypeObject, UserTypeLike,
    };
    use crate::macros::hash_map;

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

    #[test]
    fn rets_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new("parent".into(), Some(Vec::new()), generic_info, true);
        parent.set_generic_parent();
        let func = FunctionInfoType::new(FunctionInfo::from_returns(vec![param.into()]));
        let tup = TupleType::new(Vec::new());
        let other = FunctionInfoType::new(FunctionInfo::from_returns(vec![tup.clone().into()]));
        assert_eq!(
            func.generify_as(&parent.into(), &other.into()),
            Some(hash_map!(0 => tup.into()))
        );
    }

    #[test]
    fn args_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new("parent".into(), Some(Vec::new()), generic_info, true);
        parent.set_generic_parent();
        let func = FunctionInfoType::new(FunctionInfo::with_args(
            ArgumentInfo::of_types([param.into()]),
            Vec::new(),
        ));
        let tup = TupleType::new(Vec::new());
        let other = FunctionInfoType::new(FunctionInfo::with_args(
            ArgumentInfo::of_types([tup.clone().into()]),
            Vec::new(),
        ));
        assert_eq!(
            func.generify_as(&parent.into(), &other.into()),
            Some(hash_map!(0 => tup.into()))
        );
    }

    #[test]
    fn both_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new("parent".into(), Some(Vec::new()), generic_info, true);
        parent.set_generic_parent();
        let func = FunctionInfoType::new(FunctionInfo::with_args(
            ArgumentInfo::of_types([param.clone().into()]),
            vec![param.into()],
        ));
        let tup = TupleType::new(Vec::new());
        let other = FunctionInfoType::new(FunctionInfo::with_args(
            ArgumentInfo::of_types([tup.clone().into()]),
            vec![tup.clone().into()],
        ));
        assert_eq!(
            func.generify_as(&parent.into(), &other.into()),
            Some(hash_map!(0 => tup.into()))
        );
    }

    #[test]
    fn empty_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param]);
        let parent = StdTypeObject::new("parent".into(), Some(Vec::new()), generic_info, true);
        parent.set_generic_parent();
        let tup = TupleType::new(Vec::new());
        let func = FunctionInfoType::new(FunctionInfo::from_returns(vec![tup.clone().into()]));
        let other = FunctionInfoType::new(FunctionInfo::from_returns(vec![tup.into()]));
        let parent_ty = parent.into();
        assert_eq!(
            func.generify_as(&parent_ty, &other.into()),
            Some(HashMap::new())
        );
    }

    #[test]
    fn invalid_generify_as() {
        let param = TemplateParam::new("T".into(), 0, OBJECT.into());
        let generic_info = GenericInfo::new(vec![param.clone()]);
        let parent = StdTypeObject::new("parent".into(), Some(Vec::new()), generic_info, true);
        parent.set_generic_parent();
        let tup = TupleType::new(Vec::new());
        let func = FunctionInfoType::new(FunctionInfo::with_args(
            ArgumentInfo::of_types([param.into()]),
            vec![tup.clone().into()],
        ));
        let other = FunctionInfoType::new(FunctionInfo::from_returns(vec![tup.into()]));
        assert_eq!(func.generify_as(&parent.into(), &other.into()), None);
    }
}
