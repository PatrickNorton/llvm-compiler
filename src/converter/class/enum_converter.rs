use derive_new::new;
use itertools::{process_results, Itertools};

use crate::converter::access_handler::AccessLevel;
use crate::converter::annotation::{impl_annotatable, AnnotatableConverter};
use crate::converter::argument::ArgumentInfo;
use crate::converter::bytecode::{ArgcBytecode, Bytecode, Label};
use crate::converter::bytecode_list::BytecodeList;
use crate::converter::compiler_info::CompilerInfo;
use crate::converter::convertible::{base_convertible, ConverterBase};
use crate::converter::default_holder::DefaultHolder;
use crate::converter::diverge::DivergingInfo;
use crate::converter::error::CompilerException;
use crate::converter::fn_info::FunctionInfo;
use crate::converter::generic::GenericInfo;
use crate::converter::test_converter::TestConverter;
use crate::converter::type_loader::TypeLoader;
use crate::converter::type_obj::{StdTypeObject, UserTypeLike};
use crate::converter::{CompileBytes, CompileResult};
use crate::parser::annotation::AnnotatableRef;
use crate::parser::definition::BaseClassRef;
use crate::parser::descriptor::DescriptorNode;
use crate::parser::enum_def::{EnumDefinitionNode, EnumKeywordNode};
use crate::parser::line_info::{LineInfo, Lined};
use crate::parser::operator_sp::OpSpTypeNode;
use crate::parser::stmt_body::StatementBodyNode;

use super::converter_holder::ConverterHolder;
use super::method::{MethodInfo, RawMethod};
use super::{convert_supers, ensure_proper_inheritance, ClassConverterBase};

#[derive(Debug, new)]
pub struct EnumConverter<'a> {
    node: &'a EnumDefinitionNode,
}

impl<'a> ClassConverterBase<'a> for EnumConverter<'a> {
    type Converted = EnumDefinitionNode;

    fn get_node(&self) -> &'a Self::Converted {
        self.node
    }
}

impl<'a> AnnotatableConverter<'a> for EnumConverter<'a> {
    fn get_annotatable(&self) -> AnnotatableRef<'a> {
        AnnotatableRef::BaseClass(BaseClassRef::Enum(self.node))
    }

    fn convert_without_annotations(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut converter = ConverterHolder::new();
        let true_supers = convert_supers(self.node, info.types_of(self.node.get_superclasses())?)?;
        let has_type = info.has_type(self.node.get_name().str_name());
        let type_val;
        if !has_type {
            if !self.node.get_name().get_subtypes().is_empty() {
                return Err(self.subclass_exception(info).into());
            }
            type_val = StdTypeObject::new(
                self.node.get_name().str_name().to_string(),
                Some(true_supers.iter().cloned().map_into().collect()),
                GenericInfo::empty(),
                true,
            );
            let user_type = type_val.clone().into();
            ensure_proper_inheritance(self.node, &user_type, &true_supers)?;
            info.add_type(type_val.clone().into());
            self.parse_into_object(info, &mut converter, &type_val, None)?;
        } else {
            type_val = info
                .get_type_obj(self.node.get_name().str_name())
                .unwrap()
                .clone()
                .try_into()
                .unwrap();
            self.parse_statements(info, &mut converter, None)?;
        }
        if self
            .node
            .get_descriptors()
            .contains(&DescriptorNode::Nonfinal)
        {
            return Err(CompilerException::of("Enum class may not be nonfinal", self.node).into());
        }
        let super_constants = true_supers
            .iter()
            .map(|x| info.get_constant(&x.name()).unwrap().into_owned())
            .collect_vec();
        let op_new = converter
            .ops
            .mut_operators()
            .entry(OpSpTypeNode::New)
            .or_insert_with(|| self.default_new());
        // TODO: Remove clone here (and with type_val)
        let op_new_info = op_new.1.info.clone();
        if has_type {
            self.put_in_info(
                info,
                type_val.clone().into(),
                "enum",
                None,
                super_constants,
                converter,
            )?;
        } else {
            self.add_to_info(
                info,
                type_val.clone().into(),
                "enum",
                None,
                super_constants,
                converter,
            )?;
        }
        self.get_init_bytes(info, &type_val, &op_new_info)
            .map(|x| (x, DivergingInfo::new()))
    }
}

impl_annotatable!(EnumConverter<'a>);

impl<'a> EnumConverter<'a> {
    pub fn complete_type(
        &self,
        info: &mut CompilerInfo,
        obj: &StdTypeObject,
        reserve: bool,
        defaults: &mut DefaultHolder<'a>,
    ) -> CompileResult<u16> {
        let mut converter = ConverterHolder::new();
        obj.get_generic_info()
            .re_parse(info, self.node.get_name().get_subtypes())?;
        info.access_handler_mut().add_cls(obj.clone().into());
        let result = self.parse_into_object(info, &mut converter, obj, Some(defaults));
        info.access_handler_mut().remove_cls();
        result?;
        if reserve {
            Ok(info.reserve_class(obj.clone().into()))
        } else {
            Ok(u16::MAX)
        }
    }

    fn parse_into_object(
        &self,
        info: &mut CompilerInfo,
        converter: &mut ConverterHolder<'a>,
        obj: &StdTypeObject,
        defaults: Option<&mut DefaultHolder<'a>>,
    ) -> CompileResult<()> {
        self.parse_statements(info, converter, defaults)?;
        obj.is_const_class();
        converter
            .attrs
            .add_enum_statics(self.node.get_names(), obj)?;
        let mut operator_infos = converter.ops.get_operator_infos();
        operator_infos.remove(&OpSpTypeNode::New);
        converter.check_attributes()?;
        obj.set_operators(operator_infos);
        obj.set_static_operators(converter.ops.static_operator_infos());
        obj.set_attributes(converter.all_attrs());
        obj.set_static_attributes(converter.static_attrs());
        obj.seal(Some(info.global_info()), Some(info.builtins()));
        Ok(())
    }

    fn subclass_exception(&self, info: &mut CompilerInfo) -> CompilerException {
        // If every subtype of an enum is actually a defined class, the user
        // probably meant to use subclasses; this helps them accordingly
        assert!(!self.node.get_name().get_subtypes().is_empty());
        process_results(
            self.node
                .get_name()
                .get_subtypes()
                .iter()
                .map(|x| info.convert_type(x)),
            |iter| {
                CompilerException::of(
                    format!(
                        "Enums are not allowed to have generic types\n\
                         Help: If this is a list of superclasses, put \
                         it in a 'from' clause:\n\
                         enum {} from {}",
                        self.node.get_name().str_name(),
                        // TODO: Remove to_string call
                        iter.map(|x| x.name().to_string()).format(", ")
                    ),
                    self.node.get_name(),
                )
            },
        )
        .unwrap_or_else(|_| {
            CompilerException::of(
                "Enums are not allowed to have generic types",
                self.node.get_name(),
            )
        })
    }

    fn get_init_bytes(
        &self,
        info: &mut CompilerInfo,
        ty: &StdTypeObject,
        new_operator_info: &FunctionInfo,
    ) -> CompileResult<BytecodeList> {
        let loop_label = Label::new();
        let mut bytes = BytecodeList::new();
        bytes.add(Bytecode::DoStatic(loop_label.clone().into()));
        bytes.extend(
            TypeLoader::new(self.node.line_info().clone(), ty.clone().into()).convert(info)?,
        );
        for name in self.node.get_names() {
            bytes.add(Bytecode::DupTop());
            match name {
                EnumKeywordNode::Variable(_) => {
                    if !new_operator_info.matches(&[]) {
                        return Err(CompilerException::of(
                            "Incorrect number of arguments for enum \
                             (parentheses may only be omitted when enum \
                             constructor may take 0 arguments)",
                            name,
                        )
                        .into());
                    }
                }
                EnumKeywordNode::Function(fn_node) => {
                    // NOTE: Is this actually the correct thing to match against here?
                    if !new_operator_info.matches(&[]) {
                        return Err(CompilerException::of(
                            "Invalid arguments for enum constructor",
                            name,
                        )
                        .into());
                    }
                    bytes.add(Bytecode::DupTop());
                    for arg in fn_node.get_parameters() {
                        bytes.extend(TestConverter::bytes(arg.get_argument(), info, 1)?);
                    }
                    bytes.add(Bytecode::CallTos(ArgcBytecode::new(
                        fn_node.get_parameters().len() as u16,
                    )));
                }
            }
        }
        bytes.add(Bytecode::PopTop());
        bytes.add_label(loop_label);
        Ok(bytes)
    }

    fn default_new(&self) -> (MethodInfo, RawMethod<'a>) {
        let fn_info = FunctionInfo::new(
            LineInfo::empty(),
            String::new(),
            false,
            GenericInfo::empty(),
            ArgumentInfo::empty(),
            Vec::new(),
        );
        (
            MethodInfo::new(
                LineInfo::empty(),
                AccessLevel::Private,
                false,
                fn_info.clone(),
            ),
            RawMethod::new(
                AccessLevel::Private,
                false,
                fn_info,
                StatementBodyNode::empty().into(),
                LineInfo::empty(),
            ),
        )
    }
}

base_convertible!(EnumDefinitionNode, EnumConverter);
