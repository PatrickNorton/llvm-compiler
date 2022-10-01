use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display, Formatter};
use std::hash::Hash;
use std::iter::zip;

use derive_new::new;
use itertools::Itertools;
use num::{BigInt, ToPrimitive, Zero};

use crate::parser::dotted::DottedVariableNode;
use crate::parser::line_info::Lined;
use crate::parser::switch_stmt::{CaseStatementNode, SwitchStatementNode};
use crate::parser::test_node::TestNode;
use crate::parser::variable::VariableNode;
use crate::util::fmt_with::format_with;
use crate::util::{first, levenshtein, string_escape};

use super::base_converter::BaseConverter;
use super::bytecode::{Bytecode, Label};
use super::bytecode_list::BytecodeList;
use super::compiler_info::CompilerInfo;
use super::constant::{CharConstant, LangConstant};
use super::convertible::{test_convertible, ConverterBase, ConverterTest, TestConvertible};
use super::diverge::DivergingInfo;
use super::error::{CompilerException, CompilerTodoError};
use super::error_builder::ErrorBuilder;
use super::int_arithmetic::convert_const;
use super::loop_converter::LoopConverter;
use super::switch_table::{
    BigSwitchTable, CharSwitchTable, CompactSwitchTable, StringSwitchTable, SwitchTable,
};
use super::test_converter::TestConverter;
use super::type_obj::{OptionTypeObject, TypeObject, UnionTypeObject};
use super::warning::{self, WarningType};
use super::{CompileBytes, CompileResult, CompileTypes};

#[derive(Debug, new)]
pub struct SwitchConverter<'a> {
    node: &'a SwitchStatementNode,
    ret_count: u16,
}

#[derive(Debug)]
struct TblReturn {
    bytes: BytecodeList,
    diverging_info: DivergingInfo,
    default_val: Label,
}

impl<'a> ConverterTest for SwitchConverter<'a> {
    fn return_type(&mut self, info: &mut CompilerInfo) -> CompileTypes {
        let types = self.get_unmerged_types(info)?;
        let mut final_types = Vec::with_capacity(self.ret_count as usize);
        for i in 0..(self.ret_count as usize) {
            let pos_array = types.iter().map(|x| &x[i]);
            final_types.push(TypeObject::union_of(info, pos_array.cloned().collect()));
        }
        Ok(final_types)
    }
}

impl<'a> LoopConverter for SwitchConverter<'a> {
    const HAS_CONTINUE: bool = false;

    fn true_convert(&mut self, info: &mut CompilerInfo) -> CompileBytes {
        self.true_convert_with_return(info).map(|x| x.0)
    }

    fn true_convert_with_return(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut converter = self.node.get_switched().test_converter(1);
        let ret_type = first(converter.return_type(info)?);
        if <&UnionTypeObject>::try_from(&ret_type).is_err() && self.incomplete_return() {
            return Err(CompilerException::of(
                "Cannot get return from switch: Missing 'default' statement",
                self.node,
            )
            .into());
        }
        if info.builtins().int_type().is_superclass(&ret_type) {
            return self.convert_int(info);
        } else if info.builtins().str_type().is_superclass(&ret_type) {
            return self.convert_str(info);
        } else if info.builtins().char_type().is_superclass(&ret_type) {
            return self.convert_char(info);
        } else if let TypeObject::Union(u) = ret_type {
            return self.convert_union(info, u);
        }
        let mut bytes = converter.convert(info)?;
        let ret_types = if self.ret_count == 0 {
            Vec::new()
        } else {
            self.return_type(info)?
        };
        let mut had_default = false;
        let mut will_return = None;
        for case_stmt in self.node.get_cases() {
            if case_stmt.is_default() {
                had_default = true;
            } else if had_default {
                warning::warn_note(
                    "Default statement before case statement in switch",
                    "In unoptimized switch statements, cases are run through in order \
                     and thus this is unreachable",
                    WarningType::Unreachable,
                    info,
                    case_stmt,
                )?;
            }
            and_with(
                &mut will_return,
                self.add_case(info, case_stmt, &mut bytes, &ret_types)?,
            )
        }
        let mut will_return = will_return.unwrap_or_else(DivergingInfo::new);
        if !had_default {
            will_return.make_uncertain();
        }
        Ok((bytes, will_return))
    }
}

impl<'a> SwitchConverter<'a> {
    fn get_unmerged_types(&self, info: &mut CompilerInfo) -> CompileResult<Vec<Vec<TypeObject>>> {
        let cases = self.node.get_cases();
        let switch_ret = first(TestConverter::return_type(
            info,
            self.node.get_switched(),
            1,
        )?);
        let mut types = Vec::new();
        for case in cases {
            if !case.is_arrow() {
                return Err(CompilerException::of(
                    "Switch with returns must be entirely arrows",
                    case,
                )
                .into());
            }
            let has_as = case.get_as().is_empty();
            if has_as {
                info.add_stack_frame();
                let switch_ret = <&UnionTypeObject>::try_from(&switch_ret).map_err(|_| {
                    CompilerException::of(
                        format!(
                            "Switch with 'as' clause must be over a union, not '{}'",
                            switch_ret.name()
                        ),
                        self.node.get_switched(),
                    )
                })?;
                let var_type = label_to_type(info, &case.get_label()[0], switch_ret)?;
                info.add_variable(
                    case.get_as().get_name().to_string(),
                    var_type,
                    false,
                    case.get_as().line_info().clone(),
                );
            }
            types.push(TestConverter::return_type(
                info,
                <&TestNode>::try_from(&case.get_body()[0]).unwrap(),
                1,
            )?);
            if has_as {
                info.remove_stack_frame();
            }
        }
        Ok(types)
    }

    fn add_case(
        &self,
        info: &mut CompilerInfo,
        stmt: &CaseStatementNode,
        bytes: &mut BytecodeList,
        ret_types: &[TypeObject],
    ) -> CompileResult<DivergingInfo> {
        // TODO: Ensure 'default' statement is at the end
        let labels = stmt.get_label();
        let jump_label = Label::new();
        if !stmt.get_as().is_empty() {
            return Err(
                CompilerException::of("'as' clause not allowed here", stmt.get_as()).into(),
            );
        }
        if !stmt.is_default() {
            assert!(!labels.is_empty());
            let tmp_label = Label::new();
            for (i, label) in labels.iter().enumerate() {
                bytes.add(Bytecode::DupTop());
                bytes.extend(TestConverter::bytes(label, info, 1)?);
                bytes.add(Bytecode::Equal());
                bytes.add(Bytecode::JumpFalse(jump_label.clone().into()));
                if i != labels.len() - 1 {
                    bytes.add(Bytecode::JumpTrue(tmp_label.clone().into()));
                }
            }
            bytes.add_label(tmp_label);
        }
        bytes.add(Bytecode::PopTop());
        let will_return = self.convert_body(info, bytes, stmt, ret_types)?;
        bytes.add(Bytecode::Jump(
            info.loop_manager().break_label(1).clone().into(),
        ));
        bytes.add_label(jump_label);
        Ok(will_return)
    }

    fn convert_int(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.convert_tbl(
            info,
            |info, lbl_converter, lbl| {
                let constant = lbl_converter
                    .constant_return(info)?
                    .ok_or_else(|| literal_exception("int", lbl))?;
                convert_const(&constant)
                    .map(Cow::into_owned)
                    .ok_or_else(|| literal_exception("int", lbl).into())
            },
            // TODO: Remove clone here
            |x, fmt| x.fmt(fmt),
            get_tbl,
        )
    }

    fn convert_str(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.convert_tbl(
            info,
            |info, lbl_converter, lbl| {
                let constant = lbl_converter
                    .constant_return(info)?
                    .ok_or_else(|| literal_exception("string", lbl))?;
                if let LangConstant::String(constant) = constant {
                    Ok(constant.get_value().to_string())
                } else {
                    Err(literal_exception("string", lbl).into())
                }
            },
            |x, fmt| string_escape::escape(x).fmt(fmt),
            str_tbl,
        )
    }

    fn convert_char(
        &mut self,
        info: &mut CompilerInfo,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        self.convert_tbl(
            info,
            |info, lbl_converter, lbl| {
                let constant = lbl_converter
                    .constant_return(info)?
                    .ok_or_else(|| literal_exception("char", lbl))?;
                match constant {
                    LangConstant::Char(c) => Ok(c.get_value()),
                    LangConstant::String(_) => Err(literal_exception_with(
                        "char",
                        lbl,
                        "Try prefixing the literal with 'c'",
                    )
                    .into()),
                    _ => Err(literal_exception("char", lbl).into()),
                }
            },
            |&x, fmt| CharConstant::name(x).fmt(fmt),
            char_tbl,
        )
    }

    fn convert_tbl<T>(
        &mut self,
        info: &mut CompilerInfo,
        add_to_map: impl FnMut(&mut CompilerInfo, &mut TestConverter, &TestNode) -> CompileResult<T>,
        error_escape: impl Fn(&T, &mut Formatter<'_>) -> fmt::Result,
        create_table: impl FnOnce(HashMap<T, Label>, Label) -> SwitchTable,
    ) -> CompileResult<(BytecodeList, DivergingInfo)>
    where
        T: Eq + Hash + Clone,
    {
        let mut jumps = HashMap::new();
        let mut bytes = self.tbl_header(info)?;
        let result = self.convert_tbl_inner(info, add_to_map, error_escape, &mut jumps)?;
        let switch_table = create_table(jumps, result.default_val);
        let tbl_index = info.add_switch_table(switch_table);
        bytes.add(Bytecode::SwitchTable(tbl_index.into()));
        bytes.extend(result.bytes);
        Ok((bytes, result.diverging_info))
    }

    fn convert_tbl_inner<T>(
        &mut self,
        info: &mut CompilerInfo,
        mut add_to_map: impl FnMut(&mut CompilerInfo, &mut TestConverter, &TestNode) -> CompileResult<T>,
        error_escape: impl Fn(&T, &mut Formatter<'_>) -> fmt::Result,
        jumps: &mut HashMap<T, Label>,
    ) -> CompileResult<TblReturn>
    where
        T: Eq + Hash + Clone,
    {
        let mut default_val = None;
        let mut will_return = None;
        let ret_types = if self.ret_count == 0 {
            Vec::new()
        } else {
            self.return_type(info)?
        };
        let mut bytes = BytecodeList::new();
        for stmt in self.node.get_cases() {
            if !stmt.get_as().is_empty() {
                return Err(as_exception(stmt.get_as()).into());
            }
            if stmt.is_default() {
                let will_ret =
                    self.get_default_val(info, &mut default_val, &mut bytes, stmt, &ret_types)?;
                and_with(&mut will_return, will_ret);
                continue;
            }
            if stmt.get_label().is_empty() {
                return Err(empty_label_exception(stmt).into());
            }
            for label in stmt.get_label() {
                let mut lbl_converter = label.test_converter(1);
                let value = add_to_map(info, &mut lbl_converter, label)?;
                // May require feature(map_replace_key) (#44286)
                match jumps.entry(value) {
                    Entry::Vacant(entry) => {
                        let lbl = Label::new();
                        bytes.add_label(lbl.clone());
                        entry.insert(lbl);
                    }
                    Entry::Occupied(entry) => {
                        // TODO: Reference previous entry in error message
                        return Err(CompilerException::of(
                            format!(
                                "Cannot define {} twice in switch statement",
                                format_with(entry.key(), error_escape)
                            ),
                            self.node,
                        )
                        .into());
                    }
                }
            }
            and_with(
                &mut will_return,
                self.convert_body(info, &mut bytes, stmt, &ret_types)?,
            );
            bytes.add(Bytecode::Jump(
                info.loop_manager().break_label(1).clone().into(),
            ));
        }
        let mut will_return = will_return.unwrap_or_default();
        let default_val = default_val.unwrap_or_else(|| {
            will_return.make_uncertain();
            let def = Label::new();
            bytes.add_label(def.clone());
            def
        });
        Ok(TblReturn {
            bytes,
            diverging_info: will_return,
            default_val,
        })
    }

    fn convert_union(
        &mut self,
        info: &mut CompilerInfo,
        union: UnionTypeObject,
    ) -> CompileResult<(BytecodeList, DivergingInfo)> {
        let mut jumps = HashMap::new();
        let has_as = self.any_has_as();
        let mut bytes = TestConverter::bytes(self.node.get_switched(), info, 1)?;
        if has_as {
            bytes.add(Bytecode::DupTop());
        }
        bytes.add(Bytecode::VariantNo());
        let result = self.convert_union_inner(info, has_as, &union, &mut jumps)?;
        let switch_table = small_tbl(jumps, result.default_val);
        let tbl_index = info.add_switch_table(switch_table);
        bytes.add(Bytecode::SwitchTable(tbl_index.into()));
        bytes.extend(result.bytes);
        Ok((bytes, result.diverging_info))
    }

    fn convert_union_inner(
        &mut self,
        info: &mut CompilerInfo,
        has_as: bool,
        union: &UnionTypeObject,
        jumps: &mut HashMap<u16, Label>,
    ) -> CompileResult<TblReturn> {
        let mut default_val = None;
        let mut bytes = BytecodeList::new();
        let mut will_return = None;
        let ret_types = if self.ret_count == 0 {
            Vec::new()
        } else {
            self.return_type(info)?
        };
        let mut used_variants = HashSet::new();
        for stmt in self.node.get_cases() {
            if stmt.is_default() {
                if default_val.is_some() {
                    return Err(default_exception(stmt).into());
                }
                assert!(stmt.get_as().is_empty());
                if has_as {
                    bytes.add(Bytecode::PopTop());
                }
                let (label, diverging) =
                    self.convert_default(info, &mut bytes, stmt, &ret_types)?;
                default_val = Some(label);
                and_with(&mut will_return, diverging);
                continue;
            }
            let stmt_has_as = !stmt.get_as().is_empty();
            if stmt.get_label().is_empty() {
                return Err(empty_label_exception(stmt).into());
            } else if stmt.get_label().len() > 1 && stmt_has_as {
                // TODO? As clause with multiple labels of same type
                return Err(CompilerException::of(
                    "Cannot use 'as' clause with more than one label",
                    stmt,
                )
                .into());
            }
            for label in stmt.get_label() {
                let lbl_no = label_to_variant_no(info, label, union)?;
                if used_variants.contains(&lbl_no) {
                    let name = union.variant_name(lbl_no).unwrap();
                    // TODO: Use double_def here & refer to previous label
                    return Err(CompilerException::of(
                        format!("Variant {} defined twice in switch statement", name),
                        stmt,
                    )
                    .into());
                }
                used_variants.insert(lbl_no);
                let jump_label = Label::new();
                bytes.add_label(jump_label.clone());
                jumps.insert(lbl_no, jump_label);
                if stmt_has_as {
                    // Will work b/c there must only be one label if there is an 'as' clause
                    assert_eq!(stmt.get_label().len(), 1);
                    let as_stmt = stmt.get_as();
                    bytes.add(Bytecode::GetVariant(lbl_no.into()));
                    bytes.add(Bytecode::UnwrapOption());
                    info.add_stack_frame();
                    let lbl_type = label_to_type(info, label, union)?;
                    info.add_variable(
                        as_stmt.get_name().to_string(),
                        lbl_type,
                        true,
                        as_stmt.line_info().clone(),
                    );
                    bytes.add(Bytecode::Store(info.var_index(as_stmt)?.into()));
                }
            }
            if has_as && !stmt_has_as {
                bytes.add(Bytecode::PopTop());
            }
            and_with(
                &mut will_return,
                self.convert_body(info, &mut bytes, stmt, &ret_types)?,
            );
            bytes.add(Bytecode::Jump(
                info.loop_manager().break_label(1).clone().into(),
            ));
            if !stmt.get_as().is_empty() {
                info.remove_stack_frame();
            }
        }
        let mut will_return = will_return.unwrap_or_default();
        self.check_complete_union(info, &mut will_return, &default_val, union, &used_variants)?;
        let default_val = default_val.unwrap_or_else(|| {
            let label = Label::new();
            bytes.add_label(label.clone());
            label
        });
        Ok(TblReturn {
            bytes,
            diverging_info: will_return,
            default_val,
        })
    }

    fn check_complete_union(
        &self,
        info: &mut CompilerInfo,
        will_return: &mut DivergingInfo,
        default_val: &Option<Label>,
        union: &UnionTypeObject,
        used_variants: &HashSet<u16>,
    ) -> CompileResult<()> {
        if default_val.is_none() && self.ret_count > 0 {
            if let Option::Some(missing) = incomplete_union(union, used_variants) {
                return Err(CompilerException::with_note(
                    "Cannot get return type of switch",
                    format!(
                        "Missing union variant{} {}",
                        if missing.len() == 1 { "" } else { "s" },
                        missing.into_iter().format(", ")
                    ),
                    self.node,
                )
                .into());
            }
        } else if default_val.is_some() {
            if incomplete_union(union, used_variants).is_none() {
                let default_info = self
                    .default_stmt()
                    .expect("Should be a default statement here");
                warning::warn(
                    "Default statement in switch with all variants covered",
                    WarningType::Unreachable,
                    info,
                    default_info,
                )?;
            }
        } else if let Option::Some(missing) = incomplete_union(union, used_variants) {
            warning::warn_note(
                "Switch on union does not cover all variants",
                format!(
                    "Missing union variant{} {}",
                    if missing.len() == 1 { "" } else { "s" },
                    missing.into_iter().format(", ")
                ),
                WarningType::IncompleteSwitch,
                info,
                self.node,
            )?;
            will_return.make_uncertain();
        }
        Ok(())
    }

    fn convert_body(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        stmt: &CaseStatementNode,
        ret_types: &[TypeObject],
    ) -> CompileResult<DivergingInfo> {
        if stmt.is_arrow() {
            self.convert_arrow(info, bytes, stmt, ret_types)
        } else {
            if self.ret_count > 0 {
                return Err(CompilerTodoError::of(
                    "Statements requiring 'break as' not supported yet",
                    stmt,
                )
                .into());
            }
            info.add_stack_frame();
            let (bytecode, will_return) = BaseConverter::bytes_with_return(stmt.get_body(), info)?;
            bytes.extend(bytecode);
            info.remove_stack_frame();
            Ok(will_return)
        }
    }

    fn convert_arrow(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        stmt: &CaseStatementNode,
        ret_types: &[TypeObject],
    ) -> CompileResult<DivergingInfo> {
        debug_assert!(stmt.is_arrow());
        let node = <&TestNode>::try_from(&stmt.get_body()[0]).unwrap();
        let mut converter = node.test_converter(self.ret_count);
        let converter_ret = converter.return_type(info)?;
        let (bytecode, ret) = converter.convert_with_return(info)?;
        bytes.extend(bytecode);
        for (i, (expected, given)) in zip(ret_types, &converter_ret).enumerate() {
            if OptionTypeObject::needs_make_option(expected, given) {
                add_swap(bytes, ret_types.len() - i - 1);
                bytes.add(Bytecode::MakeOption());
                add_swap(bytes, ret_types.len() - i - 1);
            }
        }
        Ok(ret)
    }

    fn get_default_val(
        &self,
        info: &mut CompilerInfo,
        default_val: &mut Option<Label>,
        bytes: &mut BytecodeList,
        stmt: &CaseStatementNode,
        ret_types: &[TypeObject],
    ) -> CompileResult<DivergingInfo> {
        if default_val.is_some() {
            Err(default_exception(stmt).into())
        } else {
            let (label, diverging) = self.convert_default(info, bytes, stmt, ret_types)?;
            *default_val = Some(label);
            Ok(diverging)
        }
    }

    fn convert_default(
        &self,
        info: &mut CompilerInfo,
        bytes: &mut BytecodeList,
        stmt: &CaseStatementNode,
        ret_types: &[TypeObject],
    ) -> CompileResult<(Label, DivergingInfo)> {
        let default_lbl = Label::new();
        bytes.add_label(default_lbl.clone());
        let will_return = if stmt.is_arrow() {
            self.convert_arrow(info, bytes, stmt, ret_types)?
        } else {
            let (bytecode, diverging) = BaseConverter::bytes_with_return(stmt.get_body(), info)?;
            bytes.extend(bytecode);
            diverging
        };
        bytes.add(Bytecode::Jump(
            info.loop_manager().break_label(1).clone().into(),
        ));
        Ok((default_lbl, will_return))
    }

    fn tbl_header(&self, info: &mut CompilerInfo) -> CompileBytes {
        let mut converter = self.node.get_switched().test_converter(1);
        let constant = converter.constant_return(info)?;
        if let Option::Some(str_value) = constant.and_then(|x| x.str_value()) {
            warning::warn(
                format!("Switch conditional always evaluates to {}", str_value),
                WarningType::TrivialValue,
                info,
                self.node.get_switched(),
            )?;
        }
        converter.convert(info)
    }

    fn incomplete_return(&self) -> bool {
        self.ret_count > 0 && !self.node.get_cases().iter().any(|x| x.is_default())
    }

    fn any_has_as(&self) -> bool {
        self.node.get_cases().iter().any(|x| !x.get_as().is_empty())
    }

    fn default_stmt(&self) -> Option<&CaseStatementNode> {
        self.node.get_cases().iter().find(|x| x.is_default())
    }
}

fn label_to_variant_no(
    info: &mut CompilerInfo,
    label: &TestNode,
    switched_type: &UnionTypeObject,
) -> CompileResult<u16> {
    label_to_pair(info, label, switched_type).map(|(idx, _)| idx)
}

fn label_to_type(
    info: &mut CompilerInfo,
    label: &TestNode,
    switched_type: &UnionTypeObject,
) -> CompileResult<TypeObject> {
    label_to_pair(info, label, switched_type).map(|(_, ty)| ty.clone())
}

fn label_to_pair<'a>(
    info: &mut CompilerInfo,
    label: &TestNode,
    switched_type: &'a UnionTypeObject,
) -> CompileResult<(u16, &'a TypeObject)> {
    if let Result::Ok(dotted_lbl) = <&DottedVariableNode>::try_from(label) {
        let lbl_first = dotted_lbl.get_pre_dot();
        let lbl_second = dotted_lbl.get_post_dots();
        let mut first_converter = lbl_first.test_converter(1);
        let first_ret_type = first(first_converter.return_type(info)?);
        if let TypeObject::Type(first_type) = first_ret_type {
            if lbl_second.len() == 1 {
                let ret_type = first_type.represented_type();
                // TODO: Remove clone here
                if ret_type.same_base_type(&switched_type.clone().into()) {
                    if let Result::Ok(name) =
                        <&VariableNode>::try_from(lbl_second[0].get_post_dot())
                    {
                        if lbl_second[0].get_dot_prefix().is_empty() {
                            return switched_type.variant_info(name.get_name()).ok_or_else(|| {
                                invalid_variant_err(name.get_name(), switched_type, label).into()
                            });
                        }
                    }
                } else {
                    return Err(CompilerException::of(
                        format!(
                            "Mismatched types in label: label has type '{}', switched on type '{}'",
                            ret_type.name(),
                            switched_type.name()
                        ),
                        label,
                    )
                    .into());
                }
            }
        }
    }
    Err(CompilerException::of(
        "Switch on a union must have properly-formed variants",
        label,
    )
    .into())
}

fn invalid_variant_err(
    name: &str,
    switched_type: &UnionTypeObject,
    label: &TestNode,
) -> CompilerException {
    CompilerException::from_builder(
        ErrorBuilder::new(label)
            .with_message(format!("Invalid name for union variant: {}", name))
            .when_some(
                levenshtein::closest_name(name, switched_type.variant_names()),
                |builder, closest| builder.with_help(format!("Did you mean '{}'?", closest)),
            ),
    )
}

fn get_tbl(mut jumps: HashMap<BigInt, Label>, default_val: Label) -> SwitchTable {
    let threshold = jumps.len().saturating_mul(2);
    let zero = BigInt::zero();
    // TODO: Does this mean `jumps` should be a BTreeMap?
    let max = jumps.keys().max().unwrap_or(&zero);
    if max > &threshold.into() {
        BigSwitchTable::new(jumps, default_val).into()
    } else {
        let tbl_size = max.to_usize().unwrap();
        let table = (0..=tbl_size).map(|i| {
            jumps
                .remove(&i.into())
                .unwrap_or_else(|| default_val.clone())
        });
        CompactSwitchTable::new(table.collect(), default_val).into()
    }
}

fn small_tbl(mut jumps: HashMap<u16, Label>, default_val: Label) -> SwitchTable {
    let max = jumps.keys().max().copied().unwrap_or(0);
    let table = (0..=max).map(|i| jumps.remove(&i).unwrap_or_else(|| default_val.clone()));
    CompactSwitchTable::new(table.collect(), default_val).into()
}

fn str_tbl(jumps: HashMap<String, Label>, default_val: Label) -> SwitchTable {
    StringSwitchTable::new(jumps, default_val).into()
}

fn char_tbl(jumps: HashMap<char, Label>, default_val: Label) -> SwitchTable {
    CharSwitchTable::new(jumps, default_val).into()
}

fn literal_exception(literal_type: &str, label: impl Lined) -> CompilerException {
    CompilerException::of(
        format!(
            "'switch' on a {literal_type} requires a {literal_type} literal in each case statement"
        ),
        label,
    )
}

fn literal_exception_with(literal: &str, label: impl Lined, note: &str) -> CompilerException {
    CompilerException::with_note(
        format!("'switch' on a {literal} requires a {literal} literal in each case statement"),
        note,
        label,
    )
}

fn as_exception(as_stmt: impl Lined) -> CompilerException {
    CompilerException::of(
        "'as' clauses in a switch are only allowed when the switched value is a union",
        as_stmt,
    )
}

fn empty_label_exception(stmt: impl Lined) -> CompilerException {
    CompilerException::of("Case statements must have at least one label", stmt)
}

fn default_exception(stmt: impl Lined) -> CompilerException {
    CompilerException::of(
        "Cannot have more than one 'default' statement in a switch",
        stmt,
    )
}

fn incomplete_union<'a>(obj: &'a UnionTypeObject, variants: &HashSet<u16>) -> Option<Vec<&'a str>> {
    let result = (0..obj.variant_count())
        .filter(|x| !variants.contains(x))
        .map(|x| obj.variant_name(x).unwrap())
        .collect_vec();
    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

fn and_with(info_1: &mut Option<DivergingInfo>, info_2: DivergingInfo) {
    if let Option::Some(info) = info_1 {
        info.and_with(info_2)
    } else {
        *info_1 = Some(info_2)
    }
}

fn add_swap(bytes: &mut BytecodeList, dist_from_top: usize) {
    match dist_from_top {
        0 => {}
        1 => bytes.add(Bytecode::Swap2()),
        x => bytes.add(Bytecode::SwapStack(0.into(), (x as u16).into())),
    }
}

test_convertible!(SwitchStatementNode, SwitchConverter);
