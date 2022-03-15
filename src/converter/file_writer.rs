use std::fs::{create_dir, File};
use std::io::{stdout, BufWriter, Write};
use std::path::Path;

use derive_new::new;
use indexmap::IndexSet;

use crate::util::{usize_to_bytes, MAGIC_NUMBER};

use super::builtins::Builtins;
use super::constant::{LangConstant, StringConstant};
use super::global_info::GlobalCompilerInfo;

/// A wrapper around [`IndexSet<LangConstant>`] to ensure that
/// [`TempConsts`](TempConst) are referenced properly.
///
/// # Reasoning
/// Since temporary constants use interior mutability to give themselves a
/// proper value, they need to be taken into account specially in file-writing,
/// since they won't hash properly when put into a hash-based data structure.
///
/// Note that in order to avoid accidentally using the wrong method, [`Deref`](Deref)
/// should *not* be implemented here, but required methods should be manually
/// reimplemented and forwarded where appropriate.
///
/// # Further work
/// It might make sense to rework the [`PartialEq`], [`Eq`], and [`Hash`]
/// implementations on [`LangConstant`] to work around temporaries, but that may
/// be unfeasable.
#[derive(Debug, new)]
pub struct ConstantSet {
    value: IndexSet<LangConstant>,
}

/// Writes the compiled file to the file specified.
///
/// # File layout
/// ```text
/// Magic number: 0xABADE66
/// Imports:
///     Name of import
///     Name of import
/// Exports:
///     Name of export
///     Index of constant
/// Constants:
///     Byte representation of each constant ([LangConstant::to_bytes])
/// Functions:
///     Function name
///     Generator?
///     Number of local variables (currently unused)
///     Bytecode length
///     Bytecode
/// Classes:
///     Byte representation of the class ([ClassInfo::to_bytes])
/// Tables:
///     Byte representation of the table ([SwitchTable::to_bytes])
/// ```
pub fn write_to_file<P: AsRef<Path>>(
    info: &mut GlobalCompilerInfo,
    file: P,
) -> std::io::Result<()> {
    let constants = ConstantSet::new(info.calculate_constants());
    if info.get_arguments().should_print_bytecode() {
        // NOTE: This could be improved by #[feature(stdio_locked)] (#86845)
        print_disassembly(info, &mut stdout().lock(), &constants)?;
    }
    let args = info.get_arguments();
    if let Option::Some(path) = args.get_bytecode_path() {
        let f = File::create(path)?;
        let mut buf = BufWriter::new(f);
        print_disassembly(info, &mut buf, &constants)?;
    }
    let file = file.as_ref();
    let parent = file.parent().unwrap();
    if parent.exists() {
        create_dir(parent)?;
    }
    write_bytes(info, file, &constants)
}

// TODO? Mutable reference to info
fn write_bytes(
    info: &mut GlobalCompilerInfo,
    file: &Path,
    constants: &ConstantSet,
) -> std::io::Result<()> {
    let mut writer = BufWriter::new(File::create(file)?);
    writer.write_all(MAGIC_NUMBER)?;
    writer.write_all(&0u32.to_be_bytes())?; // In statically linked file, there are no imports or exports
    writer.write_all(&0u32.to_be_bytes())?;
    writer.write_all(&usize_to_bytes(constants.len()))?;
    for constant in constants {
        writer.write_all(&constant.to_bytes(constants))?;
    }
    let (functions, classes) = info.get_functions_classes();
    writer.write_all(&usize_to_bytes(functions.len()))?;
    for function in &*functions {
        let bytes = function.get_bytes().convert_to_bytes();
        writer.write_all(&StringConstant::str_bytes(function.get_name()))?;
        writer.write_all(&[function.is_generator().into()])?;
        writer.write_all(&function.get_max().to_be_bytes())?;
        writer.write_all(&usize_to_bytes(bytes.len()))?;
        writer.write_all(&bytes)?;
    }
    writer.write_all(&usize_to_bytes(classes.len()))?;
    for cls in classes {
        let cls = cls
            .as_ref()
            .expect("All classes should be set before writing to bytecode");
        writer.write_all(&cls.to_bytes(constants))?;
    }
    let tables = info.get_tables();
    writer.write_all(&usize_to_bytes(tables.len()))?;
    for tbl in &*tables {
        // Note: All labels should be written to at point of translation
        writer.write_all(&tbl.to_bytes())?;
    }
    writer.flush()
}

fn print_disassembly<W: Write>(
    info: &mut GlobalCompilerInfo,
    stream: &mut W,
    constants: &ConstantSet,
) -> std::io::Result<()> {
    writeln!(stream, "{}", info.dest_file().display())?;
    writeln!(stream, "Constants:")?;
    let builtins = Builtins::new(info.global_builtins());
    for (i, constant) in constants.iter().enumerate() {
        writeln!(stream, "{}: {}", i, constant.display(&builtins))?;
    }
    writeln!(stream)?;
    let (functions, classes) = info.get_functions_classes();
    for function in &functions {
        writeln!(stream, "{}:", function.get_name())?;
        function.get_bytes().disassemble_to(&functions, stream)?;
    }
    for cls in classes {
        let cls = cls
            .as_ref()
            .expect("All classes should be set before writing to bytecode");
        let cls_name = cls.get_type().name();
        for (name, method) in cls.get_method_defs() {
            writeln!(stream, "{}.{}:", cls_name, name)?;
            method.get_bytes().disassemble_to(&functions, stream)?;
        }
        for (name, method) in cls.get_static_methods() {
            writeln!(stream, "{}.{}:", cls_name, name)?;
            method.get_bytes().disassemble_to(&functions, stream)?;
        }
        for (op, method) in cls.get_operator_defs() {
            writeln!(stream, "{}.{}:", cls_name, op)?;
            method.get_bytes().disassemble_to(&functions, stream)?;
        }
        for (name, (getter, setter)) in cls.get_properties() {
            writeln!(stream, "{}.{}.get:", cls_name, name)?;
            getter.get_bytes().disassemble_to(&functions, stream)?;
            writeln!(stream, "{}.{}.set:", cls_name, name)?;
            setter.get_bytes().disassemble_to(&functions, stream)?;
        }
    }
    for (i, table) in info.get_tables().iter().enumerate() {
        writeln!(stream, "Table {}:", i)?;
        table.disassemble_to(stream)?;
    }
    stream.flush()
}

impl ConstantSet {
    pub fn get_index_of(&self, value: &LangConstant) -> Option<usize> {
        if let LangConstant::Temp(t) = value {
            self.value.get_index_of(
                t.get_inner()
                    .expect("All temp constants should be set before file-writing"),
            )
        } else {
            self.value.get_index_of(value)
        }
    }

    pub fn iter(&self) -> indexmap::set::Iter<'_, LangConstant> {
        self.value.iter()
    }

    pub fn len(&self) -> usize {
        self.value.len()
    }
}

impl<'a> IntoIterator for &'a ConstantSet {
    type Item = &'a LangConstant;

    type IntoIter = indexmap::set::Iter<'a, LangConstant>;

    fn into_iter(self) -> Self::IntoIter {
        self.value.iter()
    }
}
