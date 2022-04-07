use std::collections::HashSet;

use super::bytecode::{Bytecode, BytecodeRef};
use super::bytecode_list::BytecodeList;
use super::global_info::GlobalCompilerInfo;

/// The function that implements `-fdce`.
///
/// This currently does two passes of optimization: The first eliminates all
/// jumps to the next instruction; the second eliminates all code between any
/// unconditional jump instruction and the next label.
pub fn eliminate(info: &mut GlobalCompilerInfo) {
    for function in info.mut_functions() {
        let bytes = function.mut_bytes();
        eliminate_jumps(bytes);
        eliminate_post_jump(bytes);
    }
}

/// Eliminates all jumps that jump to the next instruction.
///
/// Any code of the form
/// ```text
///     JUMP* label
/// label:
///     more_code
/// ```
/// is unnecessary, as no matter what the condition is, execution will flow to
/// the next statement.
fn eliminate_jumps(bytes: &mut BytecodeList) {
    loop {
        let mut changed = false;
        let mut byte_index = 0;
        bytes.set_labels();
        let mut to_remove = HashSet::new();
        for (index, bytecode) in bytes.enumerate() {
            byte_index += bytecode.size();
            if let &[BytecodeRef::Location(l), ..] = &*bytecode.get_operands() {
                if l.get_label().get_value() == byte_index {
                    // FIXME: `JUMP_TRUE next_label` requires a POP_TOP as replacement
                    //        (ditto for equivalents)
                    to_remove.insert(index);
                    changed = true;
                }
            }
        }
        bytes.remove_all(to_remove);
        if !changed {
            break;
        }
    }
}

/// Eliminates all code after an unconditional jump statement.
///
/// Any code of the form
/// ```text
///     JUMP label1
///     (some code goes here)
/// label2:
///     (more code)
/// ```
/// can have the intervening code eliminated. This is because all jump
/// statements must point to a label. If there is no label after a jump, it
/// therefore is impossible for any code to reach it. It is not necessary for
/// the jump statement to point to the next label reached. As such, the above
/// code can be optimized to
/// ```text
///     JUMP label1
/// label2:
///     (more code)
/// ```
fn eliminate_post_jump(bytes: &mut BytecodeList) {
    let mut to_remove = Vec::new();
    for (index, bytecode) in bytes.enumerate() {
        if let Bytecode::Jump(_) | Bytecode::Return(_) = bytecode {
            to_remove.push((index.next(), bytes.next_label(index)));
        }
    }
    if !to_remove.is_empty() {
        todo!("Need to deal with eliminating ranges without invalidating indices")
    }
}
