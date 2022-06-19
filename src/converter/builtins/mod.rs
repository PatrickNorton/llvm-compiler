mod globals;
mod macros;
mod normal;
mod parsed;
mod reference;

pub use self::globals::{
    auto_interfaces, GlobalBuiltins, CALLABLE, FALSE, NULL_TYPE, OBJECT, STABLE_FEATURES,
    THROWS_TYPE, TRUE,
};
pub use self::normal::Builtins;
pub use self::parsed::ParsedBuiltins;
pub use self::reference::BuiltinRef;

// High-level overview of builtins and how they may differ from the Java version:
// * Here, Builtins is a class that is created for each GlobalCompilerInfo
// * Values not created in __builtins__.newlang are global constants (maybe
//    clone them into each instance of Builtins so there's no Lazy syncing?)
// * Values created in __builtins__.newlang are part of an InnerBuiltins class,
//    which is given to the ImportHandler for __builtins__ (with type
//    Option<Box<ParsedBuiltins>>), and then transferred to the GlobalCompilerInfo
//    after parsing is complete, with the type changed to InnerBuiltins.
// * The Builtins class, which is part of GlobalCompilerInfo, has a field of
//    type OnceCell<InnerBuiltins>, which is filled once parsing of __builtins__
//    is complete.
// It might make more sense to have this be an Arc somehow and cached locally in
// each CompilerInfo (so as to avoid the synchronization on the global OnceCell).
// As an example, have each Compiler have a field `builtins:
// Option<Arc<Builtins>>`, and then
// ```
// pub fn get_builtins(&mut self) -> &Builtins {
//     self.builtins.get_or_insert_with(|| {
//         self.global_info.get_builtins().clone()
//     })
// }
// ```
// or something similar.
//
// In the Java version, everything is global. Since this would involve a huge
// amount of inter-process synchronization, it makes more sense to have the
// values cached locally, since they won't change post-initialization.

pub const FORBIDDEN_NAMES: &[&str] = &[
    "true",
    "false",
    "__default__",
    "self",
    "cls",
    "super",
    "option",
    "null",
];
