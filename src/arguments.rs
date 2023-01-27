use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::Display;
use std::num::ParseIntError;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use crate::util::version::CURRENT_VERSION;

// TODO? Rewrite using clap
/// The result of parsing command line arguments.
///
/// # Arguments
/// ```text
/// -t, --test          Compile in test mode
/// --debug             Turn on debug mode (with assertions)
/// --ndebug            Turn off debug mode (no assertions)
/// -O0, -O1, -O2, -O3  Set the optimization level
/// --cfg <argument>    Enable the given cfg value
/// --print-bytecode    Print the compiled bytecode to stdout
/// -S <file>           Write the bytecode in text form to the given file
/// --stdlib <file>     The directory containing the standard library
/// -f<optimization>    Turn on the given optimization pass
/// -F<optimization>    Turn off the given optimization pass
/// ```
#[derive(Debug)]
pub struct CLArgs {
    target: PathBuf,
    is_test: bool,
    is_debug: bool,
    opt_level: u32,
    explicit_opts: HashMap<Optimization, bool>,
    cfg_options: HashSet<String>,
    print_bytecode: bool,
    bytecode_path: Option<PathBuf>,
    stdlib_path: Option<PathBuf>,
}

/// A codegen optimization.
///
/// These are the ones triggered by `-f` flags in the argument list, e.g.
/// passing `-fdce` will cause `Optimization::DeadStore` to be run.
///
/// # Examples
/// ```
/// assert_eq!(Optimization::parse("dce"), Ok(Optimization::DeadStore));
/// assert_eq!(Optimization::parse("pure-const"), Ok(Optimization::PureConst));
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Optimization {
    /// The optimization for deduplicating const `bytes` literals.
    ///
    /// Because `bytes` is not a `const` class, `bytes` literals cannot
    /// typically be interned as constants in the bytecode (mutating an interned
    /// `bytes` literal can cause UB). This allows `bytes` literals that are
    /// never mutated to be interned as constants.
    ConstBytes,

    /// The optimization for removing dead code.
    ///
    /// Dead code is any code that cannot be executed. At the moment, the only
    /// dead-code passes done are for code between unconditional jumps and
    /// labels, as well as jumps to the next statement.
    DeadCode,

    /// The optimization for removing dead stores.
    ///
    /// A dead store is any store, for example via `Bytecode::Store`, that is
    /// never referenced or only referenced in a way that can keep it on the
    /// stack.
    DeadStore,

    /// The optimization for common-subexpression elimination.
    ///
    /// This optimization attempts to spot common expressions that are repeated
    /// multiple times in code and precompute them, in order to avoid the cost
    /// of multiple computation.
    CommonSubexpr,

    /// The optimization for inlining functions.
    ///
    /// This optimization takes the bytecode of certain functions and copies
    /// them into their callers. This prevents the overhead of function calls
    /// and, when combined with other optimizations, can cause significant other
    /// speedups as well.
    ///
    /// When defined, this optimization implies
    /// [`-finline-functions-called-once`](Self::InlineFnOnce) and
    /// [`-finline-small-functions`](Self::InlineSmallFn).
    InlineFunctions,

    /// The optimization for inlining functions that are called once.
    ///
    /// This is a subset of [`-finline-functions`](Self::InlineFunctions), the
    /// difference being that this only attempts to inline functions that are
    /// called once throughout the codebase.
    InlineFnOnce,

    /// The optimization for inlining small functions.
    ///
    /// This is a subset of [`-finline-functions`](Self::InlineFunctions), the
    /// difference being that this has a much smaller threshold for compiled
    /// function size.
    InlineSmallFn,

    /// The optimization for optimizing `pure` and constant-returning functions.
    ///
    /// A `pure` function is a function whose output is fully deterministic
    /// based on its input: i.e. passing in the same set of inputs will always
    /// give the same output. Additionally, a `pure` function must have no
    /// visible side effects. These functions can be optimized away in the same
    /// way as [common-subexpression elimination](Self::CommonSubexpr).
    ///
    /// This optimization implies [`-fgsce`](Self::CommonSubexpr).
    PureConst,
}

/// An error from parsing command-line arguments.
#[derive(Debug, Clone)]
pub enum CLArgError {
    NotEnoughArgs,
    MultipleDebug,
    MultipleBytecode,
    MultipleStdlib,
    UnknownOptimization(String),
    OptimizationRedef(Optimization),
    OptLevelRedef,
    MissingValue,
    InvalidOptLevel(ParseIntError),
    RedefinedCfg(String),
    Illegal(String),
}

const MAX_OPT: u32 = OPT_LIST.len() as u32;

const O0_OPTIMIZATIONS: &[Optimization] = &[];

const O1_OPTIMIZATIONS: &[Optimization] = &[
    Optimization::ConstBytes,
    Optimization::DeadCode,
    Optimization::DeadStore,
    Optimization::InlineFnOnce,
    Optimization::PureConst,
];

const O2_OPTIMIZATIONS: &[Optimization] = &[
    Optimization::CommonSubexpr,
    Optimization::InlineFunctions,
    Optimization::InlineSmallFn,
];

const O3_OPTIMIZATIONS: &[Optimization] = &[];

const OPT_LIST: &[&[Optimization]] = &[
    O0_OPTIMIZATIONS,
    O1_OPTIMIZATIONS,
    O2_OPTIMIZATIONS,
    O3_OPTIMIZATIONS,
];

impl CLArgs {
    /// Whether or not the given optimization was enabled by the user.
    pub fn opt_is_enabled(&self, opt: Optimization) -> bool {
        self.explicit_opts.get(&opt).cloned().unwrap_or_else(|| {
            OPT_LIST[..=self.opt_level as usize]
                .iter()
                .any(|x| x.contains(&opt))
        })
    }

    /// The target location for the final bytecode to be written.
    pub fn target(&self) -> &Path {
        &self.target
    }

    /// If debug mode is on.
    ///
    /// In debug mode, assertions are enabled, and `$cfg(debug)` is true.
    pub fn is_debug(&self) -> bool {
        self.is_debug
    }

    /// If this is compiling a test binary or not.
    ///
    /// If this is compiling a test binary, `$cfg(debug)` is true and all
    /// functions with `$test` will be run as tests.
    pub fn is_test(&self) -> bool {
        self.is_test
    }

    /// The set of options passed by `--cfg foo`.
    ///
    /// `$cfg(foo)` is true if and only if `"foo"` is a member of
    /// `cfg_options()`.
    pub fn cfg_options(&self) -> &HashSet<String> {
        &self.cfg_options
    }

    /// Whether or not the bytecode disassembly should be printed to stdout.
    ///
    /// This is controlled by the `--print-bytecode` command-line flag.
    pub fn should_print_bytecode(&self) -> bool {
        self.print_bytecode
    }

    /// The path to write the bytecode output to, if present.
    ///
    /// If this is absent, no bytecode output should be written, excluding that
    /// given by the `--print-bytecode` flag.
    pub fn get_bytecode_path(&self) -> &Option<PathBuf> {
        &self.bytecode_path
    }

    /// The path to the standard library, if present.
    ///
    /// If no stdlib path was given, the default stdlib path should be used.
    /// This is controlled by the `--stdlib` flag.
    pub fn stdlib_path(&self) -> &Option<PathBuf> {
        &self.stdlib_path
    }

    /// Parses the [`CLArgs`] from an iterator of strings.
    ///
    /// For arguments and their meanings, see the [`CLArgs`] documentation.
    ///
    /// # Examples
    /// ```
    /// let args = ArgumentInfo::parse(std::env::args());
    /// ```
    pub fn parse(args: impl IntoIterator<Item = String>) -> Result<CLArgs, CLArgError> {
        let mut args = args.into_iter();
        let _this = args.next().ok_or(CLArgError::NotEnoughArgs)?;
        let file = PathBuf::from(args.next().ok_or(CLArgError::NotEnoughArgs)?);
        let mut test = false;
        let mut debug = None;
        let mut opt_level = None;
        let mut optimizations = HashMap::new();
        let mut cfg_options = HashSet::new();
        let mut print_bytecode = false;
        let mut bytecode_path = None;
        let mut stdlib_path = None;
        while let Option::Some(arg) = args.next() {
            match &*arg {
                "--test" | "-t" => test = true,
                "--ndebug" => {
                    if debug.is_some() {
                        return Err(CLArgError::MultipleDebug);
                    } else {
                        debug = Some(false);
                    }
                }
                "--help" => print_help(),
                "--debug" => {
                    if debug.is_some() {
                        return Err(CLArgError::MultipleDebug);
                    } else {
                        debug = Some(true);
                    }
                }
                "-O0" => {
                    check_opt_level(&opt_level)?;
                    opt_level = Some(0);
                }
                "-O1" => {
                    check_opt_level(&opt_level)?;
                    opt_level = Some(1);
                }
                "-O2" => {
                    check_opt_level(&opt_level)?;
                    opt_level = Some(2);
                }
                "-O3" => {
                    check_opt_level(&opt_level)?;
                    opt_level = Some(3);
                }
                "--cfg" => {
                    let cfg_val = args.next().ok_or(CLArgError::MissingValue)?;
                    if cfg_options.contains(&cfg_val) {
                        return Err(CLArgError::RedefinedCfg(cfg_val));
                    } else {
                        cfg_options.insert(cfg_val);
                    }
                }
                "-V" | "--version" => {
                    println!("Version: {CURRENT_VERSION}")
                }
                "--print-bytecode" => {
                    print_bytecode = true;
                }
                "-S" => {
                    if bytecode_path.is_some() {
                        return Err(CLArgError::MultipleBytecode);
                    } else {
                        bytecode_path =
                            Some(PathBuf::from(args.next().ok_or(CLArgError::MissingValue)?))
                    }
                }
                "--stdlib" => {
                    if stdlib_path.is_some() {
                        return Err(CLArgError::MultipleStdlib);
                    } else {
                        stdlib_path =
                            Some(PathBuf::from(args.next().ok_or(CLArgError::MissingValue)?));
                    }
                }
                _ => {
                    if let Option::Some(rest) = arg.strip_prefix("-f") {
                        update_optimizations(rest, &mut optimizations, true)?;
                    } else if let Option::Some(rest) = arg.strip_prefix("-F") {
                        update_optimizations(rest, &mut optimizations, false)?;
                    } else if let Option::Some(rest) = arg.strip_prefix("-O") {
                        let level = rest.parse().map_err(CLArgError::InvalidOptLevel)?;
                        check_opt_level(&opt_level)?;
                        if level > MAX_OPT {
                            eprintln!("Warning: -O{level} is equivalent to -O{MAX_OPT}");
                            opt_level = Some(MAX_OPT);
                        } else {
                            opt_level = Some(level);
                        }
                    } else {
                        return Err(CLArgError::Illegal(arg));
                    }
                }
            }
        }
        Ok(CLArgs {
            target: file,
            is_test: test,
            is_debug: debug.unwrap_or(test),
            opt_level: opt_level.unwrap_or(0),
            explicit_opts: optimizations,
            cfg_options,
            print_bytecode,
            bytecode_path,
            stdlib_path,
        })
    }
}

impl Optimization {
    /// The name of the optimization.
    pub fn name(&self) -> &'static str {
        match self {
            Optimization::ConstBytes => "const-bytes-object",
            Optimization::DeadCode => "dce",
            Optimization::DeadStore => "dse",
            Optimization::CommonSubexpr => "gcse",
            Optimization::InlineFunctions => "inline-functions",
            Optimization::InlineFnOnce => "inline-functions-called-once",
            Optimization::InlineSmallFn => "inline-small-functions",
            Optimization::PureConst => "pure-const",
        }
    }
}

#[inline]
fn check_opt_level(level: &Option<u32>) -> Result<(), CLArgError> {
    if level.is_some() {
        Err(CLArgError::OptLevelRedef)
    } else {
        Ok(())
    }
}

fn update_optimizations(
    name: &str,
    optimizations: &mut HashMap<Optimization, bool>,
    is_valid: bool,
) -> Result<(), CLArgError> {
    let optimization = Optimization::from_str(name)
        .map_err(|_| CLArgError::UnknownOptimization(name.to_string()))?;
    match optimizations.entry(optimization) {
        Entry::Occupied(_) => Err(CLArgError::OptimizationRedef(optimization)),
        Entry::Vacant(v) => {
            v.insert(is_valid);
            Ok(())
        }
    }
}

// TODO: Improve help message (move to separate file?)
fn print_help() {
    println!(
        "Help:

-t, --test          Compile in test mode
--debug             Turn on debug mode (with assertions)
--ndebug            Turn off debug mode (no assertions)
-O0, -O1, -O2, -O3  Set the optimization level
--cfg <argument>    Enable the given cfg value
--print-bytecode    Print the compiled bytecode to stdout
-S <file>           Write the bytecode in text form to the given file
--stdlib <file>     The directory containing the standard library
-f<optimization>    Turn on the given optimization pass
-F<optimization>    Turn off the given optimization pass"
    );
}

impl FromStr for Optimization {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "const-bytes-object" => Ok(Optimization::ConstBytes),
            "dce" => Ok(Optimization::DeadCode),
            "dse" => Ok(Optimization::DeadStore),
            "gcse" => Ok(Optimization::CommonSubexpr),
            "inline-functions" => Ok(Optimization::InlineFunctions),
            "inline-functions-called-once" => Ok(Optimization::InlineFnOnce),
            "inline-small-functions" => Ok(Optimization::InlineSmallFn),
            "pure-const" => Ok(Optimization::PureConst),
            _ => Err(()),
        }
    }
}

impl Display for Optimization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name().fmt(f)
    }
}

impl Error for CLArgError {}

impl Display for CLArgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CLArgError::NotEnoughArgs => f.write_str("Not enough arguments passed"),
            CLArgError::MultipleDebug => f.write_str("Debug defined multiple times"),
            CLArgError::MultipleBytecode => f.write_str("Redefinition of bytecode path"),
            CLArgError::MultipleStdlib => f.write_str("Redefinition of stdlib path"),
            CLArgError::UnknownOptimization(x) => write!(f, "Unknown optimization option {x}"),
            CLArgError::OptimizationRedef(o) => {
                write!(f, "Redefinition of optimization option {o}")
            }
            CLArgError::OptLevelRedef => f.write_str("Optimization level defined multiple times"),
            CLArgError::MissingValue => f.write_str("Argument expected value but didn't get one"),
            CLArgError::InvalidOptLevel(e) => write!(f, "Invalid optimization level: {e}"),
            CLArgError::RedefinedCfg(c) => write!(f, "Redefined 'cfg' option {c}"),
            CLArgError::Illegal(arg) => write!(f, "Illegal argument {arg}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::iter::{empty, once};
    use std::path::Path;

    use itertools::Itertools;

    use crate::arguments::CLArgError;
    use crate::macros::hash_set;

    use super::CLArgs;

    #[test]
    fn simple_args() {
        let args = [String::new(), "test".to_string()];
        let result = CLArgs::parse(args).expect("Argument parsing failed");
        assert_eq!(result.target(), AsRef::<Path>::as_ref("test"));
        assert!(!result.is_debug());
        assert!(!result.is_test());
        assert_eq!(result.cfg_options(), &HashSet::new());
        assert!(!result.should_print_bytecode());
        assert_eq!(result.get_bytecode_path(), &None);
        assert_eq!(result.stdlib_path(), &None);
    }

    #[test]
    fn no_args() {
        // TODO: feature(assert_matches) (#82775)
        assert!(matches!(
            CLArgs::parse(empty()),
            Err(CLArgError::NotEnoughArgs)
        ));
        assert!(matches!(
            CLArgs::parse(once("Foo".to_string())),
            Err(CLArgError::NotEnoughArgs)
        ));
    }

    #[test]
    fn debug_arg() {
        let ndebug = CLArgs::parse([String::new(), String::new(), "--ndebug".to_string()]).unwrap();
        assert!(!ndebug.is_debug());
        let debug = CLArgs::parse([String::new(), String::new(), "--debug".to_string()]).unwrap();
        assert!(debug.is_debug());
        assert!(matches!(
            CLArgs::parse([
                String::new(),
                String::new(),
                "--debug".to_string(),
                "--ndebug".to_string()
            ]),
            Err(CLArgError::MultipleDebug)
        ));
    }

    #[test]
    fn multiple_opt() {
        for args in ["-O0", "-O1", "-O2", "-O3"].into_iter().permutations(2) {
            let args = [
                String::new(),
                String::new(),
                args[0].to_string(),
                args[1].to_string(),
            ];
            assert!(matches!(
                CLArgs::parse(args),
                Err(CLArgError::OptLevelRedef)
            ))
        }
    }

    #[test]
    fn invalid_opt() {
        let args = [String::new(), String::new(), "-Otest".to_string()];
        assert!(matches!(
            CLArgs::parse(args),
            Err(CLArgError::InvalidOptLevel(_))
        ));
        let args = [String::new(), String::new(), "-O0x10".to_string()];
        assert!(matches!(
            CLArgs::parse(args),
            Err(CLArgError::InvalidOptLevel(_))
        ));
        let args = [String::new(), String::new(), "-O2_2".to_string()];
        assert!(matches!(
            CLArgs::parse(args),
            Err(CLArgError::InvalidOptLevel(_))
        ));
    }

    #[test]
    fn print_bytecode() {
        let args = [String::new(), String::new(), "--print-bytecode".to_string()];
        assert!(CLArgs::parse(args).unwrap().should_print_bytecode());
    }

    #[test]
    fn cfg_options() {
        let args = [
            String::new(),
            String::new(),
            "--cfg".to_string(),
            "test".to_string(),
        ];
        assert_eq!(
            CLArgs::parse(args).unwrap().cfg_options(),
            &hash_set!("test".into())
        );
        let double_distinct = [
            String::new(),
            String::new(),
            "--cfg".to_string(),
            "test".to_string(),
            "--cfg".to_string(),
            "test-2".to_string(),
        ];
        assert_eq!(
            CLArgs::parse(double_distinct).unwrap().cfg_options(),
            &hash_set!("test".into(), "test-2".into())
        );
    }

    #[test]
    fn cfg_failures() {
        let double_cfg = [
            String::new(),
            String::new(),
            "--cfg".to_string(),
            "test".to_string(),
            "--cfg".to_string(),
            "test".to_string(),
        ];
        assert!(matches!(
            CLArgs::parse(double_cfg),
            Err(CLArgError::RedefinedCfg(_))
        ));
        let missing_path = [String::new(), String::new(), "--cfg".to_string()];
        assert!(matches!(
            CLArgs::parse(missing_path),
            Err(CLArgError::MissingValue)
        ));
    }

    #[test]
    fn bytecode_path() {
        let args = [
            String::new(),
            String::new(),
            "-S".to_string(),
            "test".to_string(),
        ];
        assert_eq!(
            CLArgs::parse(args).unwrap().get_bytecode_path(),
            &Some("test".into())
        );
        let double_path = [
            String::new(),
            String::new(),
            "-S".to_string(),
            "test".to_string(),
            "-S".to_string(),
            "test/2".to_string(),
        ];
        assert!(matches!(
            CLArgs::parse(double_path),
            Err(CLArgError::MultipleBytecode)
        ));
        let missing_path = [String::new(), String::new(), "-S".to_string()];
        assert!(matches!(
            CLArgs::parse(missing_path),
            Err(CLArgError::MissingValue)
        ));
    }
}
