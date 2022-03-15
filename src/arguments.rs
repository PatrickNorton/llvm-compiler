use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use crate::util::version::CURRENT_VERSION;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Optimization {
    ConstBytes,
    DeadCode,
    DeadStore,
    CommonSubexpr,
    InlineFunctions,
    InlineFnOnce,
    InlineSmallFn,
    PureConst,
}

#[derive(Debug, Clone)]
pub enum CLArgError {
    MultipleDebug,
    MultipleBytecode,
    MultipleStdlib,
    UnknownOptimization(String),
    OptimizationRedef(Optimization),
    OptLevelRedef,
    Illegal(String),
}

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
    pub fn opt_is_enabled(&self, opt: Optimization) -> bool {
        self.explicit_opts.get(&opt).cloned().unwrap_or_else(|| {
            OPT_LIST[..=self.opt_level as usize]
                .iter()
                .any(|x| x.contains(&opt))
        })
    }

    pub fn target(&self) -> &Path {
        &self.target
    }

    pub fn is_debug(&self) -> bool {
        self.is_debug
    }

    pub fn is_test(&self) -> bool {
        self.is_test
    }

    pub fn cfg_options(&self) -> &HashSet<String> {
        &self.cfg_options
    }

    pub fn should_print_bytecode(&self) -> bool {
        self.print_bytecode
    }

    pub fn get_bytecode_path(&self) -> &Option<PathBuf> {
        &self.bytecode_path
    }

    pub fn stdlib_path(&self) -> &Option<PathBuf> {
        &self.stdlib_path
    }

    pub fn parse(mut args: impl Iterator<Item = String>) -> Result<CLArgs, CLArgError> {
        let _this = args.next().unwrap();
        let file = PathBuf::from(args.next().unwrap());
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
                    let cfg_val = args.next().unwrap();
                    cfg_options.insert(cfg_val);
                }
                "-V" | "--version" => {
                    println!("Version: {}", CURRENT_VERSION)
                }
                "--print-bytecode" => {
                    print_bytecode = true;
                }
                "-S" => {
                    if bytecode_path.is_some() {
                        return Err(CLArgError::MultipleBytecode);
                    } else {
                        bytecode_path = Some(PathBuf::from(args.next().unwrap()))
                    }
                }
                "-stdlib" => {
                    if stdlib_path.is_some() {
                        return Err(CLArgError::MultipleStdlib);
                    } else {
                        stdlib_path = Some(PathBuf::from(args.next().unwrap()));
                    }
                }
                _ => {
                    if let Option::Some(rest) = arg.strip_prefix("-f") {
                        update_optimizations(rest, &mut optimizations, true)?;
                    } else if let Option::Some(rest) = arg.strip_prefix("-F") {
                        update_optimizations(rest, &mut optimizations, false)?;
                    } else if let Option::Some(rest) = arg.strip_prefix("-O") {
                        let level = rest.parse().unwrap();
                        check_opt_level(&opt_level)?;
                        if level > 3 {
                            println!("Warning: -O{} is equivalent to -O3", level);
                            opt_level = Some(3);
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

impl Error for CLArgError {}

impl Display for CLArgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CLArgError::MultipleDebug => f.write_str("Debug defined multiple times"),
            CLArgError::MultipleBytecode => f.write_str("Redefinition of bytecode path"),
            CLArgError::MultipleStdlib => f.write_str("Redefinition of stdlib path"),
            CLArgError::UnknownOptimization(x) => write!(f, "Unknown optimization option {}", x),
            CLArgError::OptimizationRedef(o) => {
                write!(f, "Redefinition of optimization option {}", o.name())
            }
            CLArgError::OptLevelRedef => f.write_str("Optimization level defined multiple times"),
            CLArgError::Illegal(arg) => write!(f, "Illegal argument {}", arg),
        }
    }
}
