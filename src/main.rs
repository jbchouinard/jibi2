use std::path::PathBuf;
use std::str::FromStr;

use home::home_dir;
use lazy_static::lazy_static;
use rustyline::Editor;
use structopt::StructOpt;

use jibi2::reader::{LexError, Token, TokenValidator};
use jibi2::vm::VM;

const VERSION: &str = env!("CARGO_PKG_VERSION");

lazy_static! {
    static ref HISTORY_FILE: PathBuf = match home_dir() {
        Some(mut p) => {
            p.push(".jibi_history");
            p
        }
        None => {
            eprintln!("could not locate home dir, saving history to current dir");
            PathBuf::from_str(".jibi_history").unwrap()
        }
    };
}

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
    #[structopt(short, long)]
    interactive: bool,
}

#[cfg(debug_trace_compile)]
fn printsize<T>(name: &str) {
    println!("size of {}: {} bytes", name, std::mem::size_of::<T>());
}

fn main() {
    #[cfg(debug_trace_compile)]
    {
        use jibi2::object::{FloatType, Function, IntType, Object};
        use std::rc::Rc;
        printsize::<bool>("Bool");
        printsize::<IntType>("Int");
        printsize::<FloatType>("Float");
        printsize::<String>("String");
        printsize::<Rc<String>>("StringRef");
        printsize::<Function>("Function");
        printsize::<Box<Function>>("FunctionRef");
        printsize::<Object>("Value");
    }
    let Opt { files, interactive } = Opt::from_args();

    let mut vm = VM::new();

    for file in &files {
        let source = std::fs::read_to_string(file).unwrap();
        match vm.interpret_source(&file.to_string_lossy().to_string(), &source) {
            Ok(()) => (),
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }

    if interactive || files.is_empty() {
        repl(&mut vm);
    }
}

fn repl(vm: &mut VM) {
    println!("jibi2 v{}", VERSION);
    let mut rl = Editor::<()>::new();
    let _ = rl.load_history(&*HISTORY_FILE);

    loop {
        match get_tokens(&mut rl) {
            Ok(tokens) => match vm.interpret_tokens(Box::new(tokens.into_iter())) {
                Ok(()) => println!("{}", vm.register0.take().unwrap()),
                Err(e) => eprintln!("{}", e),
            },
            Err(e) => eprintln!("{}", e),
        }
    }
}

/// Get tokens that looks like they form a complete expression (balanced parens)
/// in multiple lines of input if necessary.
fn get_tokens(rl: &mut Editor<()>) -> Result<Vec<Token>, LexError> {
    let mut validator = TokenValidator::new("#STDIN");
    let mut input = readline(rl, ">>> ");
    loop {
        match validator.input(input.to_string()) {
            Ok(Some(v)) => return Ok(v),
            Ok(None) => (),
            Err(e) => return Err(e),
        }
        input = readline(rl, "... ");
    }
}

fn readline(rl: &mut Editor<()>, prompt: &str) -> String {
    let input = match rl.readline(prompt) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    rl.add_history_entry(&input);
    if let Err(e) = rl.save_history(&*HISTORY_FILE) {
        eprintln!("Error saving history file: {}", e)
    }
    input
}
