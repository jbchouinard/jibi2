use std::path::{Path, PathBuf};
use std::str::FromStr;

use home::home_dir;
use rustyline::Editor;
use structopt::StructOpt;

use jibi2::object::debug_print_object_sizes;
use jibi2::reader::{LexError, Token, TokenValidator};
use jibi2::vm::VM;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const TARGET: &str = env!("CARGO_BUILD_TARGET");

const PRELUDE: &str = include_str!("prelude.jibi");

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short, long)]
    interactive: bool,
    #[structopt(long)]
    print_object_sizes: bool,
    #[structopt(parse(from_os_str))]
    files: Vec<PathBuf>,
}

fn main() {
    let Opt {
        files,
        interactive,
        print_object_sizes,
    } = Opt::from_args();

    if print_object_sizes {
        debug_print_object_sizes();
    }

    let mut interpreter = Interpreter::new();

    interpreter.exec_source("#PRELUDE", PRELUDE);

    for file in &files {
        interpreter.exec_file(file);
    }

    if interactive || files.is_empty() {
        interpreter.repl();
    }
}

pub struct Interpreter {
    vm: VM,
    rl: Editor<()>,
    validator: TokenValidator,
    history_file: PathBuf,
}

#[allow(clippy::new_without_default)]
impl Interpreter {
    pub fn new() -> Self {
        Self {
            vm: VM::default(),
            rl: Editor::new(),
            validator: TokenValidator::new("#STDIN"),
            history_file: Self::get_history_file(),
        }
    }

    pub fn repl(&mut self) {
        println!("jibi2 v{} (core v{}, {})", VERSION, jibi2::VERSION, TARGET);
        let _ = self.rl.load_history(&self.history_file);

        loop {
            match self.get_tokens() {
                Ok(tokens) => {
                    match self.vm.load_tokens("#STDIN", Box::new(tokens.into_iter())) {
                        Ok(()) => (),
                        Err(e) => {
                            self.vm.reset();
                            eprintln!("{}", e);
                            continue;
                        }
                    }
                    match self.vm.run() {
                        Ok(Some(res)) => println!("{}", res),
                        Ok(None) => println!("; unspecified return value"),
                        Err(e) => {
                            self.vm.reset();
                            eprintln!("{}", e);
                        }
                    }
                }
                Err(e) => eprintln!("{}", e),
            }
        }
    }

    fn get_history_file() -> PathBuf {
        match home_dir() {
            Some(mut p) => {
                p.push(".jibi_history");
                p
            }
            None => {
                eprintln!("could not locate home dir, saving history to current dir");
                PathBuf::from_str(".jibi_history").unwrap()
            }
        }
    }

    pub fn exec_source(&mut self, name: &str, source: &str) {
        match self.vm.load_source(name, source) {
            Ok(()) => (),
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
        match self.vm.run() {
            Ok(_) => (),
            Err(e) => {
                e.print_trace();
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }

    pub fn exec_file<P: AsRef<Path>>(&mut self, path: P) {
        let file: &Path = path.as_ref();
        let source = std::fs::read_to_string(&file).unwrap();
        let name = file.to_string_lossy();
        self.exec_source(&name.to_string(), &source);
    }

    /// Get tokens that looks like they form a complete expression (balanced parens)
    /// in multiple lines of input if necessary.
    fn get_tokens(&mut self) -> Result<Vec<Token>, LexError> {
        let mut input = self.readline(">>> ");
        loop {
            match self.validator.input(input.to_string()) {
                Ok(Some(v)) => return Ok(v),
                Ok(None) => (),
                Err(e) => return Err(e),
            }
            input = self.readline("... ");
        }
    }

    fn readline(&mut self, prompt: &str) -> String {
        let input = match self.rl.readline(prompt) {
            Ok(input) => input,
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        };
        self.rl.add_history_entry(&input);
        if let Err(e) = self.rl.save_history(&self.history_file) {
            eprintln!("Error saving history file: {}", e)
        }
        input
    }
}
