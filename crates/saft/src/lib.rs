use std::rc::Rc;

use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use saft_bytecode::{chunk::Chunk, vm::Vm};

use saft_bytecode as bytecode;
use saft_ir::ir;
use saft_ir::lowerer::Lowerer;
use saft_syntax::{ast, parser::Parser};

pub struct Saft {
    lowerer: Lowerer<bytecode::value::NativeFunction>,
    compiler: bytecode::compiler::Compiler,
    vm: Vm,
    diagnostic_writer: StandardStream,
    diagnostic_config: codespan_reporting::term::Config,
}

#[allow(clippy::new_without_default)]
impl Saft {
    /// Create a new saft instance, does not contain any prelude or anything.
    pub fn new() -> Self {
        Self {
            lowerer: Lowerer::new(),
            compiler: bytecode::compiler::Compiler::new(),
            vm: Vm::new(),
            diagnostic_writer: codespan_reporting::term::termcolor::StandardStream::stdout(
                codespan_reporting::term::termcolor::ColorChoice::Auto,
            ),
            diagnostic_config: codespan_reporting::term::Config::default(),
        }
    }

    /// Create a saft instance with the standard library
    pub fn new_with_std() -> Self {
        let mut saft = Self::new();
        saft.add_native(saft_bytecode::natives::print);
        saft
    }

    /// Add a native function to the saft interpreter/compiler. This inserts it into the global
    /// namespace.
    ///
    /// * `native`: The native function
    fn add_native(&mut self, native: saft_bytecode::value::NativeFunction) {
        self.lowerer
            .add_item(native.name.to_string(), ir::Item::Builtin(native));
    }

    /// Try to parse a file with a file name and content.
    ///
    /// If the parsing fails this will exit and print an error to stderr.
    ///
    /// * `fname`: The name of the file
    /// * `s`: The content of the file
    fn try_parse(&mut self, fname: &str, s: &str) -> Option<ast::Module> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        match Parser::new(s).parse_file() {
            Ok(module) => Some(module),
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                None
            }
        }
    }

    /// Try to lower a module into a bytecode chunk. If it fails it prints an error to stderr.
    ///
    /// * `fname`: The file name
    /// * `s`: The file contents
    /// * `module`: The parsed module
    fn try_lower_and_compile(
        &mut self,
        fname: &str,
        s: &str,
        module: &ast::Module,
    ) -> Option<bytecode::chunk::Chunk> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        let mut lowerer = self.lowerer.clone();

        let ir = match lowerer.lower_module(module) {
            Ok(ir) => ir,
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                return None;
            }
        };

        let mut compiler = self.compiler.clone();

        let chunk = match compiler.compile_module(&ir, &lowerer.items) {
            Ok(chunk) => chunk,
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer.lock(),
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                return None;
            }
        };

        self.lowerer = lowerer;
        self.compiler = compiler;

        Some(chunk)
    }

    /// Try to interpret a chunk of bytecode. If any error occurs it prints it to stderr.
    ///
    /// * `fname`: The file name
    /// * `s`: The file contents
    /// * `chunk`: The bytecode chunk
    fn try_interpret(&mut self, fname: &str, s: &str, chunk: Rc<Chunk>) -> Option<()> {
        let mut files = SimpleFiles::new();
        let id = files.add(fname, s);

        let constants = &self.compiler.constants;

        match self.vm.interpret_chunk(chunk, constants) {
            Ok(()) => Some(()),
            Err(err) => {
                term::emit(
                    &mut self.diagnostic_writer,
                    &self.diagnostic_config,
                    &files,
                    &err.diagnostic(id),
                )
                .unwrap();
                None
            }
        }
    }

    /// Parse, lower and interpret a source.
    ///
    /// * `fname`: The file name
    /// * `s`: The file contents
    pub fn interpret_module(&mut self, fname: &str, s: &str) -> Option<()> {
        let ast = self.try_parse(fname, s)?;
        let chunk = self.try_lower_and_compile(fname, s, &ast)?;
        self.try_interpret(fname, s, Rc::new(chunk));

        Some(())
    }
}
