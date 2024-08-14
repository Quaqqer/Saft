use ariadne::{Label, Report, ReportKind, Source};
use clap::Parser;

use crate::lang::{eval::Evaluator, saft};

mod args;
mod repl;

pub fn cli() {
    let args = args::Args::parse();

    match args.command {
        args::Command::Run { file, interactive } => {
            let mut evaluator = Evaluator::new();
            let src = std::fs::read_to_string(&file).unwrap();
            match saft::eval_module(&mut evaluator, &src) {
                Ok(()) => {}
                Err(e) => {
                    let fname = file.as_os_str().to_str().unwrap();

                    Report::build(ReportKind::Error, fname, 0)
                        .with_message(e.kind)
                        .with_label(
                            Label::new((fname, e.span.start()..e.span.end())).with_message(e.msg),
                        )
                        .finish()
                        .eprint((fname, Source::from(&src)))
                        .unwrap();
                }
            };

            if interactive {
                repl::repl(&mut evaluator);
            }
        }
        args::Command::Repl {} => {
            let mut evaluator = Evaluator::new();
            repl::repl(&mut evaluator);
        }
    }
}
