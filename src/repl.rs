use crate::{ast::StmtOrExpr, parser::SpannedStmtOrExprParser, span::Spanned};
use ariadne::{Label, Report, ReportKind, Source};
use directories::ProjectDirs;
use rustyline::{error::ReadlineError, DefaultEditor};

use crate::eval::Evaluator;
use crate::saft;

pub fn repl(evaluator: &mut Evaluator) {
    let mut rl = DefaultEditor::new().expect("Should be to run readline repl");

    let dirs = ProjectDirs::from("com", "saft-lang", "saft")
        .expect("Should be able to create project dirs");

    let history_path = dirs
        .state_dir()
        .expect("Should have state directory")
        .join("repl_history.txt");

    let _ = rl.load_history(&history_path);

    loop {
        let readline = rl.readline("> ");

        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);

                let res = saft::eval_stmt_or_expr(evaluator, &line);

                if let Err(saft_err) = res {
                    Report::build(ReportKind::Error, "stdin", 0)
                        .with_message(saft_err.kind)
                        .with_label(
                            Label::new(("stdin", saft_err.span.start()..saft_err.span.end()))
                                .with_message(saft_err.msg),
                        )
                        .finish()
                        .print(("stdin", Source::from(line)))
                        .unwrap();
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                panic!("Got an error in readline")
            }
        }
    }

    let _ = rl.save_history(&history_path);
}
