use saft::lang::eval::Evaluator;

fn main() {
    let mut evaluator = Evaluator::new();
    saft::cli::repl::repl(&mut evaluator);
}
