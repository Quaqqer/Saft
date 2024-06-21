use saft::eval::Evaluator;

fn main() {
    let mut evaluator = Evaluator::new();
    saft::repl::repl(&mut evaluator);
}
