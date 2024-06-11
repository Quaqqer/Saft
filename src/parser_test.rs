#[cfg(test)]
mod tests {
    use crate::{parser, sexpr::SExpr};

    #[test]
    fn precedence() {
        fn check_sexpr(source: &str, sexpr: &str) {
            assert_eq!(
                SExpr::from(&parser::SpannedExprParser::new().parse(source).unwrap().v).to_string(),
                sexpr
            )
        }

        check_sexpr("1 + 2", "(+ 1 2)");
        check_sexpr("1 + 2 + 3", "(+ (+ 1 2) 3)");
        check_sexpr("1 + 2 * 3 / 4 - 3 + 1", "(+ (- (+ 1 (/ (* 2 3) 4)) 3) 1)");
    }

    #[test]
    fn modules() {
        fn check_module(source: &str, sexpr: &str) {
            assert_eq!(
                SExpr::from(&parser::SpannedModuleParser::new().parse(source).unwrap().v)
                    .to_string(),
                sexpr
            );
        }

        check_module(
            include_str!("../res/query_test/main.saft"),
            "(module ((fn main () ((let a 2)))))",
        );

        check_module(
            include_str!("../res/query_test/lib/asserts.saft"),
            "(module ((fn assert (cond) ((expr (call (. core assert) (cond)))))))",
        );
    }
}
