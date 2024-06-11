use crate::{ast, span::Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i32),
    Bool(bool),
}

impl Value {
    pub fn ty(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Bool(_) => ValueType::Bool,
        }
    }
}

pub enum ValueType {
    Int,
    Bool,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Int => write!(f, "int"),
            ValueType::Bool => write!(f, "bool"),
        }
    }
}

type Res<T> = Result<T, EvaluatorError>;

#[derive(Debug)]
pub enum EvaluatorError {
    Text(String, Span),
}

pub struct Evaluator {}

macro_rules! bail {
    ($span:expr, $($fmts:expr),+) => {
        return Err(EvaluatorError::Text(format!($($fmts),*), $span.clone()))
    };
}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval_expr(&mut self, expr: &ast::Expr, span: &Span) -> Res<Value> {
        macro_rules! binop {
            ($lhs:expr, $rhs:expr, $binop:expr, $closure:expr) => {{
                let lhs = self.eval_expr(&$lhs.v, &$lhs.s)?;
                let rhs = self.eval_expr(&$rhs.v, &$lhs.s)?;

                let e_msg = format!(
                    "Binary operation {} not supported between types {} and {}",
                    $binop,
                    lhs.ty(),
                    rhs.ty()
                );

                match (lhs, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int($closure(lhs, rhs)),
                    (_, _) => bail!(span, "{}", e_msg),
                }
            }};
        }

        Ok(match expr {
            ast::Expr::Int(i) => Value::Int(*i),
            ast::Expr::Bool(b) => Value::Bool(*b),
            ast::Expr::Var(ident) => match self.lookup(&ident.v) {
                Some(val) => val.clone(),
                None => {
                    return Err(EvaluatorError::Text(
                        format!("No variable named {} has been declared", ident.v),
                        ident.s,
                    ))
                }
            },
            ast::Expr::Call { expr, args } => todo!(),
            ast::Expr::Access { expr, field } => todo!(),
            ast::Expr::Add(lhs, rhs) => binop!(lhs, rhs, "+", |lhs, rhs| lhs + rhs),
            ast::Expr::Sub(lhs, rhs) => binop!(lhs, rhs, "-", |lhs, rhs| lhs - rhs),
            ast::Expr::Mul(lhs, rhs) => binop!(lhs, rhs, "*", |lhs, rhs| lhs * rhs),
            ast::Expr::Div(lhs, rhs) => binop!(lhs, rhs, "/", |lhs, rhs| lhs / rhs),
        })
    }

    fn lookup(&self, ident: &str) -> Option<&Value> {
        todo!()
    }

    fn declare(&mut self, ident: String, value: Value) {
        todo!()
    }

    fn assign(&mut self, ident: &str, value: Value) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast, span::Spanned};

    use super::{Evaluator, Value};

    fn parse_expr(source: &'static str) -> Spanned<ast::Expr> {
        crate::parser::SpannedExprParser::new()
            .parse(source)
            .unwrap()
    }

    fn eval_expr(source: &'static str) -> Value {
        let ast = parse_expr(source);
        let mut evaluator = Evaluator::new();
        evaluator.eval_expr(&ast.v, &ast.s).unwrap()
    }

    #[test]
    fn binop() {
        assert_eq!(eval_expr("1 + 2 + 3"), Value::Int(6));
    }
}
