use crate::{ast, span::Span};

#[derive(Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
}

type Res<T> = Result<T, EvaluatorError>;

pub enum EvaluatorError {
    Text(String, Span),
}

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval_expr(&mut self, expr: &ast::Expr, span: &Span) -> Res<Value> {
        match expr {
            ast::Expr::Int(i) => Ok(Value::Int(*i)),
            ast::Expr::Bool(b) => Ok(Value::Bool(*b)),
            ast::Expr::Var(ident) => match self.lookup(&ident.v) {
                Some(val) => Ok(val.clone()),
                None => Err(EvaluatorError::Text(
                    format!("No variable named {} has been declared", ident.v),
                    ident.s,
                )),
            },
            ast::Expr::Call { expr, args } => todo!(),
            ast::Expr::Access { expr, field } => todo!(),
            ast::Expr::Add(lhs, rhs) => todo!(),
            ast::Expr::Sub(lhs, rhs) => todo!(),
            ast::Expr::Mul(lhs, rhs) => todo!(),
            ast::Expr::Div(lhs, rhs) => todo!(),
        }
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
