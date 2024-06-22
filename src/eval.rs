use std::collections::HashMap;

use crate::{ast, span::Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn ty(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Bool(_) => ValueType::Bool,
            Value::Nil => ValueType::Nil,
        }
    }

    pub fn repr(&self) -> String {
        match self {
            Value::Int(v) => format!("{}", v),
            Value::Bool(v) => match v {
                true => "true".to_string(),
                false => "false".to_string(),
            },
            Value::Nil => "nil".to_string(),
        }
    }
}

pub enum ValueType {
    Int,
    Bool,
    Nil,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Int => write!(f, "int"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Nil => write!(f, "nil"),
        }
    }
}

type Res<T> = Result<T, EvaluatorError>;

#[derive(Debug)]
pub enum EvaluatorError {
    Text(String, Span),
}

pub struct Evaluator {
    globals: HashMap<String, Value>,
    call_frames: Vec<CallFrame>,
}

struct CallFrame {
    scopes: Vec<HashMap<String, Value>>,
}

impl CallFrame {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    /// Assign a value to a field in the scopes. Return true if successful
    ///
    /// * `ident`: The field
    /// * `value`: The value to assign the field to
    fn assign(&mut self, ident: &str, value: Value) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(cell) = scope.get_mut(ident) {
                *cell = value;
                return true;
            }
        }

        false
    }

    /// Declare a value in the outermost scope. Fails if a value is already declared by that name.
    /// Return the success.
    ///
    /// * `ident`: The field name
    /// * `value`: The value
    fn declare(&mut self, ident: String, value: Value) -> bool {
        let res = self.scopes.last_mut().unwrap().try_insert(ident, value);
        res.is_ok()
    }

    /// Look a for a field in all scopes. Return the value if found
    ///
    /// * `ident`: The field name
    fn lookup(&self, ident: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(ident) {
                return Some(v);
            }
        }

        None
    }
}

macro_rules! bail {
    ($span:expr, $($fmts:expr),+) => {
        return Err(EvaluatorError::Text(format!($($fmts),*), $span.clone()))
    };
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            call_frames: vec![CallFrame::new()],
        }
    }

    fn frame(&self) -> &CallFrame {
        self.call_frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.call_frames.last_mut().unwrap()
    }

    fn enter_frame(&mut self) {
        todo!()
    }

    fn exit_frame(&mut self) {
        todo!()
    }

    fn enter_scope(&mut self) {
        self.frame_mut().enter_scope();
    }

    fn exit_scope(&mut self) {
        self.frame_mut().exit_scope();
    }

    fn scoped<T>(&mut self, f: impl Fn(&mut Evaluator) -> T) -> T {
        self.enter_scope();
        let res = f(self);
        self.exit_scope();
        res
    }

    pub fn exec_stmt(&mut self, stmt: &ast::Stmt, _span: &Span) -> Res<()> {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.eval_expr(&expr.v, &expr.s)?;
                Ok(())
            }
            ast::Stmt::Let { ident, expr } => {
                let v = self.eval_expr(&expr.v, &expr.s)?;
                let declared = self.declare(ident.v.clone(), v);
                if declared {
                    Ok(())
                } else {
                    bail!(ident.s, "Failed to declare variable '{}', it has already been declared in this scope", ident.v);
                }
            }
        }
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
            ast::Expr::Block(ast::TrailBlock(stmts, trail)) => self.scoped(|this| {
                for stmt in stmts {
                    this.exec_stmt(&stmt.v, &stmt.s)?;
                }

                if let Some(expr) = trail {
                    Ok::<Value, EvaluatorError>(this.eval_expr(&expr.v, &expr.s)?)
                } else {
                    Ok(Value::Nil)
                }
            })?,
            ast::Expr::Call { .. } => todo!(),
            ast::Expr::Access { .. } => todo!(),
            ast::Expr::Add(lhs, rhs) => binop!(lhs, rhs, "+", |lhs, rhs| lhs + rhs),
            ast::Expr::Sub(lhs, rhs) => binop!(lhs, rhs, "-", |lhs, rhs| lhs - rhs),
            ast::Expr::Mul(lhs, rhs) => binop!(lhs, rhs, "*", |lhs, rhs| lhs * rhs),
            ast::Expr::Div(lhs, rhs) => binop!(lhs, rhs, "/", |lhs, rhs| lhs / rhs),
        })
    }

    fn lookup(&self, ident: &str) -> Option<&Value> {
        self.frame().lookup(ident)
    }

    fn declare(&mut self, ident: String, value: Value) -> bool {
        self.frame_mut().declare(ident, value)
    }

    fn assign(&mut self, ident: &str, value: Value) -> bool {
        self.frame_mut().assign(ident, value)
    }
}

#[cfg(test)]
mod tests {
    use super::{Evaluator, Value};
    use crate::{ast, span::Spanned};
    use indoc::indoc;

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

    #[test]
    fn variables() {
        assert_eq!(
            eval_expr(indoc! {"
                {
                    let x = 2;
                    let y = 3;
                    x + y
                }
            "}),
            Value::Int(5)
        );
    }
}
