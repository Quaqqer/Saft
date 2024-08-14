use std::{collections::HashMap, fmt::Write, rc::Rc};

use crate::{
    lang::ast,
    span::{Span, Spanned},
};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
    Function(FunctionValue),
}

#[derive(Debug, Clone)]
pub struct FunctionValue {
    params: Rc<Vec<String>>,
    body: Rc<Spanned<ast::Expr>>,
}

impl Value {
    pub fn ty(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Bool(_) => ValueType::Bool,
            Value::Nil => ValueType::Nil,
            Value::Function(_) => ValueType::Function,
        }
    }

    pub fn repr(&self) -> String {
        match self {
            Value::Int(v) => format!("{}", v),
            Value::Float(f) => format!("{}", f),
            Value::Bool(v) => match v {
                true => "true".to_string(),
                false => "false".to_string(),
            },
            Value::Nil => "nil".to_string(),
            Value::Function(FunctionValue { params, body: _ }) => {
                let mut s = String::new();
                write!(s, "(").unwrap();
                for (i, param) in params.iter().enumerate() {
                    if i != 0 {
                        write!(s, ", ").unwrap();
                    }
                    write!(s, "{}", param).unwrap();
                }
                write!(s, ")").unwrap();
                write!(s, " => ").unwrap();
                write!(s, "...").unwrap();
                s
            }
        }
    }
}

pub enum ValueType {
    Int,
    Float,
    Bool,
    Nil,
    Function,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValueType::Int => "int",
                ValueType::Float => "float",
                ValueType::Bool => "bool",
                ValueType::Nil => "nil",
                ValueType::Function => "function",
            }
        )
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
        self.call_frames.push(CallFrame::new());
    }

    fn exit_frame(&mut self) {
        self.call_frames.pop().unwrap();
    }

    fn framed<T>(&mut self, f: impl FnOnce(&mut Evaluator) -> T) -> T {
        self.enter_frame();
        let res = f(self);
        self.exit_frame();
        res
    }

    fn enter_scope(&mut self) {
        self.frame_mut().enter_scope();
    }

    fn exit_scope(&mut self) {
        self.frame_mut().exit_scope();
    }

    fn scoped<T>(&mut self, f: impl FnOnce(&mut Evaluator) -> T) -> T {
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
            ast::Expr::Float(f) => Value::Float(*f),
            ast::Expr::Bool(b) => Value::Bool(*b),
            ast::Expr::Var(ident) => match self.lookup(&ident.v) {
                Some(val) => val.clone(),
                None => {
                    bail!(ident.s, "No variable named {} has been declared", ident.v)
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
            ast::Expr::Call { expr, args } => {
                let f = self.eval_expr(&expr.v, &expr.s)?;

                let Value::Function(FunctionValue { params, body }) = f else {
                    bail!(expr.s, "Cannot call a non-function value {}", f.repr());
                };

                if params.len() != args.len() {
                    bail!(
                        span,
                        "Mismatched number of arguments, expected {} but got {}",
                        params.len(),
                        args.len()
                    );
                }

                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    let v = self.eval_expr(&arg.v, &arg.s)?;
                    arguments.push(v);
                }

                self.framed(|this| {
                    for (param, value) in params.iter().zip(arguments.into_iter()) {
                        this.declare(param.clone(), value);
                    }

                    // Push arguments
                    this.eval_expr(&body.v, &body.s)
                })?
            }
            ast::Expr::Access { .. } => todo!(),
            ast::Expr::Add(lhs, rhs) => binop!(lhs, rhs, "+", |lhs, rhs| lhs + rhs),
            ast::Expr::Sub(lhs, rhs) => binop!(lhs, rhs, "-", |lhs, rhs| lhs - rhs),
            ast::Expr::Mul(lhs, rhs) => binop!(lhs, rhs, "*", |lhs, rhs| lhs * rhs),
            ast::Expr::Div(lhs, rhs) => binop!(lhs, rhs, "/", |lhs, rhs| lhs / rhs),
            ast::Expr::ArrowFn(params, expr) => Value::Function(FunctionValue {
                params: Rc::new(params.iter().map(|sv| sv.v.clone()).collect()),
                body: Rc::new((**expr).clone()),
            }),
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
    use std::assert_matches::assert_matches;

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
        assert_matches!(eval_expr("1 + 2 + 3"), Value::Int(6));
    }

    #[test]
    fn variables() {
        assert_matches!(
            eval_expr(indoc! {"
                {
                    let x = 2;
                    let y = 3;
                    x + y
                }
            "}),
            Value::Int(5),
        );
    }
}
