use crate::ast;

pub(crate) enum SExpr {
    List(Vec<SExpr>),
    Atom(String),
}

impl std::fmt::Display for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SExpr::List(l) => {
                write!(f, "(")?;
                for (i, expr) in l.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }

                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            SExpr::Atom(s) => write!(f, "{}", s),
        }
    }
}

macro_rules! list {
    ($($elem:expr),*) => {
        SExpr::List(vec![$(SExpr::from($elem)),*])
    }
}

impl From<&str> for SExpr {
    fn from(value: &str) -> Self {
        SExpr::Atom(value.to_string())
    }
}

impl From<&String> for SExpr {
    fn from(value: &String) -> Self {
        SExpr::Atom(value.clone())
    }
}

impl From<String> for SExpr {
    fn from(value: String) -> Self {
        SExpr::Atom(value)
    }
}

impl From<&ast::Module> for SExpr {
    fn from(value: &ast::Module) -> Self {
        list!(
            "module",
            SExpr::List(value.items.iter().map(|item| (&item.v).into()).collect())
        )
    }
}

impl From<&ast::Item> for SExpr {
    fn from(value: &ast::Item) -> Self {
        match value {
            ast::Item::Fn {
                ident,
                params,
                stmts,
            } => list!(
                "fn",
                &ident.v,
                SExpr::List(params.iter().map(|param| (&param.v).into()).collect()),
                SExpr::List(stmts.iter().map(|stmt| (&stmt.v).into()).collect())
            ),
        }
    }
}

impl From<&ast::Stmt> for SExpr {
    fn from(value: &ast::Stmt) -> Self {
        match value {
            ast::Stmt::Expr(expr) => list!("expr", &expr.v),
            ast::Stmt::Let { ident, expr } => list!("let", &ident.v, &expr.v),
        }
    }
}

impl From<&ast::Expr> for SExpr {
    fn from(value: &ast::Expr) -> Self {
        match value {
            ast::Expr::Int(i) => format!("{}", i).into(),
            ast::Expr::Bool(b) => format!("{}", b).into(),
            ast::Expr::Var(v) => v.v.clone().into(),
            ast::Expr::Call { expr, args } => list!(
                "call",
                &expr.v,
                SExpr::List(args.iter().map(|arg| (&arg.v).into()).collect())
            ),
            ast::Expr::Block(ast::TrailBlock(stmts, trail)) => {
                let mut list_vec: Vec<SExpr> = vec![
                    "block".into(),
                    SExpr::List(stmts.iter().map(|arg| (&arg.v).into()).collect()),
                ];
                if let Some(trail) = trail {
                    list_vec.push((&trail.v).into());
                }
                SExpr::List(list_vec)
            }
            ast::Expr::Access { expr, field } => list!(".", &expr.v, &field.v),
            ast::Expr::Add(lhs, rhs) => list!("+", &lhs.v, &rhs.v),
            ast::Expr::Sub(lhs, rhs) => list!("-", &lhs.v, &rhs.v),
            ast::Expr::Mul(lhs, rhs) => list!("*", &lhs.v, &rhs.v),
            ast::Expr::Div(lhs, rhs) => list!("/", &lhs.v, &rhs.v),
        }
    }
}
