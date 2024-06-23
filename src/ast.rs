use crate::span::Spanned;

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Spanned<Item>>,
}

#[derive(Debug)]
pub enum Item {
    Fn {
        ident: Spanned<String>,
        params: Vec<Spanned<String>>,
        stmts: Vec<Spanned<Stmt>>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Let {
        ident: Spanned<String>,
        expr: Spanned<Expr>,
    },
}

#[derive(Debug)]
pub struct TrailBlock(pub Vec<Spanned<Stmt>>, pub Option<Box<Spanned<Expr>>>);

#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    Var(Spanned<String>),
    Block(TrailBlock),
    Call {
        expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    Access {
        expr: Box<Spanned<Expr>>,
        field: Spanned<String>,
    },
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
}

pub enum StmtOrExpr {
    Stmt(Stmt),
    Expr(Expr),
}
