use crate::ir;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_syntax::{
    ast,
    span::{spanned, Span, Spanned},
};
use std::collections::HashMap;

macro_rules! exotic {
    ($msg:expr, $span:expr) => {
        return Err(Error::Exotic {
            message: $msg.into(),
            span: $span.into(),
            note: None,
        })
    };

    ($msg:expr, $span:expr, $note:expr) => {
        return Err(Error::Exotic {
            message: $msg.into(),
            span: $span.into(),
            note: Some($note.into()),
        })
    };
}

#[derive(Debug)]
pub enum Error {
    Exotic {
        message: String,
        span: Span,
        note: Option<String>,
    },
}

impl Error {
    pub fn diagnostic<FileId>(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::Exotic {
                message,
                span,
                note,
            } => Diagnostic::error().with_message(message).with_labels({
                let mut label = Label::primary(file_id, span.r.clone());
                if let Some(note) = note {
                    label = label.with_message(note);
                };
                vec![label]
            }),
        }
    }
}

#[derive(Clone)]
pub struct Lowerer<N: Clone> {
    pub items: Vec<Option<Spanned<ir::Item<N>>>>,
    scopes: Vec<HashMap<String, ir::Ref>>,
    scope_base: usize,
    var_counter: usize,
}

impl<N: Clone> Lowerer<N> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            scopes: vec![HashMap::new()],
            scope_base: 0,
            var_counter: 0,
        }
    }

    fn resolve_module_items(&mut self, module: &ast::Module) -> Result<(), Error> {
        self.resolve_statements_items(&module.stmts)
    }

    fn resolve_block_items(&mut self, block: &ast::Block) -> Result<(), Error> {
        self.resolve_statements_items(&block.stmts)
    }

    fn resolve_statements_items(&mut self, stmts: &[Spanned<ast::Statement>]) -> Result<(), Error> {
        let items = stmts
            .iter()
            .filter_map(|stmt| match &stmt.v {
                ast::Statement::Item(item) => Some(stmt.s.spanned(item)),
                _ => None,
            })
            .collect::<Vec<_>>();

        let item_refs = items
            .iter()
            .map(|item| self.new_item(self.item_name(item)))
            .collect::<Vec<_>>();

        for (item, ref_) in items.iter().zip(item_refs) {
            let resolved = self.resolve_item(item)?;
            self.replace_item(ref_, resolved);
        }

        Ok(())
    }

    fn item_name(&self, item: &Spanned<&ast::Item>) -> Spanned<String> {
        match &item.v {
            ast::Item::Function(ast::Function { ident, .. }) => ident.clone(),
        }
    }

    fn resolve_item(&mut self, item: &Spanned<&ast::Item>) -> Result<Spanned<ir::Item<N>>, Error> {
        let Spanned { s, v: item } = item;

        Ok(s.spanned(match item {
            ast::Item::Function(fun) => ir::Item::Function(self.lower_item_fn(fun)?),
        }))
    }

    fn lower_item_fn(&mut self, fn_: &ast::Function) -> Result<ir::Function, Error> {
        let ast::Function { params, body, .. } = fn_;

        self.scoped_based(|l| {
            let params = params
                .iter()
                .map(|ident| l.declare(&ident.v.name))
                .try_collect::<Vec<_>>()?;
            let body = l.lower_block(body)?;

            Ok(ir::Function { params, body })
        })
    }

    pub fn lower_module(&mut self, module: &ast::Module) -> Result<ir::Module, Error> {
        self.resolve_module_items(module)?;

        let stmts = module
            .stmts
            .iter()
            .map(|stmt| self.lower_statement(stmt))
            .filter_map(|stmt| match stmt {
                Ok(m_stmt) => m_stmt.map(Ok),
                Err(e) => Some(Err(e)),
            })
            .try_collect::<Vec<_>>()?;

        Ok(ir::Module { stmts })
    }

    fn lower_statements(
        &mut self,
        stmts: &[Spanned<ast::Statement>],
    ) -> Result<Vec<Spanned<ir::Stmt>>, Error> {
        stmts
            .iter()
            .map(|stmt| self.lower_statement(stmt))
            .filter_map(|stmt| match stmt {
                Ok(m_stmt) => m_stmt.map(Ok),
                Err(e) => Some(Err(e)),
            })
            .try_collect::<Vec<_>>()
    }

    fn lower_exprs(
        &mut self,
        exprs: &[Spanned<ast::Expr>],
    ) -> Result<Vec<Spanned<ir::Expr>>, Error> {
        exprs
            .iter()
            .map(|expr| self.lower_expr(expr))
            .try_collect::<Vec<_>>()
    }

    pub fn lower_statement(
        &mut self,
        stmt: &Spanned<ast::Statement>,
    ) -> Result<Option<Spanned<ir::Stmt>>, Error> {
        let s = &stmt.s;

        Ok(Some(match &stmt.v {
            ast::Statement::Expr(e) => {
                let ir_expr = self.lower_expr(e)?;
                s.spanned(ir::Stmt::Expr(ir_expr))
            }
            ast::Statement::Declare { ident, expr } => {
                let ref_ = self.declare(ident)?;
                let expr = self.lower_expr(expr)?;
                s.spanned(ir::Stmt::Declare(ref_, expr))
            }
            ast::Statement::Return(e) => s.spanned(ir::Stmt::Return(self.lower_expr(e)?)),
            ast::Statement::Item(_) => {
                // Already handled
                return Ok(None);
            }
        }))
    }

    fn lower_expr(&mut self, expr: &Spanned<ast::Expr>) -> Result<Spanned<ir::Expr>, Error> {
        fn binary<N: Clone>(
            lowerer: &mut Lowerer<N>,
            lhs: &Spanned<ast::Expr>,
            rhs: &Spanned<ast::Expr>,
            op: ir::BinaryOp,
        ) -> Result<ir::Expr, Error> {
            let lhs = Box::new(lowerer.lower_expr(lhs)?);
            let rhs = Box::new(lowerer.lower_expr(rhs)?);
            Ok(ir::Expr::Binary(lhs, rhs, op))
        }
        let s = &expr.s;

        Ok(s.spanned(match &expr.v {
            ast::Expr::Nil => ir::Expr::Nil,
            ast::Expr::Bool(b) => ir::Expr::Bool(*b),
            ast::Expr::Float(f) => ir::Expr::Float(*f),
            ast::Expr::Integer(i) => ir::Expr::Integer(*i),
            ast::Expr::String(s) => ir::Expr::String(s.clone()),
            ast::Expr::Var(ident) => {
                let ref_ = self.resolve(ident)?;
                ir::Expr::Var(ref_)
            }
            ast::Expr::Vec(exprs) => ir::Expr::Vec(
                exprs
                    .iter()
                    .map(|expr| self.lower_expr(expr))
                    .try_collect::<Vec<_>>()?,
            ),
            ast::Expr::Grouping(expr) => ir::Expr::Grouping(Box::new(self.lower_expr(expr)?)),
            ast::Expr::Block(block) => ir::Expr::Block(Box::new(self.lower_block(block)?)),
            ast::Expr::If(cond, body, else_) => ir::Expr::If(s.spanned(ir::If {
                cond: Box::new(self.lower_expr(cond)?),
                body: Box::new(self.lower_block(body)?),
                else_: Box::new(if let Some(else_) = else_ {
                    {
                        let else_ = self.lower_expr(else_)?;
                        Some(else_.s.spanned(match else_.v {
                            ir::Expr::Block(block) => ir::Else::Block(*block),
                            ir::Expr::If(if_) => ir::Else::If(if_),
                            _ => unreachable!(),
                        }))
                    }
                } else {
                    None
                }),
            })),
            ast::Expr::Loop(stmts) => self.scoped(|l| {
                l.resolve_statements_items(stmts)?;
                Ok(ir::Expr::Loop(Box::new(ir::UntailBlock(
                    l.lower_statements(stmts)?,
                ))))
            })?,
            ast::Expr::Break(e) => ir::Expr::Break(Box::new(Some(self.lower_expr(e)?))),
            ast::Expr::Neg(expr) => {
                ir::Expr::Unary(Box::new(self.lower_expr(expr)?), ir::UnaryOp::Negate)
            }
            ast::Expr::Not(expr) => {
                ir::Expr::Unary(Box::new(self.lower_expr(expr)?), ir::UnaryOp::Not)
            }
            ast::Expr::Assign(assignable, assignment) => {
                let assignable = self.lower_lexpr(assignable)?;
                let assignment = self.lower_expr(assignment)?;
                ir::Expr::Assign(Box::new(assignable), Box::new(assignment))
            }
            ast::Expr::Add(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Add)?,
            ast::Expr::Sub(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Sub)?,
            ast::Expr::Mul(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Mul)?,
            ast::Expr::Div(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Div)?,
            ast::Expr::IDiv(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::IDiv)?,
            ast::Expr::Pow(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Pow)?,
            ast::Expr::And(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::And)?,
            ast::Expr::Or(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Or)?,
            ast::Expr::Lt(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Lt)?,
            ast::Expr::Le(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Le)?,
            ast::Expr::Gt(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Gt)?,
            ast::Expr::Ge(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Ge)?,
            ast::Expr::Eq(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Eq)?,
            ast::Expr::Ne(lhs, rhs) => binary(self, lhs, rhs, ir::BinaryOp::Ne)?,
            ast::Expr::Call(callable, args) => {
                let callable = self.lower_expr(callable)?;
                let args = self.lower_exprs(args)?;
                ir::Expr::Call(Box::new(callable), args)
            }
            ast::Expr::Index(indexable, index) => {
                let indexable = self.lower_expr(indexable)?;
                let index = self.lower_expr(index)?;
                ir::Expr::Index(Box::new(indexable), Box::new(index))
            }
        }))
    }

    fn lower_block(&mut self, block: &Spanned<ast::Block>) -> Result<Spanned<ir::Block>, Error> {
        let s = &block.s;
        self.scoped(|l| {
            l.resolve_block_items(&block.v)?;

            let stmts = l.lower_statements(&block.v.stmts)?;

            let tail = match &block.v.tail {
                Some(tail) => Some(l.lower_expr(tail)?),
                None => None,
            };

            Ok(s.spanned(ir::Block { stmts, tail }))
        })
    }

    fn lower_lexpr(
        &mut self,
        assignable: &Spanned<ast::Expr>,
    ) -> Result<Spanned<ir::LExpr>, Error> {
        let s = &assignable.s;
        Ok(s.spanned(match &assignable.v {
            ast::Expr::Var(ident) => {
                let ref_ = self.resolve(ident)?;
                match ref_ {
                    ir::Ref::Item(_) => exotic!(
                        "Unassignable",
                        s.clone(),
                        "Cannot assign to items, they are constant"
                    ),
                    ir::Ref::Var(var_ref) => ir::LExpr::Var(var_ref),
                }
            }
            ast::Expr::Index(indexable, index) => {
                let indexable = self.lower_expr(indexable)?;
                let index = self.lower_expr(index)?;
                ir::LExpr::Index(Box::new(indexable), Box::new(index))
            }
            _ => exotic!("Unassignable", s.clone(), "Not an assignable expression"),
        }))
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn scoped<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        self.enter_scope();
        let res = f(self);
        self.exit_scope();
        res
    }

    fn scoped_based<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        self.scoped(|l| {
            let prev_base = l.scope_base;
            l.scope_base = l.scopes.len() - 1;
            let res = f(l);
            l.scope_base = prev_base;
            res
        })
    }

    fn declare(&mut self, ident: &Spanned<String>) -> Result<Spanned<ir::VarRef>, Error> {
        let ref_ = self.new_varref();
        self.scopes
            .last_mut()
            .unwrap()
            .insert(ident.v.clone(), ir::Ref::Var(ref_));
        Ok(ident.s.spanned(ref_))
    }

    fn resolve(&self, ident: &Spanned<String>) -> Result<ir::Ref, Error> {
        for (scope_i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(ref_) = scope.get(&ident.v) {
                match ref_ {
                    ir::Ref::Item(_) => return Ok(*ref_),
                    ir::Ref::Var(_) => {
                        return if self.scope_base <= scope_i {
                            Ok(*ref_)
                        } else {
                            exotic!(
                                "Inaccessible variable",
                                ident.s.clone(),
                                "Variable was resolved but is inaccessible from this context"
                            )
                        }
                    }
                }
            }
        }

        exotic!(
            "Unresolved variable",
            ident.s.clone(),
            format!("Could not resolve identifier '{}'", ident.v)
        )
    }

    fn new_item(&mut self, ident: Spanned<ast::Ident>) -> ir::ItemRef {
        let ref_ = ir::ItemRef(self.items.len());
        self.items.push(None);
        self.scopes
            .last_mut()
            .unwrap()
            .insert(ident.v.clone(), ir::Ref::Item(ref_));
        ref_
    }

    pub fn add_item(&mut self, ident: ast::Ident, item: ir::Item<N>) {
        let ref_ = self.new_item(spanned(ident, 0..0));
        self.replace_item(ref_, spanned(item, 0..0));
    }

    fn replace_item(&mut self, ref_: ir::ItemRef, item: Spanned<ir::Item<N>>) {
        self.items[ref_.0] = Some(item);
    }

    fn new_varref(&mut self) -> ir::VarRef {
        let ref_ = ir::VarRef(self.var_counter);
        self.var_counter += 1;
        ref_
    }
}
