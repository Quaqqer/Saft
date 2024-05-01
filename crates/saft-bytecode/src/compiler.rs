use std::{borrow::Borrow, collections::HashMap, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ir::ir;
use saft_syntax::span::{Span, Spanned};

use crate::{
    chunk::Chunk,
    compiled_item::CompiledItem,
    constant::{Constant, ConstantRef},
    op::Op,
    value::{Function, NativeFunction, SaftFunction},
};

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

#[allow(unused)]
macro_rules! exotic {
    ($msg:expr, $span:expr) => {
        return Err(Error::Exotic {
            message: $msg.into(),
            span: $span.clone(),
            note: None,
        })
    };

    ($msg:expr, $span:expr, $note:expr) => {
        return Err(Error::Exotic {
            message: $msg.into(),
            span: $span.clone(),
            note: Some($note.into()),
        })
    };
}

#[derive(Clone, Debug)]
struct Scope {
    stack_base: usize,
}

impl Scope {
    pub fn new(stack_base: usize) -> Self {
        Self { stack_base }
    }
}

#[derive(Clone, Debug)]
pub struct CompilerFrame {
    stack_i: usize,
    loop_stack: Vec<usize>,
    scopes: Vec<Scope>,
}

#[derive(Clone, Debug)]
pub struct Compiler {
    frames: Vec<CompilerFrame>,
    ref_offsets: HashMap<ir::VarRef, usize>,
    compiled_items: Vec<CompiledItem>,
    pub constants: Vec<Constant>,
}

impl Compiler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            frames: vec![CompilerFrame {
                stack_i: 0,
                loop_stack: Vec::new(),
                scopes: Vec::new(),
            }],
            ref_offsets: HashMap::new(),
            compiled_items: Vec::new(),
            constants: vec![],
        }
    }

    fn frame(&self) -> &CompilerFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CompilerFrame {
        self.frames.last_mut().unwrap()
    }

    fn stack_i(&self) -> usize {
        self.frame().stack_i
    }

    fn stack_i_mut(&mut self) -> &mut usize {
        &mut self.frame_mut().stack_i
    }

    fn scopes(&self) -> &Vec<Scope> {
        &self.frame().scopes
    }

    fn scopes_mut(&mut self) -> &mut Vec<Scope> {
        &mut self.frame_mut().scopes
    }

    fn compile_items(
        &mut self,
        items: &[&Spanned<ir::Item<NativeFunction>>],
    ) -> Result<Option<ConstantRef>, Error> {
        let mut new_compiled_items = items
            .iter()
            .skip(self.constants.len())
            .map(|item| self.compile_item(item))
            .try_collect::<Vec<_>>()?;
        self.compiled_items.append(&mut new_compiled_items);

        Ok(None)
    }

    fn compile_item(
        &mut self,
        item: &Spanned<ir::Item<NativeFunction>>,
    ) -> Result<CompiledItem, Error> {
        let compiled_item = match &item.v {
            ir::Item::Function(fun) => {
                let constant = Constant::Function(Function::SaftFunction(
                    self.compile_fn(item.s.spanned(fun))?,
                ));
                let ref_ = self.add_constant(constant);
                CompiledItem::Function(ref_)
            }
            ir::Item::Builtin(builtin) => {
                let constant = Constant::Function(Function::NativeFunction(builtin.clone()));
                let ref_ = self.add_constant(constant);
                CompiledItem::Function(ref_)
            }
        };

        Ok(compiled_item)
    }

    fn add_constant(&mut self, constant: Constant) -> ConstantRef {
        let i = self.constants.len();
        self.constants.push(constant);
        ConstantRef(i)
    }

    pub fn compile_module(
        &mut self,
        module: &ir::Module,
        items: &[Option<Spanned<ir::Item<NativeFunction>>>],
    ) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();

        self.compile_items(
            &items
                .iter()
                .map(|opt| opt.as_ref().unwrap())
                .collect::<Vec<_>>(),
        )?;

        for stmt in &module.stmts {
            self.compile_stmt_(stmt, &mut chunk)?
        }

        Ok(chunk)
    }

    fn compile_fn(&mut self, function: Spanned<&ir::Function>) -> Result<SaftFunction, Error> {
        fn inner(
            compiler: &mut Compiler,
            function: Spanned<&ir::Function>,
        ) -> Result<SaftFunction, Error> {
            let Spanned { s, v: function } = function;
            let ir::Function { params, body } = function;

            let mut chunk = Chunk::new();
            for param in params {
                compiler.declare(param.v);
            }

            compiler.compile_block(body, &mut chunk)?;
            chunk.emit(Op::Return, s);

            Ok(SaftFunction {
                arity: params.len(),
                chunk: Rc::new(chunk),
            })
        }

        self.frames.push(CompilerFrame {
            stack_i: 0,
            loop_stack: Vec::new(),
            scopes: Vec::new(),
        });

        let res = inner(self, function)?;

        self.frames.pop().unwrap();

        Ok(res)
    }

    pub fn compile_stmt(&mut self, stmt: &Spanned<ir::Stmt>) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();
        self.compile_stmt_(stmt, &mut chunk)?;
        Ok(chunk)
    }

    fn compile_stmt_(&mut self, stmt: &Spanned<ir::Stmt>, chunk: &mut Chunk) -> Result<(), Error> {
        match &stmt.v {
            ir::Stmt::Expr(e) => {
                self.compile_expr_(e, chunk)?;
                chunk.emit(Op::Pop, &stmt.s);
            }
            ir::Stmt::Declare(ident, expr) => {
                self.compile_expr_(expr, chunk)?;
                self.declare(ident.v);
            }
            ir::Stmt::Return(e) => {
                self.compile_expr_(e, chunk)?;
                chunk.emit(Op::Return, &stmt.s);
            }
        }

        Ok(())
    }

    fn compile_block(
        &mut self,
        block: &Spanned<ir::Block>,
        chunk: &mut Chunk,
    ) -> Result<(), Error> {
        self.enter_scope();
        for stmt in &block.v.stmts {
            self.compile_stmt_(stmt, chunk)?;
        }

        if let Some(tail) = &block.v.tail {
            self.compile_expr_(tail, chunk)?;
        } else {
            chunk.emit(Op::Nil, &block.s);
        }

        self.exit_scope_trailing(chunk, &block.s);

        Ok(())
    }

    fn compile_untail_block(
        &mut self,
        block: &ir::UntailBlock,
        chunk: &mut Chunk,
    ) -> Result<(), Error> {
        self.enter_scope();

        for stmt in &block.0 {
            self.compile_stmt_(stmt, chunk)?;
        }

        self.exit_scope(chunk, Span::new(0..0));

        Ok(())
    }

    pub fn compile_expr(&mut self, expr: &Spanned<ir::Expr>) -> Result<Chunk, Error> {
        let mut chunk = Chunk::new();
        self.compile_expr_(expr, &mut chunk)?;
        Ok(chunk)
    }

    fn compile_expr_(&mut self, expr: &Spanned<ir::Expr>, chunk: &mut Chunk) -> Result<(), Error> {
        let s = &expr.s;
        match &expr.v {
            ir::Expr::Nil => chunk.emit(Op::Nil, s),
            ir::Expr::Bool(b) => chunk.emit(Op::Bool(*b), s),
            ir::Expr::Float(f) => chunk.emit(Op::Float(*f), s),
            ir::Expr::Integer(i) => chunk.emit(Op::Integer(*i), s),
            ir::Expr::String(string) => chunk.emit(Op::String(string.clone()), s),
            ir::Expr::Var(ident) => match ident {
                ir::Ref::Item(item_ref) => chunk.emit(Op::Constant(item_ref.0), s),
                ir::Ref::Var(var_ref) => {
                    let i = self.lookup(*var_ref)?;
                    chunk.emit(Op::Var(i), s);
                }
            },
            ir::Expr::Vec(exprs) => {
                for expr in exprs {
                    self.compile_expr_(expr, chunk)?;
                    *self.stack_i_mut() += 1;
                }
                *self.stack_i_mut() -= exprs.len();
                chunk.emit(Op::Vec(exprs.len()), s);
            }
            ir::Expr::Grouping(box e) => self.compile_expr_(e, chunk)?,
            ir::Expr::Block(block) => self.compile_block(block, chunk)?,
            ir::Expr::If(if_) => self.compile_if(if_, chunk)?,
            ir::Expr::Loop(block) => {
                chunk.emit(Op::Jmp(chunk.end() + 2), s);

                let loop_end = chunk.end();
                self.frame_mut().loop_stack.push(loop_end);
                chunk.emit(Op::Jmp(0), s);

                let loop_start = chunk.end();

                self.compile_untail_block(block, chunk)?;

                chunk.emit(Op::Jmp(loop_start), s);

                self.patch_jump(loop_end, chunk.end(), chunk);

                self.frame_mut().loop_stack.pop().unwrap();
            }
            ir::Expr::Break(expr) => {
                let Some(loop_end) = self.frame_mut().loop_stack.last().cloned() else {
                    exotic!("No loop to break from", s);
                };

                match expr.as_ref() {
                    Some(expr) => {
                        self.compile_expr_(expr, chunk)?;
                    }
                    None => chunk.emit(Op::Nil, s),
                }

                // Pop declarations in loop
                let env = self.scopes().last().unwrap();
                let decls = self.stack_i() - env.stack_base;
                if 0 < decls {
                    chunk.emit(Op::TailPop(decls), s)
                }

                chunk.emit(Op::Jmp(loop_end), s);
            }
            ir::Expr::Unary(expr, op) => {
                self.compile_expr_(expr, chunk)?;

                match op {
                    ir::UnaryOp::Plus => {}
                    ir::UnaryOp::Negate => chunk.emit(Op::Negate, s),
                    ir::UnaryOp::Not => chunk.emit(Op::Not, s),
                };
            }
            ir::Expr::Assign(lexpr, expr) => match &lexpr.v {
                ir::LExpr::Index(indexable, index) => {
                    self.compile_expr_(indexable, chunk)?;
                    *self.stack_i_mut() += 1;
                    self.compile_expr_(index, chunk)?;
                    *self.stack_i_mut() += 1;
                    self.compile_expr_(expr, chunk)?;
                    *self.stack_i_mut() -= 2;
                    chunk.emit(Op::AssignIndexable, s);
                }
                ir::LExpr::Var(ref_) => {
                    self.compile_expr_(expr, chunk)?;
                    chunk.emit(Op::Assign(self.lookup(*ref_)?), s);
                }
            },
            ir::Expr::Binary(lhs, rhs, op) => {
                let op = match op {
                    ir::BinaryOp::Or => Op::Or,
                    ir::BinaryOp::And => Op::And,
                    ir::BinaryOp::Eq => Op::Eq,
                    ir::BinaryOp::Ne => Op::Ne,
                    ir::BinaryOp::Lt => Op::Lt,
                    ir::BinaryOp::Le => Op::Le,
                    ir::BinaryOp::Gt => Op::Gt,
                    ir::BinaryOp::Ge => Op::Ge,
                    ir::BinaryOp::Mul => Op::Mul,
                    ir::BinaryOp::Div => Op::Div,
                    ir::BinaryOp::IDiv => Op::IDiv,
                    ir::BinaryOp::Add => Op::Add,
                    ir::BinaryOp::Sub => Op::Sub,
                    ir::BinaryOp::Pow => Op::Pow,
                };
                self.binary(chunk, lhs, rhs, op, s)?
            }

            ir::Expr::Call(callable, args) => {
                self.compile_expr_(callable, chunk)?;
                *self.stack_i_mut() += 1;
                for arg in args {
                    self.compile_expr_(arg, chunk)?;
                    *self.stack_i_mut() += 1;
                }
                *self.stack_i_mut() -= 1 + args.len();
                chunk.emit(Op::Call(args.len()), s);
            }
            ir::Expr::Index(indexable, index) => {
                self.compile_expr_(indexable, chunk)?;
                *self.stack_i_mut() += 1;
                self.compile_expr_(index, chunk)?;
                *self.stack_i_mut() -= 1;
                chunk.emit(Op::Index, s);
            }
        }

        Ok(())
    }

    fn compile_if(&mut self, if_: &Spanned<ir::If>, chunk: &mut Chunk) -> Result<(), Error> {
        let ir::If { cond, body, else_ } = &if_.v;
        let s = &if_.s;
        self.compile_expr_(cond, chunk)?;
        let else_jump = chunk.emit_i(Op::JmpFalse(0), s);
        self.compile_block(body, chunk)?;
        let end_jump = chunk.emit_i(Op::Jmp(0), s);

        // Else
        let else_offset = chunk.end();
        self.patch_jump(else_jump, else_offset, chunk);
        if let box Some(else_) = else_ {
            match &else_.v {
                ir::Else::If(if_) => self.compile_if(if_, chunk)?,
                ir::Else::Block(block) => self.compile_block(block, chunk)?,
            }
        } else {
            chunk.emit(Op::Nil, s);
        }

        let end_offset = chunk.end();
        self.patch_jump(end_jump, end_offset, chunk);

        Ok(())
    }

    fn enter_scope(&mut self) {
        let frame = self.frames.last_mut().unwrap();
        frame.scopes.push(Scope::new(frame.stack_i));
    }

    fn exit_scope(&mut self, chunk: &mut Chunk, span: impl Borrow<Span>) {
        let env = self.frames.last_mut().unwrap().scopes.pop().unwrap();
        let decls = self.stack_i() - env.stack_base;
        self.frame_mut().stack_i -= decls;

        if 0 < decls {
            chunk.emit(Op::PopN(decls), span);
        }
    }

    fn exit_scope_trailing(&mut self, chunk: &mut Chunk, span: impl Borrow<Span>) {
        let env = self.scopes_mut().pop().unwrap();
        let decls = self.stack_i() - env.stack_base;
        self.frame_mut().stack_i -= decls;

        if 0 < decls {
            chunk.emit(Op::TailPop(decls), span)
        }
    }

    fn binary(
        &mut self,
        chunk: &mut Chunk,
        lhs: &Spanned<ir::Expr>,
        rhs: &Spanned<ir::Expr>,
        op: Op,
        span: impl Borrow<Span>,
    ) -> Result<(), Error> {
        self.compile_expr_(lhs, chunk)?;
        *self.stack_i_mut() += 1;
        self.compile_expr_(rhs, chunk)?;
        chunk.emit(op, span);
        *self.stack_i_mut() -= 1;
        Ok(())
    }

    fn patch_jump(&self, jump_i: usize, target: usize, chunk: &mut Chunk) {
        let op = chunk.get_mut_op(jump_i).unwrap();
        *op = match op {
            Op::JmpFalse(_) => Op::JmpFalse(target),
            Op::JmpTrue(_) => Op::JmpTrue(target),
            Op::Jmp(_) => Op::Jmp(target),
            _ => panic!("Tried patching something else than a jump"),
        }
    }

    fn lookup(&self, ref_: ir::VarRef) -> Result<usize, Error> {
        Ok(*self.ref_offsets.get(&ref_).unwrap())
    }

    fn declare(&mut self, ident: ir::VarRef) {
        self.ref_offsets.insert(ident, self.stack_i());
        self.frame_mut().stack_i += 1;
    }
}
