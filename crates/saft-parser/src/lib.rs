#![feature(let_chains)]

use ast::{Expr, Item, Module, Statement};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use saft_ast as ast;
use saft_common::span::{Span, Spanned};
use saft_lexer::{lex::Lexer, token::Token};

mod prec {
    pub const NONE: i32 = 0;
    pub const ASSIGN: i32 = 1;
    pub const TERM: i32 = 2;
    pub const FACTOR: i32 = 3;
    pub const UNARY: i32 = 4;
    pub const EXP: i32 = 5;
    pub const CALL: i32 = 6;
    pub const PRIMARY: i32 = 5;
}

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedToken {
        got: Spanned<Token>,
        expected: String,
    },
}

impl Error {
    pub fn diagnostic<FileId>(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Error::UnexpectedToken { got, expected } => Diagnostic::error()
                .with_message("Got an unexpected token")
                .with_labels(vec![Label::primary(file_id, got.s.r.clone()).with_message(
                    format!("Got {} but expected {}", got.v.describe(), expected),
                )]),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            lexer: Lexer::new(s),
        }
    }
    pub fn parse_file(&'a mut self) -> Result<ast::Module, Error> {
        let mut stmts = Vec::<Spanned<ast::Statement>>::new();

        while self.lexer.peek().v != Token::Eof {
            stmts.push(self.parse_statement()?);
        }

        Ok(Module { stmts })
    }

    fn eat(&mut self, t: Token) -> Result<Span, Error> {
        let st = self.lexer.next();
        match st.v {
            v if v == t => Ok(st.s),
            _ => Err(Error::UnexpectedToken {
                got: st,
                expected: t.describe().into(),
            }),
        }
    }

    fn eat_ident(&mut self) -> Result<Spanned<String>, Error> {
        let st = self.lexer.next();
        match &st.v {
            Token::Identifier(ident) => Ok(st.map(|_| ident.clone())),
            _ => Err(Error::UnexpectedToken {
                got: st,
                expected: Token::Identifier("".into()).describe().into(),
            }),
        }
    }

    fn try_eat(&mut self, t: Token) -> bool {
        let st = self.lexer.peek();
        match st.v {
            v if v == t => {
                self.eat(t).unwrap();
                true
            }
            _ => false,
        }
    }

    pub fn parse_single_statment(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let s = self.parse_statement()?;
        self.eat(Token::Eof)?;
        Ok(s)
    }

    pub fn parse_statement(&mut self) -> Result<Spanned<ast::Statement>, Error> {
        let st = self.lexer.peek();
        match st.v {
            Token::Identifier(ident) if self.lexer.peek_n(2).v == Token::ColonEqual => {
                let ident_t = self.lexer.next();
                let _colon_equals = self.lexer.next();
                let expr = self.parse_expr()?;
                let expr_s = expr.s.clone();

                Ok(Spanned::new(
                    Statement::Declare {
                        ident: Spanned::new(ident, ident_t.s.clone()),
                        expr,
                    },
                    ident_t.s.join(&expr_s),
                ))
            }

            Token::LParen
            | Token::Minus
            | Token::Identifier(_)
            | Token::Float(_)
            | Token::Integer(_)
            | Token::Nil => {
                let expr = self.parse_expr()?;
                let s = expr.s.clone();
                Ok(Spanned::new(Statement::Expr(expr), s))
            }

            Token::Fn => self.parse_fn(),

            _ => self.unexpected(st, "statement"),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        self.parse_precedence(prec::NONE)
    }

    fn parse_primary_expr(&mut self) -> Result<Spanned<Expr>, Error> {
        let st = self.lexer.next();
        match st.v {
            Token::Identifier(ident) => Ok(Spanned::new(
                Expr::Var(Spanned::new(ident.to_string(), st.s.clone())),
                st.s,
            )),
            Token::Float(f) => Ok(Spanned::new(Expr::Float(f), st.s)),
            Token::Integer(i) => Ok(Spanned::new(Expr::Integer(i), st.s)),
            Token::Nil => Ok(Spanned::new(Expr::Nil, st.s)),
            Token::LParen => {
                let start = st.s;
                let inner = self.parse_expr()?;
                let end = self.eat(Token::RParen)?;
                Ok(Spanned::new(
                    Expr::Grouping(Box::new(inner)),
                    start.join(&end),
                ))
            }
            Token::Minus => {
                let expr = self.parse_precedence(prec::UNARY + 1)?;
                let s = st.s.join(&expr.s);
                Ok(Spanned::new(Expr::Neg(Box::new(expr)), s))
            }

            _ => self.unexpected(st, "expression"),
        }
    }

    fn unexpected<T>(&mut self, got: Spanned<Token>, expected: &'static str) -> Result<T, Error> {
        Err(Error::UnexpectedToken {
            got,
            expected: expected.into(),
        })
    }

    fn parse_precedence(&mut self, min_prec: i32) -> Result<Spanned<Expr>, Error> {
        let mut expr = self.parse_primary_expr()?;

        while let Some((post_prec, post_rule)) = Self::post_rule(&self.lexer.peek().v)
            && min_prec <= post_prec
        {
            expr = post_rule(expr, self)?;
        }

        Ok(expr)
    }

    /// Postfix and infix rules
    fn post_rule(
        t: &Token,
    ) -> Option<(
        i32,
        Box<dyn Fn(Spanned<Expr>, &mut Parser) -> Result<Spanned<Expr>, Error>>,
    )> {
        macro_rules! binop {
            ($prec:expr, $left_assoc:expr, $variant:path) => {
                Some((
                    $prec,
                    Box::new(|lhs, parser| {
                        parser.lexer.next();
                        let next_prec = if $left_assoc { $prec + 1 } else { $prec };
                        let rhs = parser.parse_precedence(next_prec)?;
                        let s = lhs.s.join(&rhs.s);
                        Ok(Spanned::new($variant(Box::new(lhs), Box::new(rhs)), s))
                    }),
                ))
            };
        }

        match t {
            Token::Equal => binop!(prec::ASSIGN, false, Expr::Assign),
            Token::Plus => binop!(prec::TERM, true, Expr::Add),
            Token::Minus => binop!(prec::TERM, true, Expr::Sub),
            Token::Star => binop!(prec::FACTOR, true, Expr::Mul),
            Token::Slash => binop!(prec::FACTOR, true, Expr::Div),
            Token::Caret => binop!(prec::EXP, false, Expr::Pow),
            Token::LParen => Some((
                prec::CALL,
                Box::new(|lhs, parser| {
                    parser.eat(Token::LParen)?;

                    let mut arguments = Vec::new();

                    while parser.lexer.peek().v != Token::RParen {
                        let arg = parser.parse_expr()?;
                        arguments.push(arg);

                        let ate_comma = parser.try_eat(Token::Comma);
                        if !ate_comma {
                            break;
                        }
                    }

                    let end = parser.eat(Token::RParen)?;
                    let s = lhs.s.join(&end);

                    Ok(Spanned::new(Expr::Call(Box::new(lhs), arguments), s))
                }),
            )),
            _ => None,
        }
    }

    fn parse_fn(&mut self) -> Result<Spanned<Statement>, Error> {
        let start = self.eat(Token::Fn)?;
        let ident = self.eat_ident()?;

        let mut params = Vec::new();

        self.eat(Token::LParen)?;
        while self.lexer.peek().v != Token::RParen {
            let param = self.eat_ident()?;
            params.push(param);

            if !self.try_eat(Token::Comma) {
                break;
            }
        }
        self.eat(Token::RParen)?;

        self.eat(Token::LBrace)?;
        let end = self.eat(Token::RBrace)?;

        Ok(Spanned::new(
            Statement::Item(Item::Fn {
                ident,
                params,
                body: vec![],
            }),
            start.join(&end),
        ))
    }
}

#[cfg(test)]
mod test {
    use saft_ast::Expr;
    use saft_common::span::{spanned, Spanned};

    use crate::Parser;

    fn test_expr(s: &str, expected: &Spanned<Expr>) {
        let res = Parser::new(s).parse_expr().unwrap();
        assert_eq!(&res, expected);
    }
    #[test]
    fn test_precedence() {
        test_expr(
            "a + b + c",
            &spanned(
                Expr::Add(
                    Box::new(spanned(
                        Expr::Add(
                            Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                        ),
                        0..5,
                    )),
                    Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                ),
                0..9,
            ),
        );

        test_expr(
            "a + b * c",
            &spanned(
                Expr::Add(
                    Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                    Box::new(spanned(
                        Expr::Mul(
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                            Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                        ),
                        4..9,
                    )),
                ),
                0..9,
            ),
        );
    }

    #[test]
    fn test_associativity() {
        test_expr(
            "a + b + c",
            &spanned(
                Expr::Add(
                    Box::new(spanned(
                        Expr::Add(
                            Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                        ),
                        0..5,
                    )),
                    Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                ),
                0..9,
            ),
        );

        test_expr(
            "a ^ b ^ c",
            &spanned(
                Expr::Pow(
                    Box::new(spanned(Expr::Var(spanned("a".into(), 0..1)), 0..1)),
                    Box::new(spanned(
                        Expr::Pow(
                            Box::new(spanned(Expr::Var(spanned("b".into(), 4..5)), 4..5)),
                            Box::new(spanned(Expr::Var(spanned("c".into(), 8..9)), 8..9)),
                        ),
                        4..9,
                    )),
                ),
                0..9,
            ),
        );
    }
}
