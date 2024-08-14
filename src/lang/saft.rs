use lalrpop_util::{lexer::Token, ParseError};

use crate::{
    lang::ast::StmtOrExpr,
    lang::eval::{Evaluator, EvaluatorError},
    lang::parser::SpannedStmtOrExprParser,
    span::{Span, Spanned},
};
use std::fmt::Write;

use super::parser::SpannedModuleParser;

pub struct SaftError {
    pub span: Span,
    pub kind: String,
    pub msg: String,
}

macro_rules! create_saft_error {
    ($span:expr, $kind:expr, $($fmts:expr),*) => {
        SaftError {
            span: Into::<Span>::into($span),
            kind: Into::<String>::into($kind),
            msg: format!($($fmts),*),
        }
    };
}

macro_rules! bail {
    ($($toks:tt)*) => {
        return Err(create_saft_error!($($toks)*))
    };
}

impl From<EvaluatorError> for SaftError {
    fn from(value: EvaluatorError) -> Self {
        match value {
            EvaluatorError::Text(msg, span) => SaftError {
                msg,
                kind: "Evaluation error".to_string(),
                span,
            },
        }
    }
}

type Res<T> = Result<T, SaftError>;

pub fn eval_stmt_or_expr(evaluator: &mut Evaluator, src: &str) -> Res<()> {
    let stmt_or_expr = SpannedStmtOrExprParser::new()
        .parse(src)
        .map_err(create_parse_error)?;

    let Spanned { s, v: stmt_or_expr } = stmt_or_expr;
    match stmt_or_expr {
        StmtOrExpr::Stmt(stmt) => evaluator
            .exec_stmt(&stmt, &s)
            .map_err(Into::<SaftError>::into),
        StmtOrExpr::Expr(expr) => {
            let res = evaluator
                .eval_expr(&expr, &s)
                .map_err(Into::<SaftError>::into)?;
            println!("{}", res.repr());
            Ok(())
        }
    }
}

pub fn eval_module(evaluator: &mut Evaluator, src: &str) -> Res<()> {
    let Spanned { s: _, v: module } = SpannedModuleParser::new()
        .parse(src)
        .map_err(create_parse_error)?;

    for stmt in module.stmts {
        evaluator.exec_stmt(&stmt.v, &stmt.s)?;
    }

    Ok(())
}

pub fn create_parse_error(error: ParseError<usize, Token, &str>) -> SaftError {
    let s = parse_error_span(&error);

    match error {
        lalrpop_util::ParseError::InvalidToken { location: _ } => {
            create_saft_error!(s, "Parse error", "Got unexpected token")
        }
        lalrpop_util::ParseError::UnrecognizedEof {
            location: _,
            expected,
        } => create_saft_error!(
            s,
            "Parse error",
            "Got unexpected EOF, expected {}",
            expected_list(expected)
        ),

        lalrpop_util::ParseError::UnrecognizedToken { token, expected } => create_saft_error!(
            s,
            "Parse error",
            "Got unexpected token '{}', expected {}",
            token.1,
            expected_list(expected)
        ),
        lalrpop_util::ParseError::ExtraToken { token } => {
            create_saft_error!(s, "Parse error", "Unexpected extra token '{}'", token.1)
        }

        lalrpop_util::ParseError::User { .. } => unreachable!(),
    }
}

fn parse_error_span<T, E>(err: &lalrpop_util::ParseError<usize, T, E>) -> Span {
    match err {
        lalrpop_util::ParseError::InvalidToken { location: l } => Span::new(*l, *l),
        lalrpop_util::ParseError::UnrecognizedEof { location: l, .. } => Span::new(*l, *l),
        lalrpop_util::ParseError::UnrecognizedToken { token, .. } => Span::new(token.0, token.2),
        lalrpop_util::ParseError::ExtraToken { token } => Span::new(token.0, token.2),
        lalrpop_util::ParseError::User { .. } => unreachable!(),
    }
}

fn expected_list(options: Vec<String>) -> String {
    match &options[..] {
        [] => unreachable!("Should be at least one expected item"),
        [choice] => choice.to_string(),
        [one, two] => format!("{} or {}", one, two),
        many => {
            let mut buf = String::new();
            let last = many.len() - 1;
            for (i, alt) in many.iter().enumerate() {
                if i == 0 {
                    write!(buf, "{}", alt).unwrap();
                } else if i == last {
                    write!(buf, ", or {}", alt).unwrap();
                } else {
                    write!(buf, ", {}", alt).unwrap();
                }
            }
            buf
        }
    }
}
