use crate::{
    ast::StmtOrExpr,
    eval::{Evaluator, EvaluatorError},
    parser::SpannedStmtOrExprParser,
    span::{Span, Spanned},
};

pub struct SaftError {
    pub span: Span,
    pub kind: String,
    pub msg: String,
}

macro_rules! bail {
    ($span:expr, $kind:expr, $($fmts:expr),*) => {
        return Err(SaftError {
            span: Into::<Span>::into($span),
            kind: Into::<String>::into($kind),
            msg: format!($($fmts),*),
        })
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
    let stmt_or_expr = SpannedStmtOrExprParser::new().parse(src).or_else(|e| {
        let s = parse_error_span(&e);

        match e {
            lalrpop_util::ParseError::InvalidToken { location: _ } => {
                bail!(s, "Parse error", "Got unexpected token")
            }
            lalrpop_util::ParseError::UnrecognizedEof {
                location: _,
                expected,
            } => bail!(
                s,
                "Parse error",
                "Got unexpected EOF, expected {:?}",
                expected
            ),

            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => bail!(
                s,
                "Parse error",
                "Got unexpected token '{}', expected {:?}",
                token.1,
                expected
            ),
            lalrpop_util::ParseError::ExtraToken { token } => {
                bail!(s, "Parse error", "Unexpected extra token '{}'", token.1)
            }

            lalrpop_util::ParseError::User { .. } => unreachable!(),
        }
    })?;

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

fn parse_error_span<T, E>(err: &lalrpop_util::ParseError<usize, T, E>) -> Span {
    match err {
        lalrpop_util::ParseError::InvalidToken { location: l } => Span::new(*l, *l),
        lalrpop_util::ParseError::UnrecognizedEof { location: l, .. } => Span::new(*l, *l),
        lalrpop_util::ParseError::UnrecognizedToken { token, .. } => Span::new(token.0, token.2),
        lalrpop_util::ParseError::ExtraToken { token } => Span::new(token.0, token.2),
        lalrpop_util::ParseError::User { .. } => unreachable!(),
    }
}
