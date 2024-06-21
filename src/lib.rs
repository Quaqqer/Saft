#![allow(clippy::new_without_default)]
#![feature(map_try_insert)]

pub mod ast;
pub mod eval;
mod parser_test;
pub mod query;
pub(crate) mod sexpr;
pub mod span;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);
