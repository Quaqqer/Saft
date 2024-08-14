use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod eval;
pub mod saft;
lalrpop_mod!(pub parser, "/lang/parser.rs");
