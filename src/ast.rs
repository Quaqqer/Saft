use crate::span::Spanned;

#[derive(Debug)]
pub struct Ast(pub Vec<Spanned<Item>>);

#[derive(Debug)]
pub enum Item {}
