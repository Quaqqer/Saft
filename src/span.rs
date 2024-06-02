pub struct Span(usize, usize);

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.0, self.1)
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub v: T,
    pub s: Span,
}
