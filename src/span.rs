#[derive(Clone, Copy, PartialEq)]
pub struct Span(usize, usize);

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start <= end);
        Self(start, end)
    }

    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub v: T,
    pub s: Span,
}

impl<T> Spanned<T> {
    pub fn new(v: T, span: Span) -> Self {
        Self { v, s: span }
    }
}

pub fn sp<T>(l: usize, r: usize, v: T) -> Spanned<T> {
    Spanned::new(v, Span::new(l, r))
}
