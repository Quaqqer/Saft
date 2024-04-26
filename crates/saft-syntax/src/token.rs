#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Unknown,
    Eof,
    Error(String),

    Comment(String),

    Fn,
    Return,
    If,
    Else,
    Loop,
    Break,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semicolon,

    Identifier(String),
    Float(f64),
    Integer(i64),
    String(String),
    Nil,
    True,
    False,

    And,
    Or,
    ColonEqual,
    Equal,
    Plus,
    Minus,
    Star,
    Slash,
    SlashSlash,
    Caret,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EqualEqual,
    Bang,
    BangEqual,
}

impl Token {
    pub fn describe(&self) -> String {
        use Token::*;

        match self {
            Unknown => "unknown token".into(),
            Eof => "end of file".into(),
            Identifier(i) => format!("identifier '{}'", i),
            Float(f) => format!("float '{}'", f),
            Integer(i) => format!("integer '{}'", i),
            ColonEqual => "':='".into(),
            Nil => "'nil'".into(),
            Fn => "'fn'".into(),
            LParen => "'('".into(),
            RParen => "')'".into(),
            LBrace => "'{'".into(),
            RBrace => "'}'".into(),
            Comma => "','".into(),
            Equal => "'='".into(),
            Plus => "'+'".into(),
            Minus => "'-'".into(),
            Star => "'*'".into(),
            Slash => "'/'".into(),
            Caret => "'^'".into(),
            Return => "'return'".into(),
            Semicolon => "';'".into(),
            String(s) => format!("string '{}'", s),
            Error(e) => format!("error '{}'", e),
            LBracket => "'['".into(),
            RBracket => "']'".into(),
            SlashSlash => "'//'".into(),
            Less => "'<'".into(),
            LessEqual => "'<='".into(),
            Greater => "'>'".into(),
            GreaterEqual => "'>='".into(),
            EqualEqual => "'=='".into(),
            BangEqual => "'!='".into(),
            And => "'and'".into(),
            Or => "'or'".into(),
            Bang => "'!'".into(),
            True => "'true'".into(),
            False => "'false'".into(),
            If => "'if'".into(),
            Else => "'else'".into(),
            Loop => "'loop'".into(),
            Break => "'break'".into(),
            Comment(s) => format!("comment '{s}'"),
        }
    }
}
