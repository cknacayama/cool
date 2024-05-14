use std::borrow::Cow;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Int(&'a str),
    String(Cow<'a, str>),
    Id(&'a str),      // identifier (starts with a lower-case letter or an underscore)
    ClassId(&'a str), // Class/Type identifier (starts with an upper-case letter)

    LBrace,
    RBrace,
    LParen,
    RParen,

    Dot,
    At,
    Tilde,
    Star,
    Slash,
    Plus,
    Minus,
    Eq,
    Less,
    LessEq,
    Arrow,      // <-
    HashRocket, // =>

    Colon,
    Comma,
    Semicolon,

    KwClass,
    KwElse,
    KwFalse,
    KwFi,
    KwIf,
    KwIn,
    KwInherits,
    KwIsvoid,
    KwLet,
    KwLoop,
    KwPool,
    KwThen,
    KwWhile,
    KwCase,
    KwEsac,
    KwNew,
    KwOf,
    KwNot,
    KwTrue,
}

impl<'a> TokenKind<'a> {
    pub fn keyword(s: &str) -> Option<TokenKind<'static>> {
        let s = s.to_ascii_lowercase();
        match s.as_str() {
            "class" => Some(TokenKind::KwClass),
            "else" => Some(TokenKind::KwElse),
            "false" => Some(TokenKind::KwFalse),
            "fi" => Some(TokenKind::KwFi),
            "if" => Some(TokenKind::KwIf),
            "in" => Some(TokenKind::KwIn),
            "inherits" => Some(TokenKind::KwInherits),
            "isvoid" => Some(TokenKind::KwIsvoid),
            "let" => Some(TokenKind::KwLet),
            "loop" => Some(TokenKind::KwLoop),
            "pool" => Some(TokenKind::KwPool),
            "then" => Some(TokenKind::KwThen),
            "while" => Some(TokenKind::KwWhile),
            "case" => Some(TokenKind::KwCase),
            "esac" => Some(TokenKind::KwEsac),
            "new" => Some(TokenKind::KwNew),
            "of" => Some(TokenKind::KwOf),
            "not" => Some(TokenKind::KwNot),
            "true" => Some(TokenKind::KwTrue),
            _ => None,
        }
    }

    pub fn is_ident_start(c: u8) -> bool {
        c.is_ascii_alphabetic() || c == b'_'
    }

    pub fn is_ident_char(c: u8) -> bool {
        c.is_ascii_alphanumeric() || c == b'_'
    }
}

impl<'a> std::fmt::Display for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Int(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "{}", s),
            Self::Id(s) => write!(f, "{}", s),
            Self::ClassId(s) => write!(f, "{}", s),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Dot => write!(f, "."),
            Self::At => write!(f, "@"),
            Self::Tilde => write!(f, "~"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Eq => write!(f, "="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Arrow => write!(f, "<-"),
            Self::HashRocket => write!(f, "=>"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Semicolon => write!(f, ";"),
            Self::KwClass => write!(f, "class"),
            Self::KwElse => write!(f, "else"),
            Self::KwFalse => write!(f, "false"),
            Self::KwFi => write!(f, "fi"),
            Self::KwIf => write!(f, "if"),
            Self::KwIn => write!(f, "in"),
            Self::KwInherits => write!(f, "inherits"),
            Self::KwIsvoid => write!(f, "isvoid"),
            Self::KwLet => write!(f, "let"),
            Self::KwLoop => write!(f, "loop"),
            Self::KwPool => write!(f, "pool"),
            Self::KwThen => write!(f, "then"),
            Self::KwWhile => write!(f, "while"),
            Self::KwCase => write!(f, "case"),
            Self::KwEsac => write!(f, "esac"),
            Self::KwNew => write!(f, "new"),
            Self::KwOf => write!(f, "of"),
            Self::KwNot => write!(f, "not"),
            Self::KwTrue => write!(f, "true"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}
