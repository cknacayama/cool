use std::borrow::Cow;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Int(&'a str),
    String(Cow<'a, str>),
    Id(&'a str),

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
        match s {
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
