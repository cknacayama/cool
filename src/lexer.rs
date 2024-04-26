use std::borrow::Cow;

use crate::{
    span::Span,
    token::{Token, TokenKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexErrorKind {
    InvalidChar,
    StringUnterminated,
    StringTooLong,
    StringContainsNull,
    CommentUnmatched,
    CommentUnterminated,
    UnexpectedEof,
}

impl std::fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexErrorKind::InvalidChar => write!(f, "invalid character"),
            LexErrorKind::StringUnterminated => write!(f, "unterminated string literal"),
            LexErrorKind::StringTooLong => write!(f, "string literal length > 1024"),
            LexErrorKind::StringContainsNull => {
                write!(f, "string literal contains null character (\\0)")
            }
            LexErrorKind::CommentUnmatched => write!(f, "unmatched comment"),
            LexErrorKind::CommentUnterminated => write!(f, "unterminated comment"),
            LexErrorKind::UnexpectedEof => write!(f, "unexpected end of file"),
        }
    }
}

impl std::error::Error for LexErrorKind {
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

impl std::error::Error for LexError {
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    start: usize,
    cur:   usize,
}

const EOF: u8 = b'\0';

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            start: 0,
            cur:   0,
        }
    }

    fn is_eof(&self) -> bool {
        self.cur >= self.input.len()
    }

    fn at_block_comment_start(&self) -> bool {
        self.peek() == b'(' && self.peek_next() == b'*'
    }

    fn at_block_comment_end(&self) -> bool {
        self.peek() == b'*' && self.peek_next() == b')'
    }

    fn peek(&self) -> u8 {
        self.input.get(self.cur).copied().unwrap_or(EOF)
    }

    fn peek_next(&self) -> u8 {
        self.input.get(self.cur + 1).copied().unwrap_or(EOF)
    }

    fn bump(&mut self) -> Option<u8> {
        self.cur += 1;
        self.input.get(self.cur - 1).copied()
    }

    fn bump_twice(&mut self) -> (Option<u8>, Option<u8>) {
        self.cur += 2;
        let c1 = self.input.get(self.cur - 2).copied();
        let c2 = self.input.get(self.cur - 1).copied();
        (c1, c2)
    }

    fn eat_while<P>(&mut self, mut predicate: P)
    where
        P: FnMut(u8) -> bool,
    {
        while predicate(self.peek()) && !self.is_eof() {
            self.bump();
        }
    }

    fn make_token(&self, kind: TokenKind<'a>) -> Token<'a> {
        let start: u32 = self.start.try_into().expect("span fields must be u32");
        let cur: u32 = self.cur.try_into().expect("span fields must be u32");
        let span = Span::new(start, cur).unwrap();
        Token::new(kind, span)
    }

    fn make_err(&self, kind: LexErrorKind) -> LexError {
        let start: u32 = self.start.try_into().expect("span fields must be u32");
        let cur: u32 = self.cur.try_into().expect("span fields must be u32");
        let span = Span::new(start, cur).unwrap();
        LexError::new(kind, span)
    }

    fn skip_whitespace(&mut self) -> LexResult<()> {
        while !self.is_eof() {
            let c = self.peek();
            if c.is_ascii_whitespace() {
                self.bump();
            } else if c == b'-' && self.peek_next() == b'-' {
                self.bump_twice();
                self.eat_while(|c| c != b'\n');
            } else if c == b'(' && self.peek_next() == b'*' {
                self.start = self.cur;
                self.bump_twice();
                let mut depth = 1;
                while depth > 0 {
                    if self.is_eof() {
                        return Err(self.make_err(LexErrorKind::CommentUnterminated));
                    }

                    if self.at_block_comment_start() {
                        self.bump_twice();
                        depth += 1;
                    } else if self.at_block_comment_end() {
                        self.bump_twice();
                        depth -= 1;
                    } else {
                        self.bump();
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }

    fn integer(&mut self) -> Token<'a> {
        self.eat_while(|c| c.is_ascii_digit());
        let s = &self.input[self.start..self.cur];
        let s = std::str::from_utf8(s).unwrap();
        self.make_token(TokenKind::Int(s))
    }

    fn identifier_or_keyword(&mut self) -> Token<'a> {
        self.eat_while(TokenKind::is_ident_char);
        let s = &self.input[self.start..self.cur];
        let s = std::str::from_utf8(s).unwrap();
        match TokenKind::keyword(s) {
            Some(kw) => self.make_token(kw),
            None => self.make_token(TokenKind::Id(s)),
        }
    }

    fn escaped_string(&mut self, mut s: String) -> LexResult<Token<'a>> {
        while !self.is_eof() {
            match self.peek() {
                b'"' => break,
                EOF => return Err(self.make_err(LexErrorKind::StringContainsNull)),
                b'\n' => {
                    let err = self.make_err(LexErrorKind::StringUnterminated);
                    self.eat_while(|c| c != b'"' && c != b';'); // to continue tokenizng
                    return Err(err);
                }
                b'\\' => {
                    self.bump();
                    match self.peek() {
                        EOF => return Err(self.make_err(LexErrorKind::StringContainsNull)),
                        b'n' => s.push('\n'),
                        b't' => s.push('\t'),
                        b'b' => s.push('\u{0008}'),
                        b'f' => s.push('\u{000C}'),
                        c => s.push(c as char),
                    }
                }
                c => s.push(c as char),
            }
            self.bump();
        }

        if self.is_eof() {
            return Err(self.make_err(LexErrorKind::StringContainsNull));
        }
        self.bump();

        if s.len() > 1024 {
            return Err(self.make_err(LexErrorKind::StringTooLong));
        }

        Ok(self.make_token(TokenKind::String(Cow::Owned(s))))
    }

    fn string(&mut self) -> LexResult<Token<'a>> {
        while !self.is_eof() {
            match self.peek() {
                b'"' => break,
                EOF => return Err(self.make_err(LexErrorKind::StringContainsNull)),
                b'\n' => {
                    let err = self.make_err(LexErrorKind::StringUnterminated);
                    self.eat_while(|c| c != b'"' && c != b';'); // to continue tokenizng
                    return Err(err);
                }
                b'\\' => {
                    self.bump();
                    let s = &self.input[self.start + 1..self.cur - 1];
                    let s = std::str::from_utf8(s).unwrap();
                    let mut s = s.to_string();

                    match self.bump() {
                        Some(b'n') => s.push('\n'),
                        Some(b't') => s.push('\t'),
                        Some(b'b') => s.push('\u{0008}'),
                        Some(b'f') => s.push('\u{000C}'),
                        Some(c) => s.push(c as char),
                        None => return Err(self.make_err(LexErrorKind::StringUnterminated)),
                    }

                    return self.escaped_string(s);
                }
                _ => {}
            }
            self.bump();
        }

        if self.is_eof() {
            return Err(self.make_err(LexErrorKind::StringContainsNull));
        }
        self.bump();

        let s = &self.input[self.start + 1..self.cur - 1];
        let s = std::str::from_utf8(s).unwrap();

        if s.len() > 1024 {
            return Err(self.make_err(LexErrorKind::StringTooLong));
        }

        Ok(self.make_token(TokenKind::String(Cow::Borrowed(s))))
    }

    pub fn next_token(&mut self) -> Option<LexResult<Token<'a>>> {
        self.skip_whitespace().ok()?;

        self.start = self.cur;
        let c = self.bump()?;

        macro_rules! token {
            ($tk:ident) => {{
                Some(Ok(self.make_token(TokenKind::$tk)))
            }};
            ($tk:ident, $c2:expr => $tk2:ident) => {{
                match self.peek() {
                    $c2 => {
                        self.bump();
                        Some(Ok(self.make_token(TokenKind::$tk2)))
                    }
                    _ => Some(Ok(self.make_token(TokenKind::$tk))),
                }
            }};
            ($tk:ident, $c2:expr => $tk2:ident, $c3:expr => $tk3:ident) => {{
                match self.peek() {
                    $c2 => {
                        self.bump();
                        Some(Ok(self.make_token(TokenKind::$tk2)))
                    }
                    $c3 => {
                        self.bump();
                        Some(Ok(self.make_token(TokenKind::$tk3)))
                    }
                    _ => Some(Ok(self.make_token(TokenKind::$tk))),
                }
            }};
        }

        match c {
            EOF => None,

            b'(' => token!(LParen),
            b')' => token!(RParen),
            b'{' => token!(LBrace),
            b'}' => token!(RBrace),
            b'.' => token!(Dot),
            b'@' => token!(At),
            b'~' => token!(Tilde),
            b'*' => token!(Star),
            b'/' => token!(Slash),
            b'+' => token!(Plus),
            b'-' => token!(Minus),
            b'=' => token!(Eq, b'>' => HashRocket),
            b'<' => token!(Less, b'-' => Arrow, b'=' => LessEq),
            b':' => token!(Colon),
            b',' => token!(Comma),
            b';' => token!(Semicolon),
            b'0'..=b'9' => Some(Ok(self.integer())),
            b'"' => Some(self.string()),
            c if TokenKind::is_ident_start(c) => Some(Ok(self.identifier_or_keyword())),
            _ => Some(Err(self.make_err(LexErrorKind::InvalidChar))),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string_literal() {
        let input = r#""hello world""hello \
world"
"hello world

"hello \nworld"

"hello world"#;

        let lexer = Lexer::new(input);
        let tokens = lexer
            .map(|res| match res {
                Ok(tk) => Ok(tk.kind),
                Err(err) => Err(err.kind),
            })
            .collect::<Vec<_>>();

        let expected = vec![
            Ok(TokenKind::String(Cow::from("hello world"))),
            Ok(TokenKind::String(Cow::from("hello \nworld"))),
            Err(LexErrorKind::StringUnterminated),
            Ok(TokenKind::String(Cow::from("hello \nworld"))),
            Err(LexErrorKind::StringContainsNull),
        ];

        assert_eq!(tokens.len(), expected.len());

        for (tk, expected) in tokens.into_iter().zip(expected) {
            assert_eq!(tk, expected);
        }
    }

    #[test]
    fn test_string_too_long() {
        let mut input = String::from('"');
        input.push_str(&"1111".repeat(257));
        input.push('"');

        let mut lexer = Lexer::new(&input);
        let data = lexer.next().unwrap().unwrap_err().kind;
        assert_eq!(data, LexErrorKind::StringTooLong);
    }

    #[test]
    fn test_all_tokens() {
        let input = r#"123 "hello world" abc 
{ } ( ) . @ ~ * / + - = < <= <- => : , ; 
class else false fi 
if in inherits isvoid 
let loop pool then while case esac new of not true"#;

        let lexer = Lexer::new(input);
        let tokens = lexer.map(|res| res.unwrap().kind).collect::<Vec<_>>();
        let expected = vec![
            TokenKind::Int("123"),
            TokenKind::String(Cow::from("hello world")),
            TokenKind::Id("abc"),
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Dot,
            TokenKind::At,
            TokenKind::Tilde,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Eq,
            TokenKind::Less,
            TokenKind::LessEq,
            TokenKind::Arrow,
            TokenKind::HashRocket,
            TokenKind::Colon,
            TokenKind::Comma,
            TokenKind::Semicolon,
            TokenKind::KwClass,
            TokenKind::KwElse,
            TokenKind::KwFalse,
            TokenKind::KwFi,
            TokenKind::KwIf,
            TokenKind::KwIn,
            TokenKind::KwInherits,
            TokenKind::KwIsvoid,
            TokenKind::KwLet,
            TokenKind::KwLoop,
            TokenKind::KwPool,
            TokenKind::KwThen,
            TokenKind::KwWhile,
            TokenKind::KwCase,
            TokenKind::KwEsac,
            TokenKind::KwNew,
            TokenKind::KwOf,
            TokenKind::KwNot,
            TokenKind::KwTrue,
        ];

        assert_eq!(tokens.len(), expected.len());

        for (tk, expected) in tokens.into_iter().zip(expected) {
            assert_eq!(tk, expected);
        }
    }
}
