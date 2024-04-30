use std::iter::Peekable;

use crate::{
    ast::{BinOp, CaseArm, Class, Expr, ExprKind, Feature, FeatureKind, Formal, UnOp},
    lexer::{LexError, LexErrorKind, Lexer},
    span::Span,
    token::{Token, TokenKind},
    types::Type,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    LexError(LexErrorKind),
    UnexpectedEof,
    ExpectedToken(TokenKind<'static>),
    ExpectedType,
    ExpectedExpr,
    ExpectedId,
    ExpectedFeature,
}

impl From<LexErrorKind> for ParseErrorKind {
    fn from(kind: LexErrorKind) -> Self {
        Self::LexError(kind)
    }
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::LexError(kind) => write!(f, "{}", kind),
            Self::UnexpectedEof => write!(f, "unexpected end of file"),
            Self::ExpectedToken(token) => write!(f, "expected token: {:?}", token),
            Self::ExpectedType => write!(f, "expected type"),
            Self::ExpectedExpr => write!(f, "expected expression"),
            Self::ExpectedId => write!(f, "expected identifier"),
            Self::ExpectedFeature => write!(f, "expected feature"),
        }
    }
}

impl std::error::Error for ParseErrorKind {
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        Self::new(err.kind.into(), err.span)
    }
}

impl From<&LexError> for ParseError {
    fn from(err: &LexError) -> Self {
        Self::new(err.kind.into(), err.span)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

impl std::error::Error for ParseError {
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer:     Peekable<Lexer<'a>>,
    last_span: Span,
}

impl<'a> Parser<'a> {
    pub fn from_input(input: &'a str) -> Self {
        Self::new(Lexer::new(input))
    }

    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer:     lexer.peekable(),
            last_span: Span::default(),
        }
    }

    fn peek(&mut self) -> Option<ParseResult<&Token<'a>>> {
        self.lexer
            .peek()
            .map(|res| res.as_ref().map_err(ParseError::from))
    }

    fn peek_if(&mut self, kind: TokenKind<'static>) -> bool {
        self.peek()
            .and_then(|res| res.ok())
            .map(|res| res.kind == kind)
            .unwrap_or(false)
    }

    fn next(&mut self) -> Option<ParseResult<Token<'a>>> {
        self.lexer.next().map(|res| match res {
            Ok(token) => {
                self.last_span = token.span;
                Ok(token)
            }
            Err(err) => Err(ParseError::from(err)),
        })
    }

    fn next_or_eof(&mut self) -> ParseResult<Token<'a>> {
        self.next()
            .ok_or_else(|| ParseError::new(ParseErrorKind::UnexpectedEof, self.last_span))?
    }

    fn next_if(&mut self, kind: TokenKind<'static>) -> Option<Token<'a>> {
        let token = self.peek()?.ok()?;
        if token.kind == kind {
            Some(self.next().unwrap().unwrap())
        } else {
            None
        }
    }

    fn expect(&mut self, kind: TokenKind<'static>) -> ParseResult<Span> {
        match self.next_or_eof() {
            Ok(token) if token.kind == kind => Ok(token.span),
            Ok(Token { span, .. }) => {
                Err(ParseError::new(ParseErrorKind::ExpectedToken(kind), span))
            }
            Err(err) if err.kind == ParseErrorKind::UnexpectedEof => Err(ParseError::new(
                ParseErrorKind::ExpectedToken(kind),
                err.span,
            )),
            Err(err) => Err(err),
        }
    }

    fn peek_binop(&mut self) -> Option<BinOp> {
        let token = self.peek()?.ok()?;
        match token.kind {
            TokenKind::Plus => Some(BinOp::Add),
            TokenKind::Minus => Some(BinOp::Sub),
            TokenKind::Star => Some(BinOp::Mul),
            TokenKind::Slash => Some(BinOp::Div),
            TokenKind::Less => Some(BinOp::Lt),
            TokenKind::LessEq => Some(BinOp::Le),
            TokenKind::Eq => Some(BinOp::Eq),
            _ => None,
        }
    }

    fn parse_type(&mut self) -> ParseResult<(Type<'a>, Span)> {
        let token = self.next_or_eof()?;
        let ty = match token.kind {
            TokenKind::Id("SELF_TYPE") => Type::SelfType,
            TokenKind::Id("Bool") => Type::Bool,
            TokenKind::Id("Int") => Type::Int,
            TokenKind::Id("String") => Type::String,
            TokenKind::Id("Object") => Type::Object,
            TokenKind::Id(id) if id.chars().next().unwrap().is_uppercase() => Type::Class(id),
            _ => return Err(ParseError::new(ParseErrorKind::ExpectedType, token.span)),
        };
        Ok((ty, token.span))
    }

    fn parse_expr_list(&mut self) -> ParseResult<(Box<[Expr<'a>]>, Span)> {
        let mut span = self.expect(TokenKind::LParen)?;

        let mut exprs = vec![];

        if !self.peek_if(TokenKind::RParen) {
            exprs.push(self.parse_expr()?);
            while self.next_if(TokenKind::Comma).is_some() {
                exprs.push(self.parse_expr()?);
            }
        }

        span = span.merge(&self.expect(TokenKind::RParen)?);

        Ok((exprs.into_boxed_slice(), span))
    }

    fn parse_expr_block(&mut self, span: Span) -> ParseResult<(Box<[Expr<'a>]>, Span)> {
        let mut exprs = vec![];

        while !self.peek_if(TokenKind::RBrace) {
            exprs.push(self.parse_expr()?);
            if self.next_if(TokenKind::Semicolon).is_none() {
                break;
            }
        }

        let span = span.merge(&self.expect(TokenKind::RBrace)?);

        if exprs.is_empty() {
            Err(ParseError::new(ParseErrorKind::ExpectedExpr, span))
        } else {
            Ok((exprs.into_boxed_slice(), span))
        }
    }

    fn parse_id(&mut self) -> ParseResult<(&'a str, Span)> {
        let token = self.next_or_eof()?;
        match token.kind {
            TokenKind::Id(id) => Ok((id, token.span)),
            _ => Err(ParseError::new(ParseErrorKind::ExpectedId, token.span)),
        }
    }

    fn parse_formal(&mut self) -> ParseResult<Formal<'a>> {
        let (id, span) = self.parse_id()?;
        self.expect(TokenKind::Colon)?;
        let (ty, ty_span) = self.parse_type()?;
        let span = span.merge(&ty_span);
        Ok(Formal::new(id, ty, span))
    }

    pub fn parse(&mut self) -> ParseResult<Vec<Class<'a>>> {
        self.collect()
    }

    pub fn parse_class(&mut self) -> ParseResult<Class<'a>> {
        let span = self.expect(TokenKind::KwClass)?;
        let (id, id_span) = self.parse_id()?;
        if id.chars().next().unwrap().is_lowercase() {
            return Err(ParseError::new(ParseErrorKind::ExpectedType, id_span));
        }
        let parent = if self.next_if(TokenKind::KwInherits).is_some() {
            self.parse_type()?.0
        } else {
            Type::Object
        };
        self.expect(TokenKind::LBrace)?;
        let mut features = vec![];
        while !self.peek_if(TokenKind::RBrace) {
            features.push(self.parse_feature()?);
            self.expect(TokenKind::Semicolon)?;
        }
        self.expect(TokenKind::RBrace)?;
        let span = span.merge(&self.expect(TokenKind::Semicolon)?);

        Ok(Class::new(id, parent, features.into_boxed_slice(), span))
    }

    fn parse_feature(&mut self) -> ParseResult<Feature<'a>> {
        let (id, span) = self.parse_id()?;
        let next = self.next_or_eof()?;
        match next.kind {
            TokenKind::Colon => {
                let (ty, ty_span) = self.parse_type()?;
                let mut span = span.merge(&ty_span);
                let init = if self.next_if(TokenKind::Arrow).is_some() {
                    let expr = self.parse_expr()?;
                    span = span.merge(&expr.span);
                    Some(expr)
                } else {
                    None
                };
                let kind = FeatureKind::Attribute { id, ty, init };
                Ok(Feature::new(kind, span))
            }
            TokenKind::LParen => {
                let mut formals = vec![];
                if !self.peek_if(TokenKind::RParen) {
                    formals.push(self.parse_formal()?);
                    while self.next_if(TokenKind::Comma).is_some() {
                        formals.push(self.parse_formal()?);
                    }
                }
                let span = span.merge(&self.expect(TokenKind::RParen)?);
                self.expect(TokenKind::Colon)?;
                let (return_ty, ty_span) = self.parse_type()?;
                let span = span.merge(&ty_span);
                self.expect(TokenKind::LBrace)?;
                let body = self.parse_expr()?;
                let span = span.merge(&self.expect(TokenKind::RBrace)?);
                let params = formals.into_boxed_slice();
                let kind = FeatureKind::Method {
                    id,
                    params,
                    return_ty,
                    body,
                };
                Ok(Feature::new(kind, span))
            }
            _ => Err(ParseError::new(ParseErrorKind::ExpectedFeature, next.span)),
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expr<'a>> {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.parse_not()?;
        match self.next_if(TokenKind::Arrow) {
            Some(_) => {
                let ExprKind::Id(id) = expr.kind else {
                    return Err(ParseError::new(ParseErrorKind::ExpectedId, expr.span));
                };
                let rhs = self.parse_assign()?;
                let span = expr.span.merge(&rhs.span);
                let kind = ExprKind::Assign(id, Box::new(rhs));
                Ok(Expr::new(kind, span))
            }
            None => Ok(expr),
        }
    }

    fn parse_not(&mut self) -> ParseResult<Expr<'a>> {
        match self.next_if(TokenKind::KwNot) {
            Some(not) => {
                let expr = self.parse_not()?;
                let span = not.span.merge(&expr.span);
                let kind = ExprKind::Unary(UnOp::Not, Box::new(expr));
                Ok(Expr::new(kind, span))
            }
            None => self.parse_cmp(),
        }
    }

    fn parse_cmp(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.parse_add()?;
        match self.peek_binop() {
            Some(op) if matches!(op, BinOp::Lt | BinOp::Le | BinOp::Eq) => {
                self.next();
                let rhs = self.parse_add()?;
                let span = expr.span.merge(&rhs.span);
                let kind = ExprKind::Binary(op, Box::new(expr), Box::new(rhs));
                Ok(Expr::new(kind, span))
            }
            _ => Ok(expr),
        }
    }

    fn parse_add(&mut self) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_mul()?;
        loop {
            match self.peek_binop() {
                Some(op) if matches!(op, BinOp::Add | BinOp::Sub) => {
                    self.next();
                    let rhs = self.parse_mul()?;
                    let span = expr.span.merge(&rhs.span);
                    let kind = ExprKind::Binary(op, Box::new(expr), Box::new(rhs));
                    expr = Expr::new(kind, span);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_mul(&mut self) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_isvoid()?;
        loop {
            match self.peek_binop() {
                Some(op) if matches!(op, BinOp::Mul | BinOp::Div) => {
                    self.next();
                    let rhs = self.parse_isvoid()?;
                    let span = expr.span.merge(&rhs.span);
                    let kind = ExprKind::Binary(op, Box::new(expr), Box::new(rhs));
                    expr = Expr::new(kind, span);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_isvoid(&mut self) -> ParseResult<Expr<'a>> {
        match self.next_if(TokenKind::KwIsvoid) {
            Some(isvoid) => {
                let expr = self.parse_isvoid()?;
                let span = isvoid.span.merge(&expr.span);
                let kind = ExprKind::Unary(UnOp::IsVoid, Box::new(expr));
                Ok(Expr::new(kind, span))
            }
            None => self.parse_complement(),
        }
    }

    fn parse_complement(&mut self) -> ParseResult<Expr<'a>> {
        match self.next_if(TokenKind::Tilde) {
            Some(tilde) => {
                let expr = self.parse_complement()?;
                let span = tilde.span.merge(&expr.span);
                let kind = ExprKind::Unary(UnOp::Complement, Box::new(expr));
                Ok(Expr::new(kind, span))
            }
            None => self.parse_static_dispatch(),
        }
    }

    fn parse_static_dispatch(&mut self) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_dispatch()?;
        while self.next_if(TokenKind::At).is_some() {
            let (ty, _) = self.parse_type()?;
            self.expect(TokenKind::Dot)?;
            let (id, _) = self.parse_id()?;
            let (args, span) = self.parse_expr_list()?;
            let span = expr.span.merge(&span);
            let kind = ExprKind::Dispatch(Box::new(expr), Some(ty), id, args);
            expr = Expr::new(kind, span);
        }
        Ok(expr)
    }

    fn parse_dispatch(&mut self) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_self_dispatch()?;
        while self.next_if(TokenKind::Dot).is_some() {
            let (id, _) = self.parse_id()?;
            let (args, span) = self.parse_expr_list()?;
            let span = expr.span.merge(&span);
            let kind = ExprKind::Dispatch(Box::new(expr), None, id, args);
            expr = Expr::new(kind, span);
        }
        Ok(expr)
    }

    fn parse_self_dispatch(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.parse_primary()?;
        match expr.kind {
            ExprKind::Id(id) if self.peek_if(TokenKind::LParen) => {
                let (args, span) = self.parse_expr_list()?;
                let span = expr.span.merge(&span);
                let kind = ExprKind::SelfDispatch(id, args);
                Ok(Expr::new(kind, span))
            }
            _ => Ok(expr),
        }
    }

    fn parse_primary(&mut self) -> ParseResult<Expr<'a>> {
        let tk = self.next_or_eof()?;

        match tk.kind {
            TokenKind::KwTrue => {
                let kind = ExprKind::BoolLit(true);
                Ok(Expr::new(kind, tk.span))
            }
            TokenKind::KwFalse => {
                let kind = ExprKind::BoolLit(false);
                Ok(Expr::new(kind, tk.span))
            }
            TokenKind::Int(int) => {
                let kind = ExprKind::IntLit(int.parse().unwrap());
                Ok(Expr::new(kind, tk.span))
            }
            TokenKind::String(s) => {
                let kind = ExprKind::StringLit(s);
                Ok(Expr::new(kind, tk.span))
            }
            TokenKind::Id("self") => {
                let kind = ExprKind::SelfId;
                Ok(Expr::new(kind, tk.span))
            }
            TokenKind::Id(id) => {
                let kind = ExprKind::Id(id);
                Ok(Expr::new(kind, tk.span))
            }
            TokenKind::KwNew => {
                let (ty, span) = self.parse_type()?;
                let kind = ExprKind::New(ty);
                let span = span.merge(&tk.span);
                Ok(Expr::new(kind, span))
            }
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                let span = tk.span.merge(&self.expect(TokenKind::RParen)?);
                Ok(Expr::new(expr.kind, span))
            }
            TokenKind::LBrace => {
                let (exprs, span) = self.parse_expr_block(tk.span)?;
                let kind = ExprKind::Block(exprs);
                Ok(Expr::new(kind, span))
            }
            TokenKind::KwIf => self.parse_if(tk.span),
            TokenKind::KwWhile => self.parse_while(tk.span),
            TokenKind::KwLet => self.parse_let(tk.span),
            TokenKind::KwCase => self.parse_case(tk.span),
            _ => Err(ParseError::new(ParseErrorKind::ExpectedExpr, tk.span)),
        }
    }

    fn parse_if(&mut self, span: Span) -> ParseResult<Expr<'a>> {
        let pred = self.parse_expr()?;
        self.expect(TokenKind::KwThen)?;
        let then = self.parse_expr()?;
        self.expect(TokenKind::KwElse)?;
        let else_ = self.parse_expr()?;
        let span = span.merge(&self.expect(TokenKind::KwFi)?);
        let kind = ExprKind::If(Box::new(pred), Box::new(then), Box::new(else_));
        Ok(Expr::new(kind, span))
    }

    fn parse_case(&mut self, span: Span) -> ParseResult<Expr<'a>> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::KwOf)?;
        let mut arms = vec![];
        while !self.peek_if(TokenKind::KwEsac) {
            let (id, span) = self.parse_id()?;
            self.expect(TokenKind::Colon)?;
            let (ty, _) = self.parse_type()?;
            self.expect(TokenKind::HashRocket)?;
            let expr = self.parse_expr()?;
            let span = span.merge(&expr.span);
            arms.push(CaseArm::new(id, ty, expr, span));
            self.expect(TokenKind::Semicolon)?;
        }
        let span = span.merge(&self.expect(TokenKind::KwEsac)?);
        if arms.is_empty() {
            return Err(ParseError::new(ParseErrorKind::ExpectedExpr, span));
        }
        let kind = ExprKind::Case(Box::new(expr), arms.into_boxed_slice());
        Ok(Expr::new(kind, span))
    }

    fn parse_while(&mut self, span: Span) -> ParseResult<Expr<'a>> {
        let pred = self.parse_expr()?;
        self.expect(TokenKind::KwLoop)?;
        let expr = self.parse_expr()?;
        let span = span.merge(&self.expect(TokenKind::KwPool)?);
        let kind = ExprKind::While(Box::new(pred), Box::new(expr));
        Ok(Expr::new(kind, span))
    }

    fn parse_let(&mut self, span: Span) -> ParseResult<Expr<'a>> {
        if self.peek_if(TokenKind::KwIn) {
            return Err(ParseError::new(ParseErrorKind::ExpectedId, span));
        }
        let form = self.parse_formal()?;
        let init = if self.next_if(TokenKind::Arrow).is_some() {
            Some(self.parse_expr()?)
        } else {
            None
        };
        let mut binds = vec![(form, init)];
        while self.next_if(TokenKind::Comma).is_some() {
            let form = self.parse_formal()?;
            let init = if self.next_if(TokenKind::Arrow).is_some() {
                Some(self.parse_expr()?)
            } else {
                None
            };
            binds.push((form, init));
        }
        self.expect(TokenKind::KwIn)?;
        let expr = self.parse_expr()?;
        let span = span.merge(&expr.span);
        let kind = ExprKind::Let(binds.into_boxed_slice(), Box::new(expr));
        Ok(Expr::new(kind, span))
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = ParseResult<Class<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peek().is_some() {
            Some(self.parse_class())
        } else {
            None
        }
    }
}

impl<'a> From<Lexer<'a>> for Parser<'a> {
    fn from(lexer: Lexer<'a>) -> Self {
        Self::new(lexer)
    }
}
