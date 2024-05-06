use std::borrow::Cow;

use crate::{
    span::Span,
    token::TokenKind,
    types::{Type, TypeErrorKind, TypeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
    Eq,
}

impl BinOp {
    pub fn type_of(self, lhs: TypeId, rhs: TypeId) -> Result<TypeId, TypeErrorKind<'static>> {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => match (lhs, rhs) {
                (TypeId::INT, TypeId::INT) => Ok(TypeId::INT),
                (TypeId::INT, rhs_ty) => Err(TypeErrorKind::ExpectedInt(rhs_ty)),
                (lhs_ty, _) => Err(TypeErrorKind::ExpectedInt(lhs_ty)),
            },

            BinOp::Lt | BinOp::Le => match (lhs, rhs) {
                (TypeId::INT, TypeId::INT) => Ok(TypeId::BOOL),
                (TypeId::INT, rhs_ty) => Err(TypeErrorKind::ExpectedInt(rhs_ty)),
                (lhs_ty, _) => Err(TypeErrorKind::ExpectedInt(lhs_ty)),
            },

            BinOp::Eq => match (lhs, rhs) {
                (TypeId::INT, _)
                | (TypeId::STRING, _)
                | (TypeId::BOOL, _)
                | (_, TypeId::INT)
                | (_, TypeId::STRING)
                | (_, TypeId::BOOL) => {
                    if lhs == rhs {
                        Ok(TypeId::BOOL)
                    } else {
                        Err(TypeErrorKind::ExpectedType {
                            expected: lhs,
                            found:    rhs,
                        })
                    }
                }
                _ => Ok(TypeId::BOOL),
            },
        }
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Eq => "=",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Complement,
    Not,
    IsVoid,
}

impl UnOp {
    pub fn type_of(self, ty: TypeId) -> Result<TypeId, TypeErrorKind<'static>> {
        match self {
            UnOp::Complement => match ty {
                TypeId::INT => Ok(TypeId::INT),
                ty => Err(TypeErrorKind::ExpectedInt(ty)),
            },

            UnOp::Not => match ty {
                TypeId::BOOL => Ok(TypeId::BOOL),
                ty => Err(TypeErrorKind::ExpectedBool(ty)),
            },

            UnOp::IsVoid => Ok(TypeId::BOOL),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Formal<'a> {
    pub id:   &'a str,
    pub ty:   Type<'a>,
    pub span: Span,
}

impl<'a> Formal<'a> {
    pub fn new(id: &'a str, ty: Type<'a>, span: Span) -> Self {
        Self { id, ty, span }
    }

    pub fn ty(&self) -> &Type<'a> {
        &self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm<'a> {
    pub id:   &'a str,
    pub ty:   Type<'a>,
    pub expr: Expr<'a>,
    pub span: Span,
}

impl<'a> CaseArm<'a> {
    pub fn new(id: &'a str, ty: Type<'a>, expr: Expr<'a>, span: Span) -> Self {
        Self { id, ty, expr, span }
    }

    pub fn ty(&self) -> &Type<'a> {
        &self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind<'a> {
    SelfId,
    BoolLit(bool),
    IntLit(i64),
    StringLit(Cow<'a, str>),
    Id(&'a str),

    Binary(BinOp, Box<Expr<'a>>, Box<Expr<'a>>),
    Unary(UnOp, Box<Expr<'a>>),

    /// id <- expr
    Assign(&'a str, Box<Expr<'a>>),

    /// expr[@type].id([expr [, expr]*])
    Dispatch(Box<Expr<'a>>, Option<Type<'a>>, &'a str, Box<[Expr<'a>]>),

    /// id([expr [, expr]*]) (self dispatch)
    SelfDispatch(&'a str, Box<[Expr<'a>]>),

    /// if expr then expr else expr fi
    If(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),

    /// while expr loop expr pool
    While(Box<Expr<'a>>, Box<Expr<'a>>),

    /// { [expr;]+ }
    Block(Box<[Expr<'a>]>),

    /// let id : type [<- expr] [, id : type [<- expr]]* in expr
    Let(Box<[(Formal<'a>, Option<Expr<'a>>)]>, Box<Expr<'a>>),

    /// case expr of [id : type => expr;]+ esac
    Case(Box<Expr<'a>>, Box<[CaseArm<'a>]>),

    /// new type
    New(Type<'a>),
}

impl<'a> TryFrom<TokenKind<'a>> for ExprKind<'a> {
    type Error = ();

    fn try_from(value: TokenKind<'a>) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Id("self") => Ok(ExprKind::SelfId),
            TokenKind::Id(id) => Ok(ExprKind::Id(id)),
            TokenKind::Int(int) => Ok(ExprKind::IntLit(int.parse().unwrap())),
            TokenKind::String(s) => Ok(ExprKind::StringLit(s)),
            TokenKind::KwTrue => Ok(ExprKind::BoolLit(true)),
            TokenKind::KwFalse => Ok(ExprKind::BoolLit(false)),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub span: Span,
}

impl<'a> Expr<'a> {
    pub fn new(kind: ExprKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class<'a> {
    pub id:       &'a str,
    pub parent:   Type<'a>,
    pub features: Box<[Feature<'a>]>,
    pub span:     Span,
}

impl<'a> Class<'a> {
    pub fn new(id: &'a str, parent: Type<'a>, features: Box<[Feature<'a>]>, span: Span) -> Self {
        Self {
            id,
            parent,
            features,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FeatureKind<'a> {
    Method {
        id:        &'a str,
        params:    Box<[Formal<'a>]>,
        return_ty: Type<'a>,
        body:      Expr<'a>,
    },

    Attribute {
        id:   &'a str,
        ty:   Type<'a>,
        init: Option<Expr<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Feature<'a> {
    pub kind: FeatureKind<'a>,
    pub span: Span,
}

impl<'a> Feature<'a> {
    pub fn new(kind: FeatureKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedFormal<'a> {
    pub id:   &'a str,
    pub ty:   TypeId,
    pub span: Span,
}

impl<'a> TypedFormal<'a> {
    pub fn new(id: &'a str, ty: TypeId, span: Span) -> Self {
        Self { id, ty, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedCaseArm<'a> {
    pub id:   &'a str,
    pub ty:   TypeId,
    pub expr: TypedExpr<'a>,
    pub span: Span,
}

impl<'a> TypedCaseArm<'a> {
    pub fn new(id: &'a str, ty: TypeId, expr: TypedExpr<'a>, span: Span) -> Self {
        Self { id, ty, expr, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExprKind<'a> {
    SelfId,
    BoolLit(bool),
    IntLit(i64),
    StringLit(Cow<'a, str>),
    Id(&'a str),

    Binary(BinOp, Box<TypedExpr<'a>>, Box<TypedExpr<'a>>),
    Unary(UnOp, Box<TypedExpr<'a>>),

    /// id <- expr
    Assign(&'a str, Box<TypedExpr<'a>>),

    /// expr.id([expr [, expr]*])
    Dispatch(Box<TypedExpr<'a>>, &'a str, Box<[TypedExpr<'a>]>),

    /// expr[@type].id([expr [, expr]*])
    StaticDispatch(Box<TypedExpr<'a>>, TypeId, &'a str, Box<[TypedExpr<'a>]>),

    /// id([expr [, expr]*]) (self dispatch)
    SelfDispatch(&'a str, Box<[TypedExpr<'a>]>),

    /// if expr then expr else expr fi
    If(Box<TypedExpr<'a>>, Box<TypedExpr<'a>>, Box<TypedExpr<'a>>),

    /// while expr loop expr pool
    While(Box<TypedExpr<'a>>, Box<TypedExpr<'a>>),

    /// { [expr;]+ }
    Block(Box<[TypedExpr<'a>]>),

    /// let id : type [<- expr] [, id : type [<- expr]]* in expr
    Let(
        Box<[(TypedFormal<'a>, Option<TypedExpr<'a>>)]>,
        Box<TypedExpr<'a>>,
    ),

    /// case expr of [id : type => expr;]+ esac
    Case(Box<TypedExpr<'a>>, Box<[TypedCaseArm<'a>]>),

    /// new type
    New(TypeId),
}

impl<'a> TryFrom<TokenKind<'a>> for TypedExprKind<'a> {
    type Error = ();

    fn try_from(value: TokenKind<'a>) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Id("self") => Ok(TypedExprKind::SelfId),
            TokenKind::Id(id) => Ok(TypedExprKind::Id(id)),
            TokenKind::Int(int) => Ok(TypedExprKind::IntLit(int.parse().unwrap())),
            TokenKind::String(s) => Ok(TypedExprKind::StringLit(s)),
            TokenKind::KwTrue => Ok(TypedExprKind::BoolLit(true)),
            TokenKind::KwFalse => Ok(TypedExprKind::BoolLit(false)),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedExpr<'a> {
    pub kind: TypedExprKind<'a>,
    pub span: Span,
    pub ty:   TypeId,
}

impl<'a> TypedExpr<'a> {
    pub fn new(kind: TypedExprKind<'a>, span: Span, ty: TypeId) -> Self {
        Self { kind, span, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedClass<'a> {
    pub id:        &'a str,
    pub type_id:   TypeId,
    pub parent:    TypeId,
    pub methods:   Box<[TypedMethod<'a>]>,
    pub attrs:     Box<[TypedAttribute<'a>]>,
    pub init_size: usize,
    pub span:      Span,
}

impl<'a> TypedClass<'a> {
    pub fn new(
        id: &'a str,
        type_id: TypeId,
        parent: TypeId,
        methods: Box<[TypedMethod<'a>]>,
        attrs: Box<[TypedAttribute<'a>]>,
        init_size: usize,
        span: Span,
    ) -> Self {
        Self {
            id,
            type_id,
            parent,
            methods,
            attrs,
            init_size,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedMethod<'a> {
    id:        &'a str,
    params:    Box<[TypedFormal<'a>]>,
    return_ty: TypeId,
    body:      TypedExpr<'a>,
    size:      usize,
    pub span:  Span,
}

impl<'a> TypedMethod<'a> {
    pub fn new(
        id: &'a str,
        params: Box<[TypedFormal<'a>]>,
        return_ty: TypeId,
        body: TypedExpr<'a>,
        size: usize,
        span: Span,
    ) -> Self {
        Self {
            id,
            params,
            return_ty,
            body,
            size,
            span,
        }
    }

    pub fn id(&self) -> &'a str {
        self.id
    }

    pub fn params(&self) -> &[TypedFormal<'a>] {
        &self.params
    }

    pub fn return_ty(&self) -> TypeId {
        self.return_ty
    }

    pub fn body(&self) -> &TypedExpr<'a> {
        &self.body
    }

    pub fn size(&self) -> usize {
        self.size
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedAttribute<'a> {
    id:     &'a str,
    ty:     TypeId,
    init:   Option<TypedExpr<'a>>,
    offset: usize,
    span:   Span,
}

impl<'a> TypedAttribute<'a> {
    pub fn new(
        id: &'a str,
        ty: TypeId,
        init: Option<TypedExpr<'a>>,
        offset: usize,
        span: Span,
    ) -> Self {
        Self {
            id,
            ty,
            init,
            offset,
            span,
        }
    }

    pub fn id(&self) -> &'a str {
        self.id
    }

    pub fn ty(&self) -> TypeId {
        self.ty
    }

    pub fn init(&self) -> Option<&TypedExpr<'a>> {
        self.init.as_ref()
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
