use std::collections::{HashMap, HashSet};

use crate::{
    ast::*,
    lexer::{LexError, LexErrorKind, Lexer},
    parser::{ParseError, ParseErrorKind, Parser},
    span::Span,
    types::*,
};

#[derive(Debug)]
pub struct Checker<'a> {
    object_envs: Vec<ObjectEnv<'a>>,
    cur_class:   TypeId,
    class_env:   ClassEnv<'a>,
}

impl<'a> Default for Checker<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Checker<'a> {
    pub fn new() -> Self {
        Self {
            object_envs: vec![],
            class_env:   ClassEnv::with_builtin(),
            cur_class:   TypeId::SelfType,
        }
    }

    fn begin_scope(&mut self) {
        self.object_envs.push(ObjectEnv::new());
    }

    fn end_scope(&mut self) -> Option<ObjectEnv<'a>> {
        self.object_envs.pop()
    }

    fn get_object(&self, id: &'a str) -> Option<TypeId> {
        self.object_envs
            .iter()
            .rev()
            .find_map(|env| env.get(id))
            .or_else(|| self.get_attribute(self.cur_class, id).ok())
    }

    fn get_type(&self, ty: &Type<'a>) -> Option<TypeId> {
        self.class_env.get_type(ty)
    }

    fn get_type_or_err(&self, ty: &Type<'a>) -> Result<TypeId, TypeErrorKind<'a>> {
        self.get_type(ty)
            .ok_or_else(|| TypeErrorKind::UndefinedClass(ty.to_str()))
    }

    fn insert_object(&mut self, id: &'a str, ty: TypeId) -> Option<TypeId> {
        self.object_envs.last_mut().unwrap().insert(id, ty)
    }

    fn insert_class(
        &mut self,
        ty: Type<'a>,
        data: ClassTypeData<'a>,
    ) -> Result<TypeId, TypeErrorKind<'a>> {
        self.class_env.insert_class(ty, data)
    }

    fn get_method(
        &self,
        ty: TypeId,
        method: &'a str,
    ) -> Result<&MethodTypeData, TypeErrorKind<'a>> {
        self.class_env.get_method(ty, method)
    }

    fn insert_method(
        &mut self,
        ty: TypeId,
        method: &'a str,
        data: MethodTypeData,
    ) -> Result<(), TypeErrorKind<'a>> {
        self.class_env.insert_method(ty, method, data)
    }

    fn get_attribute(&self, ty: TypeId, attr: &'a str) -> Result<TypeId, TypeErrorKind<'a>> {
        self.class_env.get_attribute(ty, attr)
    }

    fn insert_attribute(
        &mut self,
        ty: TypeId,
        attr: &'a str,
        data: TypeId,
    ) -> Result<(), TypeErrorKind<'a>> {
        self.class_env.insert_attribute(ty, attr, data)
    }

    fn is_subtype(&self, lhs: TypeId, rhs: TypeId) -> Result<(), TypeErrorKind<'a>> {
        self.class_env.is_subtype(lhs, rhs, self.cur_class)
    }

    pub fn check_class(&mut self, class: Class<'a>) -> TypeResult<'a, TypedClass<'a>> {
        let Class {
            id,
            parent,
            features,
            span,
        } = class;

        let parent = self
            .get_type_or_err(&parent)
            .map_err(|kind| TypeError::new(kind, span))?;

        let data = ClassTypeData::new(Some(parent), HashMap::new(), HashMap::new(), HashSet::new())
            .map_err(|kind| TypeError::new(kind, span))?;

        let ty = Type::Class(id);
        let ty = self
            .insert_class(ty, data)
            .map_err(|kind| TypeError::new(kind, span))?;

        self.cur_class = ty;

        self.begin_scope();
        self.insert_object("self", ty);

        let checked_features = features
            .into_vec()
            .into_iter()
            .map(|feature| self.check_feature(feature))
            .collect::<TypeResult<Box<_>>>()?;
        self.end_scope();

        self.cur_class = TypeId::SelfType;

        Ok(TypedClass::new(id, parent, checked_features, span))
    }

    fn check_feature(&mut self, feature: Feature<'a>) -> TypeResult<'a, TypedFeature<'a>> {
        let ty = self.cur_class;
        match feature.kind {
            FeatureKind::Method {
                id,
                params,
                return_ty,
                body,
            } => {
                let method_ty = MethodTypeData {
                    params:    params
                        .iter()
                        .map(|formal| {
                            self.get_type_or_err(&formal.ty)
                                .map_err(|kind| TypeError::new(kind, formal.span))
                        })
                        .collect::<TypeResult<Box<_>>>()?,
                    return_ty: self
                        .get_type_or_err(&return_ty)
                        .map_err(|kind| TypeError::new(kind, feature.span))?,
                };
                self.insert_method(ty, id, method_ty)
                    .map_err(|kind| TypeError::new(kind, feature.span))?;
                self.check_method(id, params, return_ty, body, feature.span)
            }
            FeatureKind::Attribute {
                id,
                ty: attr_ty,
                init,
            } => {
                let attr_ty = self
                    .get_type_or_err(&attr_ty)
                    .map_err(|kind| TypeError::new(kind, feature.span))?;
                let init = init
                    .map(|init| {
                        let init = self.check_expr(init)?;
                        self.is_subtype(init.ty, attr_ty)
                            .map_err(|kind| TypeError::new(kind, init.span))
                            .map(|_| init)
                    })
                    .transpose()?;
                self.insert_attribute(ty, id, attr_ty)
                    .map_err(|kind| TypeError::new(kind, feature.span))?;
                Ok(TypedFeature::new(
                    TypedFeatureKind::Attribute {
                        id,
                        ty: attr_ty,
                        init,
                    },
                    feature.span,
                ))
            }
        }
    }

    fn check_method(
        &mut self,
        id: &'a str,
        params: Box<[Formal<'a>]>,
        return_ty: Type<'a>,
        body: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedFeature<'a>> {
        self.begin_scope();

        let iter = params.into_vec().into_iter();
        let params = iter
            .map(|param| {
                let ty = self
                    .get_type_or_err(&param.ty)
                    .map_err(|kind| TypeError::new(kind, param.span))?;
                self.insert_object(param.id, ty);
                Ok(TypedFormal::new(param.id, ty, param.span))
            })
            .collect::<TypeResult<Box<_>>>()?;

        let return_ty = self
            .get_type_or_err(&return_ty)
            .map_err(|kind| TypeError::new(kind, span))?;

        let body = self.check_expr(body)?;
        self.is_subtype(body.ty, return_ty)
            .map_err(|kind| TypeError::new(kind, body.span))?;
        self.end_scope();

        Ok(TypedFeature::new(
            TypedFeatureKind::Method {
                id,
                params,
                return_ty,
                body,
            },
            span,
        ))
    }

    fn check_expr(&mut self, expr: Expr<'a>) -> TypeResult<'a, TypedExpr<'a>> {
        let Expr { kind, span } = expr;
        match kind {
            ExprKind::IntLit(int) => Ok(TypedExpr::new(
                TypedExprKind::IntLit(int),
                span,
                TypeId::INT,
            )),
            ExprKind::BoolLit(bool) => Ok(TypedExpr::new(
                TypedExprKind::BoolLit(bool),
                span,
                TypeId::BOOL,
            )),
            ExprKind::StringLit(string) => Ok(TypedExpr::new(
                TypedExprKind::StringLit(string),
                span,
                TypeId::STRING,
            )),
            ExprKind::Id(id) => {
                let ty = self
                    .get_object(id)
                    .ok_or_else(|| TypeError::new(TypeErrorKind::UndefinedObject(id), span))?;
                Ok(TypedExpr::new(TypedExprKind::Id(id), span, ty))
            }
            ExprKind::SelfId => {
                let ty = self.cur_class;
                Ok(TypedExpr::new(TypedExprKind::SelfId, span, ty))
            }
            ExprKind::Unary(op, expr) => self.check_unary_expr(op, *expr, span),
            ExprKind::Binary(op, lhs, rhs) => self.check_binary_expr(op, *lhs, *rhs, span),
            ExprKind::Assign(id, expr) => self.check_assign_expr(id, *expr, span),
            ExprKind::New(ty) => self.check_new_expr(ty, span),
            ExprKind::Block(exprs) => self.check_block_expr(exprs, span),
            ExprKind::If(cond, then, else_) => self.check_if_expr(*cond, *then, *else_, span),
            ExprKind::Case(expr, arms) => self.check_case_expr(*expr, arms, span),
            ExprKind::While(cond, loop_) => self.check_while_expr(*cond, *loop_, span),
            ExprKind::Let(bindings, expr) => self.check_let_expr(bindings, *expr, span),
            ExprKind::SelfDispatch(method, args) => self.check_self_dispatch(method, args, span),
            ExprKind::Dispatch(expr, ty, method, args) => {
                self.check_dispatch_expr(*expr, ty, method, args, span)
            }
        }
    }

    fn check_unary_expr(
        &mut self,
        op: UnOp,
        expr: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let expr = self.check_expr(expr)?;
        match op.type_of(expr.ty) {
            Ok(ty) => Ok(TypedExpr::new(
                TypedExprKind::Unary(op, Box::new(expr)),
                span,
                ty,
            )),
            Err(kind) => Err(TypeError::new(kind, span)),
        }
    }

    fn check_assign_expr(
        &mut self,
        id: &'a str,
        expr: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let expr = self.check_expr(expr)?;
        let ty = self
            .get_object(id)
            .ok_or_else(|| TypeError::new(TypeErrorKind::UndefinedObject(id), span))?;

        self.is_subtype(expr.ty, ty)
            .map_err(|kind| TypeError::new(kind, expr.span))?;

        Ok(TypedExpr::new(
            TypedExprKind::Assign(id, Box::new(expr)),
            span,
            ty,
        ))
    }

    fn check_binary_expr(
        &mut self,
        op: BinOp,
        lhs: Expr<'a>,
        rhs: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let lhs = self.check_expr(lhs)?;
        let rhs = self.check_expr(rhs)?;

        match op.type_of(lhs.ty, rhs.ty) {
            Ok(ty) => Ok(TypedExpr::new(
                TypedExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
                span,
                ty,
            )),
            Err(kind) => Err(TypeError::new(kind, span)),
        }
    }

    fn check_new_expr(&mut self, ty: Type<'a>, span: Span) -> TypeResult<'a, TypedExpr<'a>> {
        let ty = self
            .get_type_or_err(&ty)
            .map_err(|kind| TypeError::new(kind, span))?;

        Ok(TypedExpr::new(TypedExprKind::New(ty), span, ty))
    }

    fn check_if_expr(
        &mut self,
        cond: Expr<'a>,
        then: Expr<'a>,
        else_: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let cond = self.check_expr(cond)?;

        if !cond.ty.is_bool() {
            return Err(TypeError::new(TypeErrorKind::ExpectedBool(cond.ty), span));
        }

        let then = self.check_expr(then)?;
        let else_ = self.check_expr(else_)?;

        let ty = self
            .class_env
            .join(then.ty, else_.ty, self.cur_class)
            .map_err(|kind| TypeError::new(kind, span))?;

        Ok(TypedExpr::new(
            TypedExprKind::If(Box::new(cond), Box::new(then), Box::new(else_)),
            span,
            ty,
        ))
    }

    fn check_block_expr(
        &mut self,
        exprs: Box<[Expr<'a>]>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        self.begin_scope();
        let iter = exprs.into_vec().into_iter();
        let checked_exprs = iter
            .map(|expr| self.check_expr(expr))
            .collect::<TypeResult<Box<_>>>()?;
        let ty = checked_exprs.last().unwrap().ty;
        self.end_scope();
        Ok(TypedExpr::new(
            TypedExprKind::Block(checked_exprs),
            span,
            ty,
        ))
    }

    fn check_case_expr(
        &mut self,
        expr: Expr<'a>,
        arms: Box<[CaseArm<'a>]>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let expr = self.check_expr(expr)?;
        let iter = arms.into_vec().into_iter();

        let checked_arms = iter
            .map(|arm| self.check_case_arm(arm))
            .collect::<TypeResult<Box<_>>>()?;

        let ty = self
            .class_env
            .join_fold(checked_arms.iter().map(|arm| arm.ty), self.cur_class)
            .map_err(|kind| TypeError::new(kind, span))?;

        Ok(TypedExpr::new(
            TypedExprKind::Case(Box::new(expr), checked_arms),
            span,
            ty,
        ))
    }

    fn check_case_arm(&mut self, arm: CaseArm<'a>) -> TypeResult<'a, TypedCaseArm<'a>> {
        let CaseArm { id, ty, expr, span } = arm;
        self.begin_scope();
        let ty = self
            .get_type_or_err(&ty)
            .map_err(|kind| TypeError::new(kind, span))?;
        self.insert_object(id, ty);
        let expr = self.check_expr(expr)?;
        self.end_scope();

        Ok(TypedCaseArm::new(id, ty, expr, span))
    }

    fn check_while_expr(
        &mut self,
        cond: Expr<'a>,
        loop_: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let cond = self.check_expr(cond)?;

        if !cond.ty.is_bool() {
            return Err(TypeError::new(TypeErrorKind::ExpectedBool(cond.ty), span));
        }

        let loop_ = self.check_expr(loop_)?;

        Ok(TypedExpr::new(
            TypedExprKind::While(Box::new(cond), Box::new(loop_)),
            span,
            TypeId::OBJECT,
        ))
    }

    fn check_let_expr(
        &mut self,
        bindings: Box<[(Formal<'a>, Option<Expr<'a>>)]>,
        expr: Expr<'a>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        self.begin_scope();
        let iter = bindings.into_vec().into_iter();
        let checked_bindings = iter
            .map(|binding| self.check_binding(binding))
            .collect::<TypeResult<Box<_>>>()?;
        let expr = self.check_expr(expr)?;
        let ty = expr.ty;
        self.end_scope();

        Ok(TypedExpr::new(
            TypedExprKind::Let(checked_bindings, Box::new(expr)),
            span,
            ty,
        ))
    }

    fn check_binding(
        &mut self,
        (formal, expr): (Formal<'a>, Option<Expr<'a>>),
    ) -> TypeResult<'a, (TypedFormal<'a>, Option<TypedExpr<'a>>)> {
        let Formal { id, ty, span } = formal;
        let ty = self
            .get_type_or_err(&ty)
            .map_err(|kind| TypeError::new(kind, span))?;
        self.insert_object(id, ty);
        let expr = expr
            .map(|expr| {
                let expr = self.check_expr(expr)?;
                self.is_subtype(expr.ty, ty)
                    .map_err(|kind| TypeError::new(kind, expr.span))
                    .map(|_| expr)
            })
            .transpose()?;
        Ok((TypedFormal::new(id, ty, span), expr))
    }

    fn check_self_dispatch(
        &mut self,
        method: &'a str,
        args: Box<[Expr<'a>]>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let args = args
            .into_vec()
            .into_iter()
            .map(|arg| self.check_expr(arg))
            .collect::<TypeResult<Vec<_>>>()?;

        let ty = self.cur_class;
        let method_ty = self
            .get_method(ty, method)
            .map_err(|kind| TypeError::new(kind, span))?;
        let return_ty = method_ty.return_ty.map_self_type(|| ty);

        if args.len() != method_ty.params.len() {
            return Err(TypeError::new(
                TypeErrorKind::ExpectedArity {
                    expected: method_ty.params.len(),
                    found:    args.len(),
                },
                span,
            ));
        }

        let args = args
            .into_iter()
            .zip(method_ty.params.iter().copied())
            .map(|(arg, ty)| {
                self.is_subtype(arg.ty, ty)
                    .map_err(|kind| TypeError::new(kind, arg.span))
                    .map(|_| arg)
            })
            .collect::<TypeResult<Box<_>>>()?;

        Ok(TypedExpr::new(
            TypedExprKind::SelfDispatch(method, args),
            span,
            return_ty,
        ))
    }

    fn check_dispatch_expr(
        &mut self,
        expr: Expr<'a>,
        ty: Option<Type<'a>>,
        method: &'a str,
        args: Box<[Expr<'a>]>,
        span: Span,
    ) -> TypeResult<'a, TypedExpr<'a>> {
        let args = args
            .into_vec()
            .into_iter()
            .map(|arg| self.check_expr(arg))
            .collect::<TypeResult<Vec<_>>>()?;

        let expr = self.check_expr(expr)?;
        let ty = match ty {
            Some(ty) => self
                .get_type_or_err(&ty)
                .and_then(|ty| self.is_subtype(expr.ty, ty).map(|_| ty))
                .map_err(|kind| TypeError::new(kind, span))?,
            None => expr.ty.map_self_type(|| self.cur_class),
        };
        let method_ty = self
            .get_method(ty, method)
            .map_err(|kind| TypeError::new(kind, span))?;

        if args.len() != method_ty.params.len() {
            return Err(TypeError::new(
                TypeErrorKind::ExpectedArity {
                    expected: method_ty.params.len(),
                    found:    args.len(),
                },
                span,
            ));
        }

        let args = args
            .into_iter()
            .zip(method_ty.params.iter().copied())
            .map(|(arg, ty)| {
                self.is_subtype(arg.ty, ty)
                    .map_err(|kind| TypeError::new(kind, arg.span))
                    .map(|_| arg)
            })
            .collect::<TypeResult<Box<_>>>()?;

        let return_ty = method_ty.return_ty.map_self_type(|| ty);

        Ok(TypedExpr::new(
            TypedExprKind::Dispatch(Box::new(expr), ty, method, args),
            span,
            return_ty,
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticErrorKind<'a> {
    LexError(LexErrorKind),
    ParseError(ParseErrorKind),
    TypeError(TypeErrorKind<'a>),
}

impl<'a> std::fmt::Display for SemanticErrorKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticErrorKind::LexError(kind) => write!(f, "{}", kind),
            SemanticErrorKind::ParseError(kind) => write!(f, "{}", kind),
            SemanticErrorKind::TypeError(kind) => write!(f, "{}", kind),
        }
    }
}

impl<'a> std::error::Error for SemanticErrorKind<'a> {
}

impl<'a> From<LexErrorKind> for SemanticErrorKind<'a> {
    fn from(kind: LexErrorKind) -> Self {
        Self::LexError(kind)
    }
}

impl<'a> From<ParseErrorKind> for SemanticErrorKind<'a> {
    fn from(kind: ParseErrorKind) -> Self {
        Self::ParseError(kind)
    }
}

impl<'a> From<TypeErrorKind<'a>> for SemanticErrorKind<'a> {
    fn from(kind: TypeErrorKind<'a>) -> Self {
        Self::TypeError(kind)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticError<'a> {
    pub kind: SemanticErrorKind<'a>,
    pub span: Span,
}

impl<'a> SemanticError<'a> {
    pub fn new(kind: SemanticErrorKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

impl<'a> std::fmt::Display for SemanticError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

impl<'a> std::error::Error for SemanticError<'a> {
}

impl<'a> From<ParseError> for SemanticError<'a> {
    fn from(err: ParseError) -> Self {
        Self::new(SemanticErrorKind::ParseError(err.kind), err.span)
    }
}

impl<'a> From<LexError> for SemanticError<'a> {
    fn from(err: LexError) -> Self {
        Self::new(SemanticErrorKind::LexError(err.kind), err.span)
    }
}

impl<'a> From<TypeError<'a>> for SemanticError<'a> {
    fn from(err: TypeError<'a>) -> Self {
        Self::new(SemanticErrorKind::TypeError(err.kind), err.span)
    }
}

pub type SemanticResult<'a, T> = Result<T, SemanticError<'a>>;

#[derive(Debug)]
pub struct SemanticChecker<'a> {
    parser:  Parser<'a>,
    checker: Checker<'a>,
}

impl<'a> SemanticChecker<'a> {
    pub fn from_input(input: &'a str) -> Self {
        Self::new(Parser::from_input(input))
    }

    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            checker: Checker::new(),
        }
    }

    pub fn check(mut self) -> SemanticResult<'a, (Vec<TypedClass<'a>>, ClassEnv<'a>)> {
        let classes = (&mut self).collect::<SemanticResult<Vec<_>>>()?;
        Ok((classes, self.checker.class_env))
    }
}

impl<'a> Iterator for SemanticChecker<'a> {
    type Item = SemanticResult<'a, TypedClass<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parser.next().map(|class| {
            self.checker
                .check_class(class?)
                .map_err(SemanticError::from)
        })
    }
}

impl<'a> From<Parser<'a>> for SemanticChecker<'a> {
    fn from(parser: Parser<'a>) -> Self {
        Self::new(parser)
    }
}

impl<'a> From<Lexer<'a>> for SemanticChecker<'a> {
    fn from(lexer: Lexer<'a>) -> Self {
        Self::new(Parser::new(lexer))
    }
}
