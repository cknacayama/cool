use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    rc::Rc,
};

use crate::{
    ast::*,
    index_vec::{index_vec, IndexVec, Key},
    ir::*,
    span::Span,
    types::{ClassEnv, TypeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(u32);

impl BlockId {
    pub const ENTRY: BlockId = BlockId(0);

    pub fn is_entry(self) -> bool {
        self == Self::ENTRY
    }
}

impl Key for BlockId {
    fn to_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<'a> {
    id:         BlockId,
    pub scope:  u32,
    pub instrs: VecDeque<Instr<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(id: BlockId, scope: u32) -> Self {
        Self {
            id,
            scope,
            instrs: VecDeque::new(),
        }
    }

    pub fn id(&self) -> BlockId {
        self.id
    }

    fn push_front(&mut self, instr: Instr<'a>) {
        self.instrs.push_front(instr)
    }

    fn push_back(&mut self, instr: Instr<'a>) {
        self.instrs.push_back(instr)
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr<'a>> {
        self.instrs.iter()
    }

    pub fn set_label(&mut self) {
        if self.id.is_entry() {
            return;
        }
        let kind = InstrKind::Label(self.id);
        let instr = Instr::new(kind, Span::default(), TypeId::SelfType);
        self.push_front(instr)
    }

    pub fn instrs_mut(&mut self) -> impl Iterator<Item = &mut Instr<'a>> {
        self.instrs.iter_mut()
    }

    pub fn phis_args(&mut self) -> Vec<&mut Vec<Id>> {
        let mut phis = vec![];
        for instr in self.instrs.iter_mut() {
            if let InstrKind::Phi(_, args) = &mut instr.kind {
                phis.push(args);
            }
        }
        phis
    }
}

#[derive(Debug)]
pub struct Method<'a> {
    class:       TypeId,
    id:          &'a str,
    pub blocks:  IndexVec<BlockId, Block<'a>>,
    cur_locals:  Vec<HashMap<&'a str, Id>>,
    locals:      IndexVec<Id, (&'a str, TypeId, BlockId)>,
    pub cur_tmp: usize,
}

impl<'a> Method<'a> {
    pub fn new(class: TypeId, id: &'a str) -> Self {
        Self {
            class,
            id,
            cur_tmp: 0,
            blocks: index_vec![Block::new(BlockId::ENTRY, 0)],
            cur_locals: Vec::new(),
            locals: IndexVec::new(),
        }
    }

    pub fn class(&self) -> TypeId {
        self.class
    }

    pub fn id(&self) -> &'a str {
        self.id
    }

    pub fn push_front(&mut self, block: BlockId, instr: Instr<'a>) {
        self.blocks[block].push_front(instr)
    }

    pub fn locals(&self) -> &IndexVec<Id, (&'a str, TypeId, BlockId)> {
        &self.locals
    }

    pub fn local_ids(&self) -> impl Iterator<Item = Id> {
        (0..self.locals.len()).map(Id::Local)
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr<'a>> {
        self.blocks
            .iter()
            .flat_map(|(_, b)| b.instrs.iter().filter(|i| !i.kind.is_nop()))
    }

    pub fn instrs_mut(&mut self) -> impl Iterator<Item = &mut Instr<'a>> {
        self.blocks
            .iter_mut()
            .flat_map(|(_, b)| b.instrs.iter_mut().filter(|i| !i.kind.is_nop()))
    }

    pub fn blocks(&self) -> &[Block] {
        (&self.blocks).into()
    }

    pub fn set_labels(&mut self) {
        for (_, b) in self.blocks.iter_mut() {
            b.set_label();
        }
    }

    fn get_local(&self, name: &'a str) -> Id {
        self.cur_locals
            .iter()
            .rev()
            .find_map(|locals| locals.get(name))
            .copied()
            .unwrap()
    }

    fn cur_scope_mut(&mut self) -> &mut HashMap<&'a str, Id> {
        self.cur_locals.last_mut().unwrap()
    }

    fn begin_scope(&mut self) {
        self.cur_locals.push(HashMap::new())
    }

    fn end_scope(&mut self) -> HashMap<&'a str, Id> {
        self.cur_locals.pop().unwrap()
    }

    fn last_instr(&self) -> &Instr<'a> {
        self.blocks.last().unwrap().instrs.back().unwrap()
    }

    fn begin_block(&mut self, span: Span) -> BlockId {
        assert!(self.blocks.len() < u32::MAX as usize);
        assert!(self.cur_locals.len() < u32::MAX as usize);
        let id = BlockId(self.blocks.len() as u32);
        let block = Block::new(id, self.cur_locals.len() as u32);
        if !self.last_instr().kind.is_block_end() {
            self.push_instr(Instr::new(InstrKind::Jmp(id), span, TypeId::SelfType));
        }
        self.blocks.push(block);
        id
    }

    fn patch_jmp_cond(&mut self, block: BlockId, on_true: BlockId, on_false: BlockId) {
        let instr = self.blocks[block].instrs.back_mut().unwrap();
        match instr.kind {
            InstrKind::JmpCond(cond, _, _) => {
                instr.kind = InstrKind::JmpCond(cond, on_true, on_false);
            }
            _ => unreachable!(),
        }
    }

    fn patch_jmp(&mut self, block: BlockId, target: BlockId) {
        let instr = self.blocks[block].instrs.back_mut().unwrap();
        match instr.kind {
            InstrKind::Jmp(_) => {
                instr.kind = InstrKind::Jmp(target);
            }
            _ => unreachable!(),
        }
    }

    pub fn new_tmp(&mut self) -> Id {
        let tmp = self.cur_tmp;
        self.cur_tmp += 1;
        Id::Tmp(tmp)
    }

    fn cur_block(&self) -> BlockId {
        BlockId(self.blocks.len() as u32 - 1)
    }

    fn push_instr(&mut self, instr: Instr<'a>) {
        self.blocks.last_mut().unwrap().push_back(instr)
    }

    fn set_attrs(&mut self, attrs: impl Iterator<Item = (&'a str, TypeId)> + Clone, span: Span) {
        for (name, ty) in attrs {
            let id = self.new_ptr(name, ty);
            let kind = InstrKind::Attr(ty, id);
            let instr = Instr::new(kind, span, TypeId::SelfType);
            self.push_instr(instr);
        }
    }

    fn set_params(&mut self, locals: impl Iterator<Item = (&'a str, TypeId)> + Clone, span: Span) {
        let tmp = self.new_tmp();
        let kind = InstrKind::Param(self.class, tmp);
        let instr = Instr::new(kind, span, self.class);
        self.push_instr(instr);

        let id = self.new_local("self", self.class);
        let kind = InstrKind::Local(self.class, id);
        let instr = Instr::new(kind, span, TypeId::SelfType);
        self.push_instr(instr);

        let kind = InstrKind::Store(id, self.class, tmp);
        let instr = Instr::new(kind, span, TypeId::SelfType);
        self.push_instr(instr);

        for (name, ty) in locals {
            let tmp = self.new_tmp();
            let kind = InstrKind::Param(ty, tmp);
            let instr = Instr::new(kind, span, ty);
            self.push_instr(instr);

            let id = self.new_local(name, ty);
            let kind = InstrKind::Local(ty, id);
            let instr = Instr::new(kind, span, TypeId::SelfType);
            self.push_instr(instr);

            let kind = InstrKind::Store(id, ty, tmp);
            let instr = Instr::new(kind, span, TypeId::SelfType);
            self.push_instr(instr);
        }
    }

    fn new_local(&mut self, name: &'a str, ty: TypeId) -> Id {
        let local = self.locals.len();
        let id = Id::Local(local);
        self.cur_scope_mut().insert(name, id);
        self.locals.push((name, ty, self.cur_block()));
        id
    }

    fn new_ptr(&mut self, name: &'a str, ty: TypeId) -> Id {
        let local = self.locals.len();
        let id = Id::Ptr(local);
        self.cur_scope_mut().insert(name, id);
        self.locals.push((name, ty, self.cur_block()));
        id
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId, span: Span) -> Id {
        let id = self.new_local(name, ty);
        let kind = InstrKind::Local(ty, id);
        let instr = Instr::new(kind, span, ty);
        self.push_instr(instr);
        id
    }

    pub fn block_ids(&self) -> impl Iterator<Item = BlockId> {
        (0..self.blocks.len() as u32).map(BlockId)
    }
}

#[derive(Debug)]
pub struct IrBuilder<'a> {
    cur_class: TypeId,
    env:       ClassEnv<'a>,
    methods:   Vec<Method<'a>>,
    strings:   HashSet<Rc<str>>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(env: ClassEnv<'a>) -> Self {
        Self {
            cur_class: TypeId::SelfType,
            env,
            methods: Vec::new(),
            strings: HashSet::from([Rc::from("")]),
        }
    }

    pub fn strings(&self) -> &HashSet<Rc<str>> {
        &self.strings
    }

    pub fn methods(&self) -> &[Method<'a>] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut [Method<'a>] {
        &mut self.methods
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr<'a>> {
        self.methods.iter().flat_map(|m| m.instrs())
    }

    pub fn blocks(&self) -> impl Iterator<Item = &Block> {
        self.methods.iter().flat_map(|m| m.blocks())
    }

    fn cur_method_mut(&mut self) -> &mut Method<'a> {
        self.methods.last_mut().unwrap()
    }

    fn cur_method(&self) -> &Method<'a> {
        self.methods.last().unwrap()
    }

    fn get_local(&self, name: &'a str) -> Id {
        self.cur_method().get_local(name)
    }

    fn push_instr(&mut self, instr: Instr<'a>) {
        self.cur_method_mut().push_instr(instr)
    }

    fn begin_scope(&mut self) {
        self.cur_method_mut().begin_scope()
    }

    fn end_scope(&mut self) -> HashMap<&'a str, Id> {
        self.cur_method_mut().end_scope()
    }

    fn end_method(&mut self) -> HashMap<&'a str, Id> {
        self.end_scope()
    }

    fn new_tmp(&mut self) -> Id {
        self.cur_method_mut().new_tmp()
    }

    fn begin_block(&mut self, span: Span) -> BlockId {
        self.cur_method_mut().begin_block(span)
    }

    fn patch_jmp_cond(&mut self, block: BlockId, on_true: BlockId, on_false: BlockId) {
        self.cur_method_mut()
            .patch_jmp_cond(block, on_true, on_false)
    }

    fn patch_jmp(&mut self, block: BlockId, target: BlockId) {
        self.cur_method_mut().patch_jmp(block, target)
    }

    fn cur_block(&self) -> BlockId {
        self.cur_method().cur_block()
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId, span: Span) -> Id {
        self.cur_method_mut().push_local(name, ty, span)
    }

    fn intern_string(&mut self, s: &str) -> Rc<str> {
        if let Some(s) = self.strings.get(s) {
            return s.clone();
        }
        let s: Rc<str> = Rc::from(s);
        self.strings.insert(s.clone());
        s
    }

    pub fn build_class(&mut self, class: TypedClass<'a>) {
        let TypedClass {
            type_id, methods, ..
        } = class;

        self.cur_class = type_id;

        // self.build_new(type_id);

        for method in methods.into_vec() {
            self.build_method(method);
        }
    }

    fn build_method(&mut self, typed_method: TypedMethod<'a>) -> HashMap<&'a str, Id> {
        let id = typed_method.id();
        let params = typed_method.params();
        let span = typed_method.span;

        let mut method = Method::new(self.cur_class, id);
        let class_attrs = self.env.get_class(self.cur_class).unwrap().attrs();

        method.begin_scope();
        let mut instr_params = vec![self.cur_class];
        instr_params.extend(params.iter().map(|f| f.ty));
        let kind = InstrKind::Method(
            self.cur_class,
            id,
            typed_method.return_ty(),
            instr_params.len(),
        );
        let instr = Instr::new(kind, span, TypeId::SelfType);
        method.push_instr(instr);
        method.set_params(params.iter().map(|f| (f.id, f.ty)), span);
        method.set_attrs(class_attrs.iter().map(|(k, ty)| (*k, *ty)), span);
        self.methods.push(method);

        let (body, ty) = self.build_expr(typed_method.take_body());
        let kind = InstrKind::Return(body);
        let instr = Instr::new(kind, span, ty);
        self.push_instr(instr);

        self.end_method()
    }

    fn build_maybe_cast(
        &mut self,
        ty: TypeId,
        expr: Id,
        expr_ty: TypeId,
        span: Span,
    ) -> Option<Id> {
        match expr_ty {
            TypeId::INT | TypeId::BOOL | TypeId::STRING if ty != expr_ty => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignToObj(tmp, expr_ty, expr);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                Some(tmp)
            }
            _ => None,
        }
    }

    fn build_expr(&mut self, expr: TypedExpr<'a>) -> (Id, TypeId) {
        let TypedExpr { kind, span, ty } = expr;
        use TypedExprKind as TEK;
        match kind {
            TEK::IntLit(i) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignInt(tmp, i);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::BoolLit(b) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignBool(tmp, b);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::StringLit(s) => {
                let tmp = self.new_tmp();
                let s = self.intern_string(&s);
                let kind = InstrKind::AssignStr(tmp, s);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Id(id) => {
                let tmp = self.new_tmp();
                let id = self.get_local(id);
                let kind = InstrKind::AssignLoad(tmp, ty, id);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Unary(op, expr) => {
                let (expr, _) = self.build_expr(*expr);
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignUn(op, tmp, expr);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Binary(op, lhs, rhs) => {
                let (lhs, _) = self.build_expr(*lhs);
                let (rhs, _) = self.build_expr(*rhs);
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignBin(op, tmp, lhs, rhs);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::New(ty) => {
                let tmp1 = self.new_tmp();
                let kind = InstrKind::AssignInt(tmp1, 0);
                let instr = Instr::new(kind, span, TypeId::INT);
                self.push_instr(instr);

                let tmp2 = self.new_tmp();
                let kind = InstrKind::AssignStaticDispatch(tmp2, tmp1, ty, "New", Box::new([]));
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp2, ty)
            }
            TEK::Assign(id, expr) => {
                let (expr, expr_ty) = self.build_expr(*expr);
                let new_expr = self
                    .build_maybe_cast(ty, expr, expr_ty, span)
                    .unwrap_or(expr);
                let id = self.get_local(id);
                let kind = InstrKind::Store(id, ty, new_expr);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (new_expr, ty)
            }
            TEK::SelfId => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignLoad(tmp, self.cur_class, Id::LOCAL_SELF);
                let instr = Instr::new(kind, span, ty);
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::SelfDispatch(id, args) => self.build_self_dispatch(id, args, ty, span),
            TEK::Dispatch(expr, id, args) => self.build_dispatch(*expr, id, args, ty, span),
            TEK::StaticDispatch(expr, ty, id, args) => {
                self.build_static_dispatch(*expr, ty, id, args, span, ty)
            }
            TEK::Let(formals, body) => {
                self.begin_scope();
                for (formal, expr) in formals.into_vec() {
                    let TypedFormal { id, ty, span } = formal;
                    match expr {
                        Some(expr) => {
                            let (expr, expr_ty) = self.build_expr(expr);
                            let id = self.push_local(id, ty, span);
                            let expr = self
                                .build_maybe_cast(ty, expr, expr_ty, span)
                                .unwrap_or(expr);
                            let kind = InstrKind::Store(id, ty, expr);
                            let instr = Instr::new(kind, span, ty);
                            self.push_instr(instr);
                        }
                        None => {
                            let id = self.push_local(id, ty, span);
                            let tmp = self.new_tmp();
                            let kind = InstrKind::AssignDefault(tmp, ty);
                            let instr = Instr::new(kind, span, ty);
                            self.push_instr(instr);
                            let kind = InstrKind::Store(id, ty, tmp);
                            let instr = Instr::new(kind, span, ty);
                            self.push_instr(instr);
                        }
                    }
                }
                let body = self.build_expr(*body);
                self.end_scope();
                body
            }
            TEK::Block(exprs) => {
                let exprs = exprs.into_vec().into_iter();
                exprs.fold((Id::Tmp(0), TypeId::SelfType), |_, expr| {
                    self.build_expr(expr)
                })
            }
            TEK::If(cond, then, els) => self.build_if(*cond, *then, *els, ty, span),
            TEK::While(cond, body) => self.build_while(*cond, *body, span),
            TEK::Case(expr, cases) => self.build_case(*expr, cases, span),
        }
    }

    fn build_case(
        &mut self,
        expr: TypedExpr<'a>,
        cases: Box<[TypedCaseArm<'a>]>,
        span: Span,
    ) -> (Id, TypeId) {
        let _ = expr;
        let _ = cases;
        let _ = span;
        todo!()
    }

    fn build_while(
        &mut self,
        cond: TypedExpr<'a>,
        body: TypedExpr<'a>,
        span: Span,
    ) -> (Id, TypeId) {
        let cond_block = self.begin_block(cond.span);
        let (cond, _) = self.build_expr(cond);
        let jmp_0_kind = InstrKind::JmpCond(cond, cond_block, cond_block);
        self.push_instr(Instr::new(jmp_0_kind, span, TypeId::SelfType));

        let body_block = self.begin_block(body.span);
        let _ = self.build_expr(body);
        let jmp_kind = InstrKind::Jmp(cond_block);
        self.push_instr(Instr::new(jmp_kind, span, TypeId::SelfType));

        let end_block = self.begin_block(span);
        let tmp = self.new_tmp();
        let kind = InstrKind::AssignDefault(tmp, TypeId::OBJECT);
        let instr = Instr::new(kind, span, TypeId::OBJECT);
        self.push_instr(instr);
        self.patch_jmp_cond(cond_block, body_block, end_block);
        (tmp, TypeId::OBJECT)
    }

    fn build_if(
        &mut self,
        cond: TypedExpr<'a>,
        then: TypedExpr<'a>,
        els: TypedExpr<'a>,
        ty: TypeId,
        span: Span,
    ) -> (Id, TypeId) {
        let (cond, _) = self.build_expr(cond);

        let cond_block = self.cur_block();
        let jmp_0_kind = InstrKind::JmpCond(cond, cond_block, cond_block);
        self.push_instr(Instr::new(jmp_0_kind, span, TypeId::SelfType));

        let then_block = self.begin_block(then.span);
        let (then, then_ty) = self.build_expr(then);
        let then = self
            .build_maybe_cast(ty, then, then_ty, span)
            .unwrap_or(then);
        let jmp_kind = InstrKind::Jmp(then_block);
        self.push_instr(Instr::new(jmp_kind, span, TypeId::SelfType));

        let else_block = self.begin_block(els.span);
        let (els, els_ty) = self.build_expr(els);
        let els = self.build_maybe_cast(ty, els, els_ty, span).unwrap_or(els);

        let end_block = self.begin_block(span);
        let tmp = self.new_tmp();
        let kind = InstrKind::Phi(tmp, vec![then, els]);
        let instr = Instr::new(kind, span, ty);
        self.push_instr(instr);
        self.patch_jmp_cond(cond_block, then_block, else_block);
        self.patch_jmp(then_block, end_block);
        (tmp, ty)
    }

    fn build_self_dispatch(
        &mut self,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
        span: Span,
    ) -> (Id, TypeId) {
        let method_params = self
            .env
            .get_method(self.cur_class, method_name)
            .unwrap()
            .params()
            .to_vec();
        let args = self.build_args(args, method_params);

        let tmp1 = self.new_tmp();
        let kind = InstrKind::AssignLoad(tmp1, self.cur_class, Id::LOCAL_SELF);
        let instr = Instr::new(kind, span, self.cur_class);
        self.push_instr(instr);

        let tmp2 = self.new_tmp();
        let kind = InstrKind::AssignDispatch(tmp2, tmp1, method_name, args.into_boxed_slice());
        let instr = Instr::new(kind, span, ty);
        self.push_instr(instr);

        (tmp2, ty)
    }

    fn build_args(&mut self, args: Box<[TypedExpr<'a>]>, params: Vec<TypeId>) -> Vec<(TypeId, Id)> {
        let mut arg_ids = vec![];
        for (arg, param) in args.into_vec().into_iter().zip(params) {
            let arg_span = arg.span;
            let (arg, arg_ty) = self.build_expr(arg);
            let arg = self
                .build_maybe_cast(param, arg, arg_ty, arg_span)
                .unwrap_or(arg);
            arg_ids.push((param, arg));
        }
        arg_ids
    }

    fn build_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
        span: Span,
    ) -> (Id, TypeId) {
        let (expr, expr_ty) = self.build_expr(expr);

        let method_params = self
            .env
            .get_method(expr_ty, method_name)
            .unwrap()
            .params()
            .to_vec();

        let args = self.build_args(args, method_params);

        let tmp = self.new_tmp();
        let kind = InstrKind::AssignDispatch(tmp, expr, method_name, args.into_boxed_slice());
        let instr = Instr::new(kind, span, ty);
        self.push_instr(instr);

        (tmp, ty)
    }

    fn build_static_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        ty: TypeId,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        span: Span,
        result_ty: TypeId,
    ) -> (Id, TypeId) {
        let method_params = self
            .env
            .get_method(ty, method_name)
            .unwrap()
            .params()
            .to_vec();

        let args = self.build_args(args, method_params);

        let (expr, _) = self.build_expr(expr);
        let tmp = self.new_tmp();
        let kind =
            InstrKind::AssignStaticDispatch(tmp, expr, ty, method_name, args.into_boxed_slice());
        let instr = Instr::new(kind, span, result_ty);
        self.push_instr(instr);

        (tmp, result_ty)
    }
}
