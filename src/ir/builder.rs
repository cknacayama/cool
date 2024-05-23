use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
};

use crate::{
    ast::*,
    index_vec::{index_vec, IndexVec, Key},
    ir::{
        block::{Block, BlockId},
        IrId, Instr, InstrKind, LocalId, Value,
    },
    span::Span,
    types::{ClassEnv, TypeId},
};

#[derive(Debug)]
pub struct Method<'a> {
    class:       TypeId,
    id:          &'a str,
    pub blocks:  IndexVec<BlockId, Block<'a>>,
    cur_locals:  Vec<HashMap<&'a str, IrId>>,
    locals:      IndexVec<LocalId, (TypeId, BlockId)>,
    pub cur_tmp: u32,
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

    pub fn locals(&self) -> &IndexVec<LocalId, (TypeId, BlockId)> {
        &self.locals
    }

    pub fn local_ids(&self) -> impl Iterator<Item = LocalId> {
        (0..self.locals.len()).map(LocalId::from_index)
    }

    pub fn instrs_with_nops(&self) -> impl Iterator<Item = &Instr<'a>> {
        self.blocks.iter().flat_map(|(_, b)| b.instrs.iter())
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

    pub fn remove_nops(&mut self) {
        for (_, block) in self.blocks.iter_mut() {
            block.remove_nops();
        }
    }

    pub fn set_labels(&mut self) {
        for (_, b) in self.blocks.iter_mut() {
            b.set_label();
        }
    }

    fn get_local(&self, name: &'a str) -> IrId {
        self.cur_locals
            .iter()
            .rev()
            .find_map(|locals| locals.get(name))
            .copied()
            .unwrap()
    }

    fn cur_scope_mut(&mut self) -> &mut HashMap<&'a str, IrId> {
        self.cur_locals.last_mut().unwrap()
    }

    fn begin_scope(&mut self) {
        self.cur_locals.push(HashMap::new())
    }

    fn end_scope(&mut self) -> HashMap<&'a str, IrId> {
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
            self.push_instr(InstrKind::Jmp(id), span, TypeId::SelfType);
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

    pub fn new_tmp(&mut self) -> IrId {
        assert!(self.cur_tmp < u32::MAX);
        let tmp = self.cur_tmp;
        self.cur_tmp += 1;
        IrId::Tmp(tmp)
    }

    fn cur_block(&self) -> BlockId {
        BlockId(self.blocks.len() as u32 - 1)
    }

    fn push_instr(&mut self, kind: InstrKind<'a>, span: Span, ty: TypeId) {
        self.blocks.last_mut().unwrap().push_back(kind, span, ty)
    }

    fn set_attrs(&mut self, attrs: impl Iterator<Item = (&'a str, TypeId)> + Clone, span: Span) {
        for (name, ty) in attrs {
            let id = self.new_ptr(name, ty);
            let kind = InstrKind::Attr(ty, id);
            self.push_instr(kind, span, TypeId::SelfType);
        }
    }

    fn set_params(&mut self, locals: impl Iterator<Item = (&'a str, TypeId)> + Clone, span: Span) {
        let tmp = self.new_tmp();
        let kind = InstrKind::Param(self.class, tmp);
        self.push_instr(kind, span, self.class);

        let id = self.new_local("self", self.class);
        let kind = InstrKind::Local(self.class, id);
        self.push_instr(kind, span, TypeId::SelfType);

        let kind = InstrKind::Store(id, self.class, Value::Id(tmp));
        self.push_instr(kind, span, TypeId::SelfType);

        for (name, ty) in locals {
            let tmp = self.new_tmp();
            let kind = InstrKind::Param(ty, tmp);
            self.push_instr(kind, span, ty);

            let id = self.new_local(name, ty);
            let kind = InstrKind::Local(ty, id);
            self.push_instr(kind, span, ty);

            let kind = InstrKind::Store(id, ty, Value::Id(tmp));
            self.push_instr(kind, span, TypeId::SelfType);
        }
    }

    fn new_local(&mut self, name: &'a str, ty: TypeId) -> IrId {
        assert!(self.locals.len() < u32::MAX as usize);
        let local = self.locals.len();
        let id = IrId::Local(LocalId::from_index(local));
        self.cur_scope_mut().insert(name, id);
        self.locals.push((ty, self.cur_block()));
        id
    }

    fn new_ptr(&mut self, name: &'a str, ty: TypeId) -> IrId {
        assert!(self.locals.len() < u32::MAX as usize);
        let local = self.locals.len();
        let id = IrId::Ptr(LocalId::from_index(local));
        self.cur_scope_mut().insert(name, id);
        self.locals.push((ty, self.cur_block()));
        id
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId, span: Span) -> IrId {
        let id = self.new_local(name, ty);
        let kind = InstrKind::Local(ty, id);
        self.push_instr(kind, span, ty);
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

    pub fn instrs_with_nops(&self) -> impl Iterator<Item = &Instr<'a>> {
        self.methods.iter().flat_map(|m| m.instrs_with_nops())
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

    fn get_local(&self, name: &'a str) -> IrId {
        self.cur_method().get_local(name)
    }

    fn push_instr(&mut self, kind: InstrKind<'a>, span: Span, ty: TypeId) {
        self.cur_method_mut().push_instr(kind, span, ty)
    }

    fn begin_scope(&mut self) {
        self.cur_method_mut().begin_scope()
    }

    fn end_scope(&mut self) -> HashMap<&'a str, IrId> {
        self.cur_method_mut().end_scope()
    }

    fn end_method(&mut self) -> HashMap<&'a str, IrId> {
        self.end_scope()
    }

    fn new_tmp(&mut self) -> IrId {
        self.cur_method_mut().new_tmp()
    }

    fn new_ptr(&mut self, name: &'a str, ty: TypeId) -> IrId {
        self.cur_method_mut().new_ptr(name, ty)
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

    fn push_local(&mut self, name: &'a str, ty: TypeId, span: Span) -> IrId {
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

    fn build_method(&mut self, typed_method: TypedMethod<'a>) -> HashMap<&'a str, IrId> {
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
        method.push_instr(kind, span, TypeId::SelfType);
        method.set_params(params.iter().map(|f| (f.id, f.ty)), span);
        method.set_attrs(class_attrs.iter().map(|(k, ty)| (*k, *ty)), span);
        self.methods.push(method);

        let (body, ty) = self.build_expr(typed_method.take_body());
        let kind = InstrKind::Return(Value::Id(body));
        self.push_instr(kind, span, ty);

        self.end_method()
    }

    fn build_maybe_cast(
        &mut self,
        ty: TypeId,
        expr: IrId,
        expr_ty: TypeId,
        span: Span,
    ) -> Option<IrId> {
        match expr_ty {
            TypeId::INT | TypeId::BOOL | TypeId::STRING if ty != expr_ty => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignToObj(tmp, expr_ty, Value::Id(expr));
                self.push_instr(kind, span, ty);
                Some(tmp)
            }
            _ => None,
        }
    }

    fn build_expr(&mut self, expr: TypedExpr<'a>) -> (IrId, TypeId) {
        let TypedExpr { kind, span, ty } = expr;
        use TypedExprKind as TEK;
        match kind {
            TEK::IntLit(i) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::Assign(tmp, Value::Int(i));
                self.push_instr(kind, span, ty);
                (tmp, ty)
            }
            TEK::BoolLit(b) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::Assign(tmp, Value::Bool(b));
                self.push_instr(kind, span, ty);
                (tmp, ty)
            }
            TEK::StringLit(s) => {
                let tmp = self.new_tmp();
                let s = self.intern_string(&s);
                let kind = InstrKind::Assign(tmp, Value::Str(s));
                self.push_instr(kind, span, ty);
                (tmp, ty)
            }
            TEK::Id(id) => {
                let tmp = self.new_tmp();
                let id = self.get_local(id);
                let kind = InstrKind::AssignLoad(tmp, ty, id, 0);
                self.push_instr(kind, span, ty);
                (tmp, ty)
            }
            TEK::Unary(op, expr) => {
                let (expr, _) = self.build_expr(*expr);
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignUn(op, tmp, expr);
                self.push_instr(kind, span, ty);
                (tmp, ty)
            }
            TEK::Binary(op, lhs, rhs) => {
                let (lhs, _) = self.build_expr(*lhs);
                let (rhs, _) = self.build_expr(*rhs);
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignBin(op, tmp, Value::Id(lhs), Value::Id(rhs));
                self.push_instr(kind, span, ty);
                (tmp, ty)
            }
            TEK::New(ty) => {
                let tmp1 = self.new_tmp();
                let kind = InstrKind::Assign(tmp1, Value::Int(0));
                self.push_instr(kind, span, TypeId::INT);

                let tmp2 = self.new_tmp();
                let tmp1 = Value::Id(tmp1);
                let kind =
                    InstrKind::AssignStaticCall(tmp2, ty, "New", Box::new([(TypeId::INT, tmp1)]));
                self.push_instr(kind, span, ty);
                (tmp2, ty)
            }
            TEK::Assign(id, expr) => {
                let (expr, expr_ty) = self.build_expr(*expr);
                let new_expr = self
                    .build_maybe_cast(ty, expr, expr_ty, span)
                    .unwrap_or(expr);
                let id = self.get_local(id);
                let kind = InstrKind::Store(id, ty, Value::Id(new_expr));
                self.push_instr(kind, span, ty);
                (new_expr, ty)
            }
            TEK::SelfId => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignLoad(tmp, self.cur_class, IrId::LOCAL_SELF, 0);
                self.push_instr(kind, span, ty);
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
                            let kind = InstrKind::Store(id, ty, Value::Id(expr));
                            self.push_instr(kind, span, ty);
                        }
                        None => {
                            let id = self.push_local(id, ty, span);
                            let tmp = self.new_tmp();
                            let kind = InstrKind::AssignDefault(tmp, ty);
                            self.push_instr(kind, span, ty);
                            let kind = InstrKind::Store(id, ty, Value::Id(tmp));
                            self.push_instr(kind, span, ty);
                        }
                    }
                }
                let body = self.build_expr(*body);
                self.end_scope();
                body
            }
            TEK::Block(exprs) => {
                let exprs = exprs.into_vec().into_iter();
                exprs.fold((IrId::Tmp(0), TypeId::SelfType), |_, expr| {
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
    ) -> (IrId, TypeId) {
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
    ) -> (IrId, TypeId) {
        let cond_block = self.begin_block(cond.span);
        let (cond, _) = self.build_expr(cond);
        let jmp_0_kind = InstrKind::JmpCond(cond, cond_block, cond_block);
        self.push_instr(jmp_0_kind, span, TypeId::SelfType);

        let body_block = self.begin_block(body.span);
        let _ = self.build_expr(body);
        let jmp_kind = InstrKind::Jmp(cond_block);
        self.push_instr(jmp_kind, span, TypeId::SelfType);

        let end_block = self.begin_block(span);
        let tmp = self.new_tmp();
        let kind = InstrKind::AssignDefault(tmp, TypeId::OBJECT);
        self.push_instr(kind, span, TypeId::OBJECT);
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
    ) -> (IrId, TypeId) {
        let (cond, _) = self.build_expr(cond);

        let cond_block = self.cur_block();
        let jmp_0_kind = InstrKind::JmpCond(cond, cond_block, cond_block);
        self.push_instr(jmp_0_kind, span, TypeId::SelfType);

        let then_block = self.begin_block(then.span);
        let (then, then_ty) = self.build_expr(then);
        let then = self
            .build_maybe_cast(ty, then, then_ty, span)
            .unwrap_or(then);
        let jmp_kind = InstrKind::Jmp(then_block);
        self.push_instr(jmp_kind, span, TypeId::SelfType);

        let else_block = self.begin_block(els.span);
        let (els, els_ty) = self.build_expr(els);
        let els = self.build_maybe_cast(ty, els, els_ty, span).unwrap_or(els);

        let end_block = self.begin_block(span);
        let tmp = self.new_tmp();
        let kind = InstrKind::Phi(
            tmp,
            vec![(Value::Id(then), then_block), (Value::Id(els), else_block)],
        );
        self.push_instr(kind, span, ty);
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
    ) -> (IrId, TypeId) {
        let method_params = self
            .env
            .get_method(self.cur_class, method_name)
            .unwrap()
            .params()
            .to_vec();
        let mut args = self.build_args(args, method_params);

        let tmp1 = self.new_tmp();
        let kind = InstrKind::AssignLoad(tmp1, self.cur_class, IrId::LOCAL_SELF, 0);
        self.push_instr(kind, span, self.cur_class);
        args[0] = (self.cur_class, Value::Id(tmp1));

        let ptr = self.new_ptr("VTable", TypeId::INT);
        let kind = InstrKind::AssignExtract(ptr, tmp1, 1);
        self.push_instr(kind, span, TypeId::INT);

        let offset = self
            .env
            .get_class(self.cur_class)
            .unwrap()
            .get_vtable_offset(method_name)
            .unwrap();

        let tmp3 = self.new_tmp();
        let kind = InstrKind::AssignLoad(tmp3, TypeId::INT, ptr, offset);
        self.push_instr(kind, span, TypeId::INT);

        let tmp4 = self.new_tmp();
        let kind = InstrKind::AssignCall(tmp4, tmp3, args.into_boxed_slice());
        self.push_instr(kind, span, ty);

        (tmp4, ty)
    }

    /// first argument is 'empty'
    fn build_args(
        &mut self,
        args: Box<[TypedExpr<'a>]>,
        params: Vec<TypeId>,
    ) -> Vec<(TypeId, Value)> {
        let mut arg_ids = vec![(TypeId::SelfType, Value::Int(0))];
        for (arg, param) in args.into_vec().into_iter().zip(params) {
            let arg_span = arg.span;
            let (arg, arg_ty) = self.build_expr(arg);
            let arg = self
                .build_maybe_cast(param, arg, arg_ty, arg_span)
                .unwrap_or(arg);
            arg_ids.push((param, Value::Id(arg)));
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
    ) -> (IrId, TypeId) {
        let expr_ty = expr.ty;

        let method_params = self
            .env
            .get_method(expr_ty, method_name)
            .unwrap()
            .params()
            .to_vec();

        let mut args = self.build_args(args, method_params);

        let (expr, _) = self.build_expr(expr);
        args[0] = (expr_ty, Value::Id(expr));

        let ptr = self.new_ptr("VTable", TypeId::INT);
        let kind = InstrKind::AssignExtract(ptr, expr, 1);
        self.push_instr(kind, span, expr_ty);

        let offset = self
            .env
            .get_class(self.cur_class)
            .unwrap()
            .get_vtable_offset(method_name)
            .unwrap();

        let tmp2 = self.new_tmp();
        let kind = InstrKind::AssignLoad(tmp2, TypeId::INT, ptr, offset);
        self.push_instr(kind, span, TypeId::INT);

        let tmp3 = self.new_tmp();
        let kind = InstrKind::AssignCall(tmp3, tmp2, args.into_boxed_slice());
        self.push_instr(kind, span, ty);

        (tmp3, ty)
    }

    fn build_static_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        ty: TypeId,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        span: Span,
        result_ty: TypeId,
    ) -> (IrId, TypeId) {
        let method_params = self
            .env
            .get_method(ty, method_name)
            .unwrap()
            .params()
            .to_vec();

        let mut args = self.build_args(args, method_params);

        let (expr, _) = self.build_expr(expr);
        args[0] = (ty, Value::Id(expr));

        let tmp = self.new_tmp();
        let kind = InstrKind::AssignStaticCall(tmp, ty, method_name, args.into_boxed_slice());
        self.push_instr(kind, span, result_ty);

        (tmp, result_ty)
    }
}
