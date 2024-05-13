use std::collections::{HashMap, HashSet};

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
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    id:    BlockId,
    first: usize,
    last:  usize,
    len:   u32,
}

impl Block {
    pub fn new(id: BlockId, first: usize) -> Self {
        Self {
            id,
            first,
            last: first,
            len: 0,
        }
    }

    pub fn id(&self) -> BlockId {
        self.id
    }
}

#[derive(Debug)]
pub struct Method<'a> {
    class:      TypeId,
    id:         &'a str,
    blocks:     IndexVec<BlockId, Block>,
    cur_block:  BlockId,
    cur_locals: Vec<HashMap<&'a str, Id>>,
    locals:     IndexVec<Id, (&'a str, BlockId)>,
    cur_tmp:    usize,
    instrs:     Vec<Instr<'a>>,
}

impl<'a> Method<'a> {
    pub fn new(class: TypeId, id: &'a str) -> Self {
        Self {
            class,
            id,
            cur_tmp: 0,
            instrs: Vec::new(),
            blocks: index_vec![Block::new(BlockId::ENTRY, 0)],
            cur_block: BlockId::ENTRY,
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

    pub fn locals(&self) -> &IndexVec<Id, (&'a str, BlockId)> {
        &self.locals
    }

    pub fn local_ids(&self) -> impl Iterator<Item = Id> {
        (0..self.cur_locals.len()).map(Id::Local)
    }

    pub fn instrs(&self) -> &[Instr<'a>] {
        &self.instrs
    }

    pub fn blocks(&self) -> &[Block] {
        (&self.blocks).into()
    }

    fn get_local(&self, name: &'a str) -> Id {
        self.cur_locals
            .iter()
            .rev()
            .find_map(|locals| locals.get(name))
            .copied()
            .unwrap()
    }

    fn cur_scope(&self) -> &HashMap<&'a str, Id> {
        self.cur_locals.last().unwrap()
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

    fn new_block(&mut self) -> BlockId {
        assert!(self.blocks.len() < u32::MAX as usize);
        let id = BlockId(self.blocks.len() as u32);
        let block = Block::new(id, 0);
        self.blocks.push(block);
        id
    }

    fn last_instr(&self) -> &Instr<'a> {
        self.instrs.last().unwrap()
    }

    fn begin_block(&mut self, block: BlockId, span: Span) -> BlockId {
        if !self.last_instr().kind.block_end() {
            self.push_instr(Instr::new(InstrKind::Jmp(block), span, None));
        }
        let last = self.cur_block;
        self.cur_block = block;
        self.push_instr(Instr::new(InstrKind::Label(block), span, None));
        self.begin_scope();
        last
    }

    fn new_tmp(&mut self) -> Id {
        let tmp = self.cur_tmp;
        self.cur_tmp += 1;
        Id::Tmp(tmp)
    }

    fn push_instr(&mut self, instr: Instr<'a>) {
        let idx = self.cur_block;
        if self.blocks[idx].len == 0 {
            self.blocks[idx].first = self.instrs.len();
        }
        self.blocks[idx].last = self.instrs.len();
        self.blocks[idx].len += 1;
        self.instrs.push(instr)
    }

    fn extend_locals(&mut self, locals: impl Iterator<Item = &'a str> + Clone) {
        let iter = locals.clone().zip((self.locals.len()..).map(Id::Local));
        self.cur_scope_mut().extend(iter);
        self.locals
            .extend(locals.map(|name| (name, self.cur_block)));
    }

    fn new_local(&mut self, name: &'a str) -> Id {
        let local = self.locals.len();
        let id = Id::Local(local);
        self.cur_scope_mut().insert(name, id);
        self.locals.push((name, self.cur_block));
        id
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId, span: Span) -> Id {
        let id = self.new_local(name);
        let kind = InstrKind::Local(id, ty.size_of());
        let instr = Instr::new(kind, span, Some(ty));
        self.push_instr(instr);
        id
    }

    pub fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, mut f: F) {
        let block = &self.blocks[block];

        let last = block.last;

        match self.instrs[last].kind {
            InstrKind::Jmp(target) => {
                f(target);
            }
            InstrKind::JmpIfZero(_, on_false, on_true) => {
                f(on_true);
                f(on_false);
            }
            InstrKind::EndMethod(_) => {}
            _ => unreachable!(),
        }
    }

    pub fn block_instrs(&self, bb: BlockId) -> &[Instr<'a>] {
        let block = &self.blocks[bb];
        &self.instrs[block.first..=block.last]
    }

    pub fn block_instrs_visit<F: FnMut(&Instr)>(&self, bb: BlockId, mut f: F) {
        for instr in self.block_instrs(bb) {
            f(instr)
        }
    }

    fn block_ids(&self) -> impl Iterator<Item = BlockId> {
        (0..self.blocks.len() as u32).map(BlockId)
    }

    pub fn predecessors(&self) -> IndexVec<BlockId, Vec<BlockId>> {
        let mut preds = index_vec![vec![]; self.blocks.len()];

        for bb in self.block_ids() {
            self.successors_visit(bb, |succ| preds[succ].push(bb));
        }

        preds
    }

    fn post_order(&self) -> Vec<BlockId> {
        let mut post_order = vec![];
        let mut visited = index_vec![false; self.blocks.len()];

        fn visit(
            fun: &Method<'_>,
            bb: BlockId,
            post_order: &mut Vec<BlockId>,
            visited: &mut IndexVec<BlockId, bool>,
        ) {
            fun.successors_visit(bb, |succ| {
                if !visited[succ] {
                    visited[succ] = true;
                    visit(fun, succ, post_order, visited);
                }
            });
            post_order.push(bb);
        }
        visit(self, BlockId::ENTRY, &mut post_order, &mut visited);

        post_order
    }

    fn post_order_indices(&self, post_order: &[BlockId]) -> IndexVec<BlockId, Option<u32>> {
        let mut post_idx = index_vec![None; self.blocks.len()];
        for (idx, &bb) in post_order.iter().enumerate() {
            post_idx[bb] = Some(idx as u32);
        }
        post_idx
    }

    pub fn idoms(&self, preds: &IndexVec<BlockId, Vec<BlockId>>) -> IndexVec<BlockId, BlockId> {
        let post_ord = self.post_order();
        let post_idx = self.post_order_indices(&post_ord);

        let mut doms = index_vec![None; self.blocks.len()];
        let b0 = post_idx[BlockId::ENTRY].unwrap();
        doms[b0] = Some(b0);

        let mut changed = true;
        while changed {
            changed = false;
            for block in post_ord.iter().filter(|b| !b.is_entry()).rev().copied() {
                let preds = &preds[block];
                let idx = post_idx[block].unwrap();

                let mut new_idom = post_idx[preds[0]].unwrap();

                for pred in preds
                    .iter()
                    .skip(1)
                    .copied()
                    .filter_map(|pred| post_idx.get(pred).and_then(|idx| *idx))
                {
                    if doms[pred].is_some() {
                        let mut x = new_idom;
                        let mut y = pred;

                        while x != y {
                            while x < y {
                                x = doms[x].unwrap();
                            }
                            while y < x {
                                y = doms[y].unwrap();
                            }
                        }

                        new_idom = x;
                    }
                }

                if doms[idx] != Some(new_idom) {
                    doms[idx] = Some(new_idom);
                    changed = true;
                }
            }
        }

        self.block_ids()
            .map(|bb| match post_idx.get(bb) {
                Some(post_index) => {
                    let post_index = post_index.unwrap();
                    let idom_post_index = doms[post_index].unwrap();
                    let idom = post_ord[idom_post_index.to_index()];
                    idom
                }
                None => bb,
            })
            .collect()
    }

    pub fn dom_frontiers(
        &self,
        preds: &IndexVec<BlockId, Vec<BlockId>>,
        idoms: &IndexVec<BlockId, BlockId>,
    ) -> IndexVec<BlockId, Vec<BlockId>> {
        let mut frontiers = index_vec![vec![]; self.blocks.len()];

        for bb in self.block_ids() {
            let preds = &preds[bb];
            if preds.len() < 2 {
                continue;
            }

            let Some(&idom) = idoms.get(bb) else { continue };

            for pred in preds
                .iter()
                .copied()
                .filter(|pred| pred.is_entry() || pred.ne(&idoms[*pred]))
            {
                let mut at = pred;
                while at != idom {
                    let df = &mut frontiers[at];
                    if !df.contains(&bb) {
                        df.push(bb);
                    }
                    at = idoms[at];
                }
            }
        }

        frontiers
    }

    fn contains_store(&self, block: BlockId, local: Id) -> bool {
        let block = &self.blocks[block];

        self.instrs[block.first..=block.last]
            .iter()
            .find(|instr| matches!(instr.kind, InstrKind::Store(id, _) if id == local))
            .is_some()
    }

    pub fn phi_positions(
        &self,
        preds: &IndexVec<BlockId, Vec<BlockId>>,
        dom_frontiers: &IndexVec<BlockId, Vec<BlockId>>,
    ) -> IndexVec<BlockId, Vec<(Id, Vec<BlockId>)>> {
        todo!()
    }
}

#[derive(Debug)]
pub struct IrBuilder<'a> {
    cur_class: TypeId,
    env:       ClassEnv<'a>,
    methods:   Vec<Method<'a>>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(env: ClassEnv<'a>) -> Self {
        Self {
            cur_class: TypeId::SelfType,
            env,
            methods: Vec::new(),
        }
    }

    pub fn methods(&self) -> &[Method<'a>] {
        &self.methods
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

    fn new_method(&mut self, id: &'a str, params: &[TypedFormal<'a>]) {
        let mut method = Method::new(self.cur_class, id);
        let class_attrs = self.env.get_class(self.cur_class).unwrap().attrs();
        method.begin_scope();
        method.new_local("self");
        let iter = class_attrs
            .keys()
            .copied()
            .chain(params.iter().map(|f| f.id));
        method.extend_locals(iter);
        self.methods.push(method)
    }

    fn new_tmp(&mut self) -> Id {
        self.cur_method_mut().new_tmp()
    }

    fn new_block(&mut self) -> BlockId {
        self.cur_method_mut().new_block()
    }

    fn begin_block(&mut self, block: BlockId, span: Span) -> BlockId {
        self.cur_method_mut().begin_block(block, span)
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId, span: Span) -> Id {
        self.cur_method_mut().push_local(name, ty, span)
    }

    pub fn build_class(&mut self, class: TypedClass<'a>) {
        let TypedClass {
            id,
            type_id,
            methods,
            ..
        } = class;

        self.cur_class = type_id;

        // self.build_new(type_id);

        for method in methods.into_vec() {
            self.build_method(id, method);
        }
    }

    fn build_method(&mut self, type_name: &'a str, method: TypedMethod<'a>) {
        self.new_method(method.id(), method.params());
        let span = method.span;
        let kind = InstrKind::Method(type_name, method.id());
        let instr = Instr::new(kind, span, None);
        self.push_instr(instr);
        let kind = InstrKind::BeginMethod(method.size());
        let instr = Instr::new(kind, span, None);
        self.push_instr(instr);
        let (body, ty) = self.build_expr(method.take_body());
        let kind = InstrKind::EndMethod(body);
        let instr = Instr::new(kind, span, Some(ty));
        self.push_instr(instr);
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
                let instr = Instr::new(kind, span, Some(ty));
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
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::BoolLit(b) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignBool(tmp, b);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::StringLit(s) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignStr(tmp, s);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Id(id) => {
                let tmp = self.new_tmp();
                let id = self.get_local(id);
                let kind = InstrKind::AssignLoad(tmp, id);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Unary(op, expr) => {
                let (expr, _) = self.build_expr(*expr);
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignUn(op, tmp, expr);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Binary(op, lhs, rhs) => {
                let (lhs, _) = self.build_expr(*lhs);
                let (rhs, _) = self.build_expr(*rhs);
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignBin(op, tmp, lhs, rhs);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::New(ty) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignNew(tmp, ty);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::Assign(id, expr) => {
                let (expr, expr_ty) = self.build_expr(*expr);
                let new_expr = self
                    .build_maybe_cast(ty, expr, expr_ty, span)
                    .unwrap_or(expr);
                let id = self.get_local(id);
                let kind = InstrKind::Store(id, new_expr);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (new_expr, ty)
            }
            TEK::SelfId => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignLoad(tmp, Id::LOCAL_SELF);
                let instr = Instr::new(kind, span, Some(ty));
                self.push_instr(instr);
                (tmp, ty)
            }
            TEK::SelfDispatch(id, args) => self.build_self_dispatch(id, args, ty, span),
            TEK::Dispatch(expr, id, args) => self.build_dispatch(*expr, id, args, ty, span),
            TEK::StaticDispatch(expr, ty, id, args) => {
                self.build_static_dispatch(*expr, ty, id, args, span, ty)
            }
            TEK::Let(formals, body) => {
                for (formal, expr) in formals.into_vec() {
                    let TypedFormal { id, ty, span } = formal;
                    match expr {
                        Some(expr) => {
                            let (expr, expr_ty) = self.build_expr(expr);
                            let id = self.push_local(id, ty, span);
                            let expr = self
                                .build_maybe_cast(ty, expr, expr_ty, span)
                                .unwrap_or(expr);
                            let kind = InstrKind::Store(id, expr);
                            let instr = Instr::new(kind, span, Some(ty));
                            self.push_instr(instr);
                        }
                        None => {
                            let id = self.push_local(id, ty, span);
                            let tmp = self.new_tmp();
                            let kind = InstrKind::AssignDefault(tmp);
                            let instr = Instr::new(kind, span, Some(ty));
                            self.push_instr(instr);
                            let kind = InstrKind::Store(id, tmp);
                            let instr = Instr::new(kind, span, Some(ty));
                            self.push_instr(instr);
                        }
                    }
                }
                let body = self.build_expr(*body);
                body
            }
            TEK::Block(exprs) => {
                let exprs = exprs.into_vec().into_iter();
                let block = self.new_block();
                self.begin_block(block, span);
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
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let end_block = self.new_block();
        let _ = self.begin_block(cond_block, cond.span);
        let (cond, _) = self.build_expr(cond);
        let jmp_0_kind = InstrKind::JmpIfZero(cond, end_block, body_block);
        self.push_instr(Instr::new(jmp_0_kind, span, None));

        let _ = self.begin_block(body_block, body.span);
        let _ = self.build_expr(body);
        let jmp_kind = InstrKind::Jmp(cond_block);
        self.push_instr(Instr::new(jmp_kind, span, None));

        let _ = self.begin_block(end_block, span);
        let tmp = self.new_tmp();
        let kind = InstrKind::AssignDefault(tmp);
        let instr = Instr::new(kind, span, Some(TypeId::OBJECT));
        self.push_instr(instr);
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

        let then_block = self.new_block();
        let else_block = self.new_block();
        let end_block = self.new_block();
        let jmp_0_kind = InstrKind::JmpIfZero(cond, else_block, then_block);
        self.push_instr(Instr::new(jmp_0_kind, span, None));

        let _ = self.begin_block(then_block, then.span);
        let (then, then_ty) = self.build_expr(then);
        let then = self
            .build_maybe_cast(ty, then, then_ty, span)
            .unwrap_or(then);
        let jmp_kind = InstrKind::Jmp(end_block);
        self.push_instr(Instr::new(jmp_kind, span, None));

        let _ = self.begin_block(else_block, els.span);
        let (els, els_ty) = self.build_expr(els);
        let els = self.build_maybe_cast(ty, els, els_ty, span).unwrap_or(els);

        let _ = self.begin_block(end_block, span);
        let tmp = self.new_tmp();
        let kind = InstrKind::Phi(tmp, vec![(then, then_block), (els, else_block)]);
        let instr = Instr::new(kind, span, Some(ty));
        self.push_instr(instr);
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
        let len = args.len();
        let args_size = self.build_args(args, method_params, span);

        let tmp1 = self.new_tmp();
        let kind = InstrKind::AssignLoad(tmp1, Id::LOCAL_SELF);
        let instr = Instr::new(kind, span, Some(self.cur_class));
        self.push_instr(instr);

        let tmp2 = self.new_tmp();
        let kind = InstrKind::AssignDispatch(tmp2, tmp1, method_name, len);
        let instr = Instr::new(kind, span, Some(ty));
        self.push_instr(instr);

        let kind = InstrKind::PopArgs(args_size);
        let instr = Instr::new(kind, span, None);
        self.push_instr(instr);
        (tmp2, ty)
    }

    fn build_args(&mut self, args: Box<[TypedExpr<'a>]>, params: Vec<TypeId>, span: Span) -> usize {
        let mut len = 0;
        for (arg, param) in args.into_vec().into_iter().zip(params) {
            let arg_span = arg.span;
            let (arg, arg_ty) = self.build_expr(arg);
            let arg = self
                .build_maybe_cast(param, arg, arg_ty, arg_span)
                .unwrap_or(arg);
            len += if matches!(param, TypeId::INT | TypeId::BOOL) {
                8
            } else {
                16
            };
            let kind = InstrKind::PushArg(arg);
            let instr = Instr::new(kind, span, None);
            self.push_instr(instr);
        }
        len
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

        let len = args.len();
        let args_size = self.build_args(args, method_params, span);

        let tmp = self.new_tmp();
        let kind = InstrKind::AssignDispatch(tmp, expr, method_name, len);
        let instr = Instr::new(kind, span, Some(ty));
        self.push_instr(instr);

        let kind = InstrKind::PopArgs(args_size);
        let instr = Instr::new(kind, span, None);
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
        let len = args.len();
        let args_size = self.build_args(args, method_params, span);

        let (expr, _) = self.build_expr(expr);
        let tmp = self.new_tmp();
        let kind = InstrKind::AssignStaticDispatch(tmp, expr, ty, method_name, len);
        let instr = Instr::new(kind, span, Some(result_ty));
        self.push_instr(instr);

        let kind = InstrKind::PopArgs(args_size);
        let instr = Instr::new(kind, span, None);
        self.push_instr(instr);

        (tmp, result_ty)
    }
}
