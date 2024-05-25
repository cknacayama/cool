use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    ast::{TypedCaseArm, TypedClass, TypedExpr, TypedExprKind, TypedFormal, TypedMethod},
    index_vec::{index_vec, IndexVec, Key},
    types::{ClassEnv, TypeId},
};

use super::{
    block::{Block, BlockId},
    GlobalId, Instr, InstrKind, IrId, LocalId, Value,
};

#[derive(Debug)]
pub struct MethodBuilder<'a> {
    class:             TypeId,
    pub(super) blocks: IndexVec<BlockId, Block>,
    cur_locals:        Vec<HashMap<&'a str, IrId>>,
    pub(super) locals: IndexVec<LocalId, (TypeId, BlockId)>,
    cur_tmp:           u32,
}

impl<'a> MethodBuilder<'a> {
    pub fn new(class: TypeId) -> Self {
        Self {
            class,
            cur_tmp: 0,
            blocks: index_vec![Block::new(BlockId::ENTRY)],
            cur_locals: Vec::new(),
            locals: IndexVec::new(),
        }
    }

    pub fn push_front(&mut self, block: BlockId, instr: Instr) {
        self.blocks[block].push_front(instr)
    }

    pub fn locals(&self) -> &IndexVec<LocalId, (TypeId, BlockId)> {
        &self.locals
    }

    pub fn get_instr(&self, index: usize) -> Option<&Instr> {
        self.blocks
            .inner()
            .iter()
            .flat_map(|b| b.instrs.iter())
            .nth(index)
    }

    pub fn get_instr_mut(&mut self, index: usize) -> Option<&mut Instr> {
        self.blocks
            .inner_mut()
            .iter_mut()
            .flat_map(|b| b.instrs.iter_mut())
            .nth(index)
    }

    pub fn local_ids(&self) -> impl Iterator<Item = LocalId> {
        (0..self.locals.len()).map(LocalId::from_index)
    }

    fn instrs_with_nops(&self) -> impl Iterator<Item = &Instr> {
        self.blocks.inner().iter().flat_map(|b| b.instrs.iter())
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr> {
        self.blocks
            .inner()
            .iter()
            .flat_map(|b| b.instrs.iter().filter(|i| !i.kind.is_nop()))
    }

    pub fn instrs_mut(&mut self) -> impl Iterator<Item = &mut Instr> {
        self.blocks
            .inner_mut()
            .iter_mut()
            .flat_map(|b| b.instrs.iter_mut().filter(|i| !i.kind.is_nop()))
    }

    pub fn set_labels(&mut self) {
        for b in self.blocks.inner_mut() {
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

    fn set_dom(&mut self, idom: BlockId, block: BlockId) {
        self.blocks[block].idom = Some(idom);
        self.blocks[idom].sdoms.push(block);
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

    fn last_instr(&self) -> &Instr {
        self.blocks.last().unwrap().instrs.back().unwrap()
    }

    fn begin_block(&mut self) -> BlockId {
        assert!(self.blocks.len() < u32::MAX as usize);
        assert!(self.cur_locals.len() < u32::MAX as usize);
        let id = BlockId(self.blocks.len() as u32);
        let block = Block::new(id);
        if !self.last_instr().kind.is_block_end() {
            self.push_instr(InstrKind::Jmp(id), TypeId::SelfType);
        }
        self.blocks.push(block);
        id
    }

    fn patch_jmp_cond(&mut self, block: BlockId, on_true: BlockId, on_false: BlockId) {
        let instr = self.blocks[block].instrs.back_mut().unwrap();
        match instr.kind {
            InstrKind::JmpCond { src, .. } => {
                instr.kind = InstrKind::JmpCond {
                    src,
                    on_true,
                    on_false,
                };
            }
            _ => unreachable!(),
        }

        self.set_dom(block, on_true);
        self.set_dom(block, on_false);
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

    fn new_tmp(&mut self) -> IrId {
        assert!(self.cur_tmp < u32::MAX);
        let tmp = self.cur_tmp;
        self.cur_tmp += 1;
        IrId::Tmp(tmp)
    }

    fn cur_block(&self) -> BlockId {
        BlockId(self.blocks.len() as u32 - 1)
    }

    fn push_instr(&mut self, kind: InstrKind, ty: TypeId) {
        self.blocks.last_mut().unwrap().push_back(kind, ty)
    }

    fn set_attrs(&mut self, attrs: impl Iterator<Item = (&'a str, TypeId)>) {
        for (name, ty) in attrs {
            let id = self.new_ptr(name, ty);
            let kind = InstrKind::Attr(ty.size_of(), id);
            self.push_instr(kind, TypeId::SelfType);
        }
    }

    fn set_params(&mut self, locals: impl Iterator<Item = (&'a str, TypeId)>) {
        let tmp = self.new_tmp();
        let kind = InstrKind::Param(self.class.size_of(), tmp);
        self.push_instr(kind, self.class);

        let id = self.new_local("self", self.class);
        let kind = InstrKind::Local(self.class.size_of(), id);
        self.push_instr(kind, TypeId::SelfType);

        let kind = InstrKind::Store(id, self.class.size_of(), Value::Id(tmp));
        self.push_instr(kind, TypeId::SelfType);

        for (name, ty) in locals {
            let tmp = self.new_tmp();
            let kind = InstrKind::Param(ty.size_of(), tmp);
            self.push_instr(kind, ty);

            let id = self.new_local(name, ty);
            let kind = InstrKind::Local(ty.size_of(), id);
            self.push_instr(kind, ty);

            let kind = InstrKind::Store(id, ty.size_of(), Value::Id(tmp));
            self.push_instr(kind, TypeId::SelfType);
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

    fn push_local(&mut self, name: &'a str, ty: TypeId) -> IrId {
        let id = self.new_local(name, ty);
        let kind = InstrKind::Local(ty.size_of(), id);
        self.push_instr(kind, ty);
        id
    }

    pub fn block_ids(&self) -> impl Iterator<Item = BlockId> {
        (0..self.blocks.len() as u32).map(BlockId)
    }
}

#[derive(Debug)]
pub enum GlobalValue<'a> {
    Method(MethodBuilder<'a>),
    Vtable(InstrKind),
}

#[derive(Debug)]
pub struct IrBuilder<'a> {
    cur_class:          TypeId,
    env:                ClassEnv<'a>,
    globals_map:        HashMap<(TypeId, &'a str), GlobalId>,
    pub(super) globals: IndexVec<GlobalId, Option<GlobalValue<'a>>>,
    strings:            HashSet<Rc<str>>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(env: ClassEnv<'a>) -> Self {
        let mut globals_map = HashMap::new();
        let mut globals = IndexVec::new();

        for (class, data) in env.classes() {
            let vtable = data
                .vtable()
                .iter()
                .map(|(_, method)| {
                    IrBuilder::new_method(&mut globals_map, &mut globals, class, method)
                })
                .collect();
            IrBuilder::new_vtable(&mut globals_map, &mut globals, class, vtable);
        }

        Self {
            cur_class: TypeId::SelfType,
            env,
            globals_map,
            globals,
            strings: HashSet::from([Rc::from("")]),
        }
    }

    pub fn strings(&self) -> &HashSet<Rc<str>> {
        &self.strings
    }

    pub fn methods(&self) -> impl Iterator<Item = &MethodBuilder<'a>> {
        self.globals.inner().iter().filter_map(|m| {
            m.as_ref().and_then(|m| match m {
                GlobalValue::Method(m) => Some(m),
                _ => None,
            })
        })
    }

    pub fn methods_mut(&mut self) -> impl Iterator<Item = &mut MethodBuilder<'a>> {
        self.globals.inner_mut().iter_mut().filter_map(|m| {
            m.as_mut().and_then(|m| match m {
                GlobalValue::Method(m) => Some(m),
                _ => None,
            })
        })
    }

    fn instrs_with_nops(&self) -> impl Iterator<Item = &InstrKind> {
        let iter = self
            .globals
            .inner()
            .iter()
            .filter_map(|m| {
                m.as_ref().and_then(|m| match m {
                    GlobalValue::Vtable(instr) => Some(instr),
                    _ => None,
                })
            })
            .chain(
                self.methods()
                    .flat_map(|m| m.instrs_with_nops().map(|i| &i.kind)),
            );
        iter
    }

    pub fn instrs(&self) -> impl Iterator<Item = &InstrKind> {
        self.instrs_with_nops().filter(|i| !i.is_nop())
    }

    fn cur_method_mut(&mut self) -> &mut MethodBuilder<'a> {
        match self.globals.last_mut() {
            Some(Some(GlobalValue::Method(m))) => m,
            _ => unreachable!(),
        }
    }

    fn cur_method(&self) -> &MethodBuilder<'a> {
        match self.globals.last() {
            Some(Some(GlobalValue::Method(m))) => m,
            _ => unreachable!(),
        }
    }

    fn get_local(&self, name: &'a str) -> IrId {
        self.cur_method().get_local(name)
    }

    fn push_instr(&mut self, kind: InstrKind, ty: TypeId) {
        self.cur_method_mut().push_instr(kind, ty)
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

    fn begin_block(&mut self) -> BlockId {
        self.cur_method_mut().begin_block()
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

    fn push_local(&mut self, name: &'a str, ty: TypeId) -> IrId {
        self.cur_method_mut().push_local(name, ty)
    }

    fn set_dom(&mut self, idom: BlockId, block: BlockId) {
        self.cur_method_mut().set_dom(idom, block)
    }

    fn intern_string(&mut self, s: &str) -> Rc<str> {
        if let Some(s) = self.strings.get(s) {
            return s.clone();
        }
        let s: Rc<str> = Rc::from(s);
        self.strings.insert(s.clone());
        s
    }

    fn new_vtable(
        globals_map: &mut HashMap<(TypeId, &'a str), GlobalId>,
        globals: &mut IndexVec<GlobalId, Option<GlobalValue<'a>>>,
        ty: TypeId,
        vtable: Box<[GlobalId]>,
    ) -> GlobalId {
        let len = globals.len();
        assert!(len < u32::MAX as usize);
        let id = GlobalId(len as u32);
        globals_map.insert((ty, "Table"), id);
        globals.push(Some(GlobalValue::Vtable(InstrKind::Vtable(id, vtable))));
        id
    }

    fn new_method(
        globals_map: &mut HashMap<(TypeId, &'a str), GlobalId>,
        globals: &mut IndexVec<GlobalId, Option<GlobalValue<'a>>>,
        ty: TypeId,
        name: &'a str,
    ) -> GlobalId {
        use std::collections::hash_map::Entry;

        let len = globals_map.len();

        match globals_map.entry((ty, name)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                assert!(len < u32::MAX as usize);
                let id = GlobalId(len as u32);
                entry.insert(id);
                globals.push(None);
                id
            }
        }
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

        let method_id = self.globals_map.get(&(self.cur_class, id)).unwrap();
        let mut method = MethodBuilder::new(self.cur_class);
        let class_attrs = self.env.get_class(self.cur_class).unwrap().attrs();

        method.begin_scope();
        let mut instr_params = vec![self.cur_class];
        instr_params.extend(params.iter().map(|f| f.ty));
        let kind = InstrKind::Method {
            id:    IrId::Global(*method_id),
            ret:   typed_method.return_ty().size_of(),
            arity: instr_params.len(),
        };
        method.push_instr(kind, TypeId::SelfType);
        method.set_params(params.iter().map(|f| (f.id, f.ty)));
        method.set_attrs(class_attrs.iter().map(|(k, ty)| (*k, *ty)));
        self.globals.push(Some(GlobalValue::Method(method)));

        let (body, ty) = self.build_expr(typed_method.take_body());
        let kind = InstrKind::Return(Value::Id(body));
        self.push_instr(kind, ty);

        self.end_method()
    }

    fn build_maybe_cast(&mut self, ty: TypeId, expr: IrId, expr_ty: TypeId) -> Option<IrId> {
        match expr_ty {
            TypeId::INT | TypeId::BOOL | TypeId::STRING if ty != expr_ty => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignToObj(tmp, expr_ty, Value::Id(expr));
                self.push_instr(kind, ty);
                Some(tmp)
            }
            _ => None,
        }
    }

    fn build_expr(&mut self, expr: TypedExpr<'a>) -> (IrId, TypeId) {
        let TypedExpr { kind, ty, .. } = expr;
        use TypedExprKind as TEK;
        match kind {
            TEK::IntLit(i) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::Assign(tmp, Value::Int(i));
                self.push_instr(kind, ty);
                (tmp, ty)
            }
            TEK::BoolLit(b) => {
                let tmp = self.new_tmp();
                let kind = InstrKind::Assign(tmp, Value::Bool(b));
                self.push_instr(kind, ty);
                (tmp, ty)
            }
            TEK::StringLit(s) => {
                let tmp = self.new_tmp();
                let s = self.intern_string(&s);
                let kind = InstrKind::Assign(tmp, Value::Str(s));
                self.push_instr(kind, ty);
                (tmp, ty)
            }
            TEK::Id(id) => {
                let dst = self.new_tmp();
                let src = self.get_local(id);
                let kind = InstrKind::AssignLoad {
                    dst,
                    size: ty.size_of(),
                    src,
                    offset: 0,
                };
                self.push_instr(kind, ty);
                (dst, ty)
            }
            TEK::Unary(op, expr) => {
                let (src, _) = self.build_expr(*expr);
                let dst = self.new_tmp();
                let kind = InstrKind::AssignUn { dst, op, src };
                self.push_instr(kind, ty);
                (dst, ty)
            }
            TEK::Binary(op, lhs, rhs) => {
                let (lhs, _) = self.build_expr(*lhs);
                let (rhs, _) = self.build_expr(*rhs);
                let dst = self.new_tmp();
                let kind = InstrKind::AssignBin {
                    dst,
                    op,
                    lhs: Value::Id(lhs),
                    rhs: Value::Id(rhs),
                };
                self.push_instr(kind, ty);
                (dst, ty)
            }
            TEK::New(ty) => {
                let tmp1 = self.new_tmp();
                let kind = InstrKind::Assign(tmp1, Value::Int(0));
                self.push_instr(kind, TypeId::INT);

                let tmp2 = self.new_tmp();
                let tmp1 = Value::Id(tmp1);
                let method_id = self
                    .globals_map
                    .get(&(ty, "new"))
                    .map(|&id| IrId::Global(id))
                    .unwrap();
                let kind = InstrKind::AssignCall(
                    tmp2,
                    method_id,
                    Box::new([(TypeId::INT.size_of(), tmp1)]),
                );
                self.push_instr(kind, ty);
                (tmp2, ty)
            }
            TEK::Assign(id, expr) => {
                let (expr, expr_ty) = self.build_expr(*expr);
                let new_expr = self.build_maybe_cast(ty, expr, expr_ty).unwrap_or(expr);
                let id = self.get_local(id);
                let kind = InstrKind::Store(id, ty.size_of(), Value::Id(new_expr));
                self.push_instr(kind, ty);
                (new_expr, ty)
            }
            TEK::SelfId => {
                let tmp = self.new_tmp();
                let kind = InstrKind::AssignLoad {
                    dst:    tmp,
                    size:   self.cur_class.size_of(),
                    src:    IrId::LOCAL_SELF,
                    offset: 0,
                };
                self.push_instr(kind, ty);
                (tmp, ty)
            }
            TEK::SelfDispatch(id, args) => self.build_self_dispatch(id, args, ty),
            TEK::Dispatch(expr, id, args) => self.build_dispatch(*expr, id, args, ty),
            TEK::StaticDispatch(expr, ty, id, args) => {
                self.build_static_dispatch(*expr, ty, id, args, ty)
            }
            TEK::Let(formals, body) => {
                self.begin_scope();
                for (formal, expr) in formals.into_vec() {
                    let TypedFormal { id, ty, .. } = formal;
                    match expr {
                        Some(expr) => {
                            let (expr, expr_ty) = self.build_expr(expr);
                            let id = self.push_local(id, ty);
                            let expr = self.build_maybe_cast(ty, expr, expr_ty).unwrap_or(expr);
                            let kind = InstrKind::Store(id, ty.size_of(), Value::Id(expr));
                            self.push_instr(kind, ty);
                        }
                        None => {
                            let id = self.push_local(id, ty);
                            let tmp = self.new_tmp();
                            let kind = match ty {
                                TypeId::INT => InstrKind::Assign(tmp, Value::Int(0)),
                                TypeId::BOOL => InstrKind::Assign(tmp, Value::Bool(false)),
                                TypeId::STRING => {
                                    let s = self.intern_string("");
                                    InstrKind::Assign(tmp, Value::Str(s))
                                }
                                _ => InstrKind::Assign(tmp, Value::Void),
                            };
                            self.push_instr(kind, ty);
                            let kind = InstrKind::Store(id, ty.size_of(), Value::Id(tmp));
                            self.push_instr(kind, ty);
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
            TEK::If(cond, then, els) => self.build_if(*cond, *then, *els, ty),
            TEK::While(cond, body) => self.build_while(*cond, *body),
            TEK::Case(expr, cases) => self.build_case(*expr, cases),
        }
    }

    fn build_case(
        &mut self,
        expr: TypedExpr<'a>,
        cases: Box<[TypedCaseArm<'a>]>,
    ) -> (IrId, TypeId) {
        let _ = expr;
        let _ = cases;
        todo!()
    }

    fn build_while(&mut self, cond: TypedExpr<'a>, body: TypedExpr<'a>) -> (IrId, TypeId) {
        let cur_block = self.cur_block();
        let cond_block = self.begin_block();
        self.set_dom(cur_block, cond_block);

        let (cond, _) = self.build_expr(cond);
        let jmp_0_kind = InstrKind::JmpCond {
            src:      cond,
            on_true:  cond_block,
            on_false: cond_block,
        };
        self.push_instr(jmp_0_kind, TypeId::SelfType);

        let body_block = self.begin_block();
        let _ = self.build_expr(body);
        let jmp_kind = InstrKind::Jmp(cond_block);
        self.push_instr(jmp_kind, TypeId::SelfType);

        let end_block = self.begin_block();
        let tmp = self.new_tmp();
        let kind = InstrKind::Assign(tmp, Value::Void);
        self.push_instr(kind, TypeId::OBJECT);
        self.patch_jmp_cond(cond_block, body_block, end_block);
        (tmp, TypeId::OBJECT)
    }

    fn build_if(
        &mut self,
        cond: TypedExpr<'a>,
        then: TypedExpr<'a>,
        els: TypedExpr<'a>,
        ty: TypeId,
    ) -> (IrId, TypeId) {
        let (cond, _) = self.build_expr(cond);

        let cond_block = self.cur_block();
        let jmp_0_kind = InstrKind::JmpCond {
            src:      cond,
            on_true:  cond_block,
            on_false: cond_block,
        };
        self.push_instr(jmp_0_kind, TypeId::SelfType);

        let then_block = self.begin_block();
        let (then, then_ty) = self.build_expr(then);
        let then = self.build_maybe_cast(ty, then, then_ty).unwrap_or(then);
        let jmp_kind = InstrKind::Jmp(then_block);
        self.push_instr(jmp_kind, TypeId::SelfType);

        let else_block = self.begin_block();
        let (els, els_ty) = self.build_expr(els);
        let els = self.build_maybe_cast(ty, els, els_ty).unwrap_or(els);

        let end_block = self.begin_block();
        let tmp = self.new_tmp();
        let kind = InstrKind::Phi(
            tmp,
            vec![(Value::Id(then), then_block), (Value::Id(els), else_block)],
        );
        self.push_instr(kind, ty);
        self.patch_jmp_cond(cond_block, then_block, else_block);
        self.patch_jmp(then_block, end_block);
        self.set_dom(cond_block, end_block);

        (tmp, ty)
    }

    fn build_self_dispatch(
        &mut self,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
    ) -> (IrId, TypeId) {
        let method_params = self
            .env
            .get_method(self.cur_class, method_name)
            .unwrap()
            .params()
            .to_vec();
        let mut args = self.build_args(args, method_params);

        let tmp1 = self.new_tmp();
        let kind = InstrKind::AssignLoad {
            dst:    tmp1,
            size:   self.cur_class.size_of(),
            src:    IrId::LOCAL_SELF,
            offset: 0,
        };
        self.push_instr(kind, self.cur_class);
        args[0] = (self.cur_class.size_of(), Value::Id(tmp1));

        let ptr = self.new_ptr("VTable", TypeId::INT);
        let kind = InstrKind::AssignExtract(ptr, tmp1, 1);
        self.push_instr(kind, TypeId::INT);

        let offset = self
            .env
            .get_class(self.cur_class)
            .unwrap()
            .get_vtable_offset(method_name)
            .unwrap();

        let tmp3 = self.new_tmp();
        let kind = InstrKind::AssignLoad {
            dst: tmp3,
            size: TypeId::INT.size_of(),
            src: ptr,
            offset,
        };
        self.push_instr(kind, TypeId::INT);

        let tmp4 = self.new_tmp();
        let kind = InstrKind::AssignCall(tmp4, tmp3, args.into_boxed_slice());
        self.push_instr(kind, ty);

        (tmp4, ty)
    }

    /// first argument is 'empty'
    fn build_args(
        &mut self,
        args: Box<[TypedExpr<'a>]>,
        params: Vec<TypeId>,
    ) -> Vec<(usize, Value)> {
        let mut arg_ids = vec![(0, Value::Int(0))];
        for (arg, param) in args.into_vec().into_iter().zip(params) {
            let (arg, arg_ty) = self.build_expr(arg);
            let arg = self.build_maybe_cast(param, arg, arg_ty).unwrap_or(arg);
            arg_ids.push((param.size_of(), Value::Id(arg)));
        }
        arg_ids
    }

    fn build_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
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
        args[0] = (expr_ty.size_of(), Value::Id(expr));

        let ptr = self.new_ptr("VTable", TypeId::INT);
        let kind = InstrKind::AssignExtract(ptr, expr, 1);
        self.push_instr(kind, expr_ty);

        let offset = self
            .env
            .get_class(self.cur_class)
            .unwrap()
            .get_vtable_offset(method_name)
            .unwrap();

        let tmp2 = self.new_tmp();
        let kind = InstrKind::AssignLoad {
            dst: tmp2,
            size: TypeId::INT.size_of(),
            src: ptr,
            offset,
        };
        self.push_instr(kind, TypeId::INT);

        let tmp3 = self.new_tmp();
        let kind = InstrKind::AssignCall(tmp3, tmp2, args.into_boxed_slice());
        self.push_instr(kind, ty);

        (tmp3, ty)
    }

    fn build_static_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        ty: TypeId,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
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
        args[0] = (ty.size_of(), Value::Id(expr));

        let tmp = self.new_tmp();
        let method_id = self
            .globals_map
            .get(&(ty, method_name))
            .map(|&id| IrId::Global(id))
            .unwrap();
        let kind = InstrKind::AssignCall(tmp, method_id, args.into_boxed_slice());
        self.push_instr(kind, result_ty);

        (tmp, result_ty)
    }
}
