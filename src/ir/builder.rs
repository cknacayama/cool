use std::rc::Rc;

use crate::{
    ast::{
        BinOp, TypedAttribute, TypedCaseArm, TypedClass, TypedExpr, TypedExprKind, TypedFormal,
        TypedMethod,
    },
    fxhash::{FxHashMap, FxHashSet},
    index_vec::{index_vec, IndexVec, Key},
    types::{AttrData, ClassEnv, TypeId},
};

use super::{
    block::{Block, BlockId},
    GlobalId, Instr, InstrKind, IrId, LocalId, Value,
};

#[derive(Debug, Clone)]
pub struct StringSet(FxHashSet<Rc<str>>);

impl StringSet {
    pub fn intern(&mut self, s: &str) -> Rc<str> {
        if let Some(s) = self.0.get(s) {
            return s.clone();
        }
        let s: Rc<str> = s.into();
        self.0.insert(s.clone());
        s
    }

    pub fn get(&self, s: &str) -> Option<&Rc<str>> {
        self.0.get(s)
    }
}

#[derive(Debug)]
pub struct FunctionBuilder<'a> {
    class:               TypeId,
    pub(super) blocks:   IndexVec<BlockId, Block>,
    cur_locals:          Vec<FxHashMap<&'a str, IrId>>,
    pub(super) locals:   IndexVec<LocalId, (TypeId, BlockId)>,
    cur_tmp:             u32,
    is_in_new:           bool,
    pub(super) idoms:    IndexVec<BlockId, BlockId>,
    pub(super) dom_tree: IndexVec<BlockId, Vec<BlockId>>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(class: TypeId) -> Self {
        Self {
            class,
            cur_tmp: 0,
            blocks: index_vec![Block::new(BlockId::ENTRY)],
            cur_locals: Vec::new(),
            locals: IndexVec::new(),
            is_in_new: false,
            idoms: index_vec![BlockId::ENTRY],
            dom_tree: index_vec![Vec::new()],
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

    fn get_local(&self, name: &'a str) -> Option<IrId> {
        self.cur_locals
            .iter()
            .rev()
            .find_map(|locals| locals.get(name))
            .copied()
    }

    fn set_dom(&mut self, idom: BlockId, block: BlockId) {
        self.idoms[block] = idom;
        self.dom_tree[idom].push(block);
    }

    fn set_pred(&mut self, block: BlockId, pred: BlockId) {
        self.blocks[block].preds.push(pred);
        self.blocks[pred].succs.push(block);
    }

    fn cur_scope_mut(&mut self) -> &mut FxHashMap<&'a str, IrId> {
        self.cur_locals.last_mut().unwrap()
    }

    fn begin_scope(&mut self) {
        self.cur_locals.push(FxHashMap::default())
    }

    fn end_scope(&mut self) -> FxHashMap<&'a str, IrId> {
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
        let inserted_jmp = !self.last_instr().kind.is_block_end();
        if inserted_jmp {
            self.push_instr(InstrKind::Jmp(id), TypeId::SelfType);
        }
        let cur_block = self.cur_block();
        self.blocks.push(block);
        self.idoms.push(BlockId::ENTRY);
        self.dom_tree.push(Vec::new());
        if inserted_jmp {
            self.set_pred(id, cur_block);
        }
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

        self.set_pred(on_true, block);
        self.set_pred(on_false, block);

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
        self.set_pred(target, block);
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

    fn set_attrs(&mut self, attrs: &[(&'a str, AttrData, usize)]) {
        if attrs.is_empty() {
            return;
        }

        let tmp1 = self.new_tmp();
        let kind = InstrKind::AssignLoad {
            dst:    tmp1,
            size:   self.class.size_of(),
            src:    IrId::LOCAL_SELF,
            offset: 0,
        };
        self.push_instr(kind, self.class);
        let tmp = self.new_tmp();
        let kind = InstrKind::AssignExtract(tmp, tmp1, 0);
        self.push_instr(kind, TypeId::INT);

        for (name, data, offset) in attrs {
            if self.get_local(name).is_some() {
                continue;
            }
            let ptr = self.new_ptr(name, data.ty);
            let kind = InstrKind::AssignBin {
                dst: ptr,
                op:  BinOp::Add,
                lhs: Value::Id(tmp),
                rhs: Value::Int(*offset as i64),
            };
            self.push_instr(kind, TypeId::INT);
        }
    }

    fn set_self(&mut self) {
        let size = if self.is_in_new {
            TypeId::INT.size_of()
        } else {
            self.class.size_of()
        };
        let dst = self.new_local("self", self.class);
        let kind = InstrKind::Local(size, dst);
        self.push_instr(kind, TypeId::SelfType);

        let kind = InstrKind::Store {
            dst,
            size,
            src: Value::Id(IrId::Tmp(0)),
        };
        self.push_instr(kind, TypeId::SelfType);
    }

    fn set_params(&mut self, locals: impl Iterator<Item = (&'a str, TypeId)>) {
        self.set_self();

        for (pos, (name, ty)) in locals.enumerate() {
            let dst = self.new_local(name, ty);
            let kind = InstrKind::Local(ty.size_of(), dst);
            self.push_instr(kind, ty);

            let tmp = IrId::Tmp(pos as u32 + 1);
            let kind = InstrKind::Store {
                dst,
                size: ty.size_of(),
                src: Value::Id(tmp),
            };
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
    Function(FunctionBuilder<'a>),
    Vtable(InstrKind),
}

#[derive(Debug)]
pub struct Global<'a> {
    pub(super) name:  Rc<str>,
    pub(super) value: Option<GlobalValue<'a>>,
}

#[derive(Debug)]
pub struct IrBuilder<'a> {
    cur_class:          TypeId,
    cur_function:       GlobalId,
    env:                ClassEnv<'a>,
    globals_map:        FxHashMap<(TypeId, &'a str), GlobalId>,
    pub(super) globals: IndexVec<GlobalId, Global<'a>>,
    strings:            StringSet,
    class_sizes:        IndexVec<TypeId, usize>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(env: ClassEnv<'a>) -> Self {
        let mut globals_map = FxHashMap::default();
        let mut globals = IndexVec::new();
        let mut strings = StringSet(FxHashSet::default());
        strings.0.insert(Rc::from(""));

        let mut count = 0;
        for (class, data) in env.classes() {
            count += 1;
            let class_name = data.id();
            let vtable = data
                .vtable()
                .iter()
                .skip(1) // parent vtable
                .map(|(ty, function)| {
                    let ty_name = env.get_class(*ty).unwrap().id();
                    IrBuilder::new_function(
                        &mut strings,
                        &mut globals_map,
                        &mut globals,
                        *ty,
                        ty_name,
                        function,
                    )
                })
                .collect();
            IrBuilder::new_vtable(
                &mut strings,
                &mut globals_map,
                &mut globals,
                class,
                class_name,
                vtable,
            );
        }

        let mut class_sizes = index_vec![0; count];
        class_sizes[TypeId::INT] = 8;
        class_sizes[TypeId::BOOL] = 1;
        class_sizes[TypeId::STRING] = 16;

        Self {
            cur_class: TypeId::SelfType,
            cur_function: GlobalId(0),
            env,
            globals_map,
            globals,
            strings,
            class_sizes,
        }
    }

    pub fn strings(&self) -> &StringSet {
        &self.strings
    }

    pub fn functions(&self) -> impl Iterator<Item = &FunctionBuilder<'a>> {
        self.globals.inner().iter().filter_map(|m| {
            m.value.as_ref().and_then(|m| match m {
                GlobalValue::Function(m) => Some(m),
                _ => None,
            })
        })
    }

    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionBuilder<'a>> {
        self.globals.inner_mut().iter_mut().filter_map(|m| {
            m.value.as_mut().and_then(|m| match m {
                GlobalValue::Function(m) => Some(m),
                _ => None,
            })
        })
    }

    pub fn globals_names(&self) -> impl Iterator<Item = (GlobalId, &Rc<str>)> {
        self.globals.iter().map(|(id, m)| (id, &m.name))
    }

    fn instrs_with_nops(&self) -> impl Iterator<Item = &InstrKind> {
        let iter = self
            .globals
            .inner()
            .iter()
            .filter_map(|m| {
                m.value.as_ref().and_then(|m| match m {
                    GlobalValue::Vtable(instr) => Some(instr),
                    _ => None,
                })
            })
            .chain(
                self.functions()
                    .flat_map(|m| m.instrs_with_nops().map(|i| &i.kind)),
            );
        iter
    }

    pub fn instrs(&self) -> impl Iterator<Item = &InstrKind> {
        self.instrs_with_nops().filter(|i| !i.is_nop())
    }

    fn cur_function_mut(&mut self) -> &mut FunctionBuilder<'a> {
        match self.globals[self.cur_function].value {
            Some(GlobalValue::Function(ref mut m)) => m,
            _ => unreachable!(),
        }
    }

    fn cur_function(&self) -> &FunctionBuilder<'a> {
        match self.globals[self.cur_function].value {
            Some(GlobalValue::Function(ref m)) => m,
            _ => unreachable!(),
        }
    }

    fn get_local(&self, name: &'a str) -> IrId {
        self.cur_function().get_local(name).unwrap()
    }

    fn push_instr(&mut self, kind: InstrKind, ty: TypeId) {
        self.cur_function_mut().push_instr(kind, ty)
    }

    fn begin_scope(&mut self) {
        self.cur_function_mut().begin_scope()
    }

    fn end_scope(&mut self) -> FxHashMap<&'a str, IrId> {
        self.cur_function_mut().end_scope()
    }

    fn end_function(&mut self) -> FxHashMap<&'a str, IrId> {
        self.end_scope()
    }

    fn new_tmp(&mut self) -> IrId {
        self.cur_function_mut().new_tmp()
    }

    fn new_ptr(&mut self, name: &'a str, ty: TypeId) -> IrId {
        self.cur_function_mut().new_ptr(name, ty)
    }

    fn begin_block(&mut self) -> BlockId {
        self.cur_function_mut().begin_block()
    }

    fn patch_jmp_cond(&mut self, block: BlockId, on_true: BlockId, on_false: BlockId) {
        self.cur_function_mut()
            .patch_jmp_cond(block, on_true, on_false)
    }

    fn patch_jmp(&mut self, block: BlockId, target: BlockId) {
        self.cur_function_mut().patch_jmp(block, target)
    }

    fn cur_block(&self) -> BlockId {
        self.cur_function().cur_block()
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId) -> IrId {
        self.cur_function_mut().push_local(name, ty)
    }

    fn set_dom(&mut self, idom: BlockId, block: BlockId) {
        self.cur_function_mut().set_dom(idom, block)
    }

    fn set_pred(&mut self, block: BlockId, pred: BlockId) {
        self.cur_function_mut().set_pred(block, pred)
    }

    fn intern_string(&mut self, s: &str) -> Rc<str> {
        self.strings.intern(s)
    }

    fn new_vtable(
        strings: &mut StringSet,
        globals_map: &mut FxHashMap<(TypeId, &'a str), GlobalId>,
        globals: &mut IndexVec<GlobalId, Global<'a>>,
        ty: TypeId,
        class_name: &'a str,
        vtable: Box<[GlobalId]>,
    ) -> GlobalId {
        use std::collections::hash_map::Entry;

        match globals_map.entry((ty, "Table")) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let len = globals.len();
                assert!(len < u32::MAX as usize);
                let id = GlobalId(len as u32);
                entry.insert(id);
                let name = format!("{}.Table", class_name);
                let global = Global {
                    name:  strings.intern(&name),
                    value: Some(GlobalValue::Vtable(InstrKind::Vtable(id, vtable))),
                };
                globals.push(global);
                id
            }
        }
    }

    fn new_function(
        strings: &mut StringSet,
        globals_map: &mut FxHashMap<(TypeId, &'a str), GlobalId>,
        globals: &mut IndexVec<GlobalId, Global<'a>>,
        ty: TypeId,
        class_name: &'a str,
        name: &'a str,
    ) -> GlobalId {
        use std::collections::hash_map::Entry;

        match globals_map.entry((ty, name)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let len = globals.len();
                assert!(len < u32::MAX as usize);
                let id = GlobalId(len as u32);
                entry.insert(id);
                let name = format!("{}.{}", class_name, name);
                let global = Global {
                    name:  strings.intern(&name),
                    value: None,
                };
                globals.push(global);
                id
            }
        }
    }

    fn build_default(&mut self, dst: IrId, ty: TypeId) {
        match ty {
            TypeId::INT => {
                let kind = InstrKind::Store {
                    dst,
                    size: ty.size_of(),
                    src: Value::Int(0),
                };
                self.push_instr(kind, ty);
            }
            TypeId::BOOL => {
                let kind = InstrKind::Store {
                    dst,
                    size: ty.size_of(),
                    src: Value::Bool(false),
                };
                self.push_instr(kind, ty);
            }
            TypeId::STRING => {
                let s = self.intern_string("");
                let kind = InstrKind::Store {
                    dst,
                    size: ty.size_of(),
                    src: Value::Str(s),
                };
                self.push_instr(kind, ty);
            }
            _ => {
                let kind = InstrKind::Store {
                    dst,
                    size: ty.size_of(),
                    src: Value::Int(0),
                };
                self.push_instr(kind, ty);
            }
        }
    }

    pub fn build_class(&mut self, class: TypedClass<'a>) {
        let TypedClass {
            type_id,
            methods,
            attrs,
            ..
        } = class;

        self.cur_class = type_id;
        let mut class_attrs = self
            .env
            .get_class(type_id)
            .unwrap()
            .attrs()
            .iter()
            .map(|(k, ty)| (*k, *ty, 0))
            .collect::<Vec<_>>();

        class_attrs.sort_by_key(|(_, data, _)| data.pos);

        let mut cur_offset = 0;
        for (_, data, offset) in class_attrs.iter_mut() {
            *offset = data.ty.align_offset(cur_offset);
            cur_offset = *offset + data.ty.size_of();
        }

        self.class_sizes[type_id] = cur_offset;

        self.build_new(type_id, attrs, &class_attrs);

        for method in methods.into_vec() {
            self.build_function(method, &class_attrs);
        }
    }

    fn build_new(
        &mut self,
        ty: TypeId,
        typed_attrs: Box<[TypedAttribute<'a>]>,
        class_attrs: &[(&'a str, AttrData, usize)],
    ) -> FxHashMap<&'a str, IrId> {
        let parent = self.env.get_class(ty).unwrap().parent().unwrap();
        let parent_new = self
            .globals_map
            .get(&(parent, "new"))
            .map(|&id| IrId::Global(id))
            .unwrap();
        let mut function = FunctionBuilder::new(self.cur_class);
        let function_id = *self.globals_map.get(&(self.cur_class, "new")).unwrap();
        self.cur_function = function_id;
        let function_params = Box::new([(8, function.new_tmp())]);

        function.begin_scope();
        let kind = InstrKind::Function {
            id:     IrId::Global(function_id),
            ret:    ty.size_of(),
            params: function_params,
        };
        function.push_instr(kind, TypeId::SelfType);
        function.set_params(std::iter::empty());
        function.set_attrs(class_attrs);
        self.globals[function_id].value = Some(GlobalValue::Function(function));

        let tmp1 = self.new_tmp();
        let kind =
            InstrKind::AssignCall(tmp1, parent_new, Box::new([(8, Value::Id(IrId::Tmp(0)))]));
        self.push_instr(kind, ty);

        for TypedAttribute { id, init, ty, .. } in typed_attrs.into_vec() {
            let id = self.get_local(id);

            match init {
                Some(init) => {
                    let (init, init_ty) = self.build_expr(init);
                    let init = self.build_maybe_cast(ty, init, init_ty);
                    let kind = InstrKind::Store {
                        dst:  id,
                        size: ty.size_of(),
                        src:  Value::Id(init),
                    };
                    self.push_instr(kind, ty);
                }
                None => self.build_default(id, ty),
            }
        }

        let vtable = self.globals_map.get(&(ty, "Table")).unwrap();
        let vtable = Value::Id(IrId::Global(*vtable));
        let tmp2 = self.new_tmp();
        let kind = InstrKind::AssignInsert {
            dst: tmp2,
            src: tmp1,
            val: vtable,
            idx: 1,
        };
        self.push_instr(kind, ty);

        let kind = InstrKind::Return(Value::Id(tmp2));
        self.push_instr(kind, ty);

        self.end_function()
    }

    fn build_function(
        &mut self,
        typed_method: TypedMethod<'a>,
        class_attrs: &[(&'a str, AttrData, usize)],
    ) -> FxHashMap<&'a str, IrId> {
        let id = typed_method.id();

        let params = typed_method.params();

        let mut function = FunctionBuilder::new(self.cur_class);
        let function_id = *self.globals_map.get(&(self.cur_class, id)).unwrap();
        self.cur_function = function_id;
        let mut function_params = vec![(self.cur_class.size_of(), function.new_tmp())];
        function_params.extend(params.iter().map(|f| (f.ty.size_of(), function.new_tmp())));

        function.begin_scope();
        let kind = InstrKind::Function {
            id:     IrId::Global(function_id),
            ret:    typed_method.return_ty().size_of(),
            params: function_params.into_boxed_slice(),
        };
        function.push_instr(kind, TypeId::SelfType);
        function.set_params(params.iter().map(|f| (f.id, f.ty)));
        function.set_attrs(class_attrs);
        self.globals[function_id].value = Some(GlobalValue::Function(function));

        let ret_ty = typed_method.return_ty();
        let (body, ty) = self.build_expr(typed_method.take_body());
        let body = self.build_maybe_cast(ret_ty, body, ty);
        let kind = InstrKind::Return(Value::Id(body));
        self.push_instr(kind, ty);

        let ret = self.end_function();

        // println!();
        // for (_, global) in self.globals.iter() {
        //     println!("{:?}:", global.name);
        //     if let Some(GlobalValue::Vtable(InstrKind::Vtable(_, ref vtable))) = global.value {
        //         for &method in vtable {
        //             println!("  {:?}", self.globals[method].name);
        //         }
        //     }
        // }

        ret
    }

    fn build_maybe_cast(&mut self, ty: TypeId, expr: IrId, expr_ty: TypeId) -> IrId {
        match expr_ty {
            TypeId::INT | TypeId::BOOL | TypeId::STRING if ty != expr_ty => {
                let tmp = self.new_tmp();
                let function_id = self
                    .globals_map
                    .get(&(expr_ty, "Cast"))
                    .map(|&id| IrId::Global(id))
                    .unwrap();
                let kind = InstrKind::AssignCall(
                    tmp,
                    function_id,
                    Box::new([(expr_ty.size_of(), Value::Id(expr))]),
                );
                self.push_instr(kind, ty);
                tmp
            }
            _ => expr,
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
                let allocator = self
                    .globals_map
                    .get(&(TypeId::ALLOCATOR, "alloc"))
                    .map(|&id| IrId::Global(id))
                    .unwrap();
                let tmp1 = self.new_tmp();
                let size = self.class_sizes[ty] as i64;
                let kind = InstrKind::AssignCall(
                    tmp1,
                    allocator,
                    Box::new([(16, Value::Void), (8, Value::Int(size))]),
                );
                self.push_instr(kind, TypeId::INT);

                let tmp1 = Value::Id(tmp1);
                let tmp2 = self.new_tmp();
                let function_id = self
                    .globals_map
                    .get(&(ty, "new"))
                    .map(|&id| IrId::Global(id))
                    .unwrap();
                let kind = InstrKind::AssignCall(tmp2, function_id, Box::new([(8, tmp1)]));
                self.push_instr(kind, ty);
                (tmp2, ty)
            }
            TEK::Assign(id, expr) => {
                let (expr, expr_ty) = self.build_expr(*expr);
                let new_expr = self.build_maybe_cast(ty, expr, expr_ty);
                let id = self.get_local(id);
                let kind = InstrKind::Store {
                    dst:  id,
                    size: ty.size_of(),
                    src:  Value::Id(new_expr),
                };
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
                            let expr = self.build_maybe_cast(ty, expr, expr_ty);
                            let kind = InstrKind::Store {
                                dst:  id,
                                size: ty.size_of(),
                                src:  Value::Id(expr),
                            };
                            self.push_instr(kind, ty);
                        }
                        None => {
                            let id = self.push_local(id, ty);
                            self.build_default(id, ty);
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
        let cond_end_block = self.cur_block();
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
        self.set_pred(cond_block, self.cur_block());

        let end_block = self.begin_block();
        let tmp = self.new_tmp();
        let kind = InstrKind::Assign(tmp, Value::Void);
        self.push_instr(kind, TypeId::OBJECT);
        self.patch_jmp_cond(cond_end_block, body_block, end_block);
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
        let then = self.build_maybe_cast(ty, then, then_ty);
        let jmp_kind = InstrKind::Jmp(then_block);
        self.push_instr(jmp_kind, TypeId::SelfType);
        let then_end_block = self.cur_block();

        let else_block = self.begin_block();
        let (els, els_ty) = self.build_expr(els);
        let els = self.build_maybe_cast(ty, els, els_ty);

        let end_block = self.begin_block();
        let tmp = self.new_tmp();
        let kind = InstrKind::Phi(
            tmp,
            vec![(Value::Id(then), then_block), (Value::Id(els), else_block)],
        );
        self.push_instr(kind, ty);
        self.patch_jmp_cond(cond_block, then_block, else_block);
        self.patch_jmp(then_end_block, end_block);
        self.set_dom(cond_block, end_block);

        (tmp, ty)
    }

    fn build_self_dispatch(
        &mut self,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
    ) -> (IrId, TypeId) {
        let function_params = self
            .env
            .get_method(self.cur_class, method_name)
            .unwrap()
            .params()
            .to_vec();
        let mut args = self.build_args(args, function_params);

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
            let arg = self.build_maybe_cast(param, arg, arg_ty);
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

        let function_params = self
            .env
            .get_method(expr_ty, method_name)
            .unwrap()
            .params()
            .to_vec();

        let mut args = self.build_args(args, function_params);

        let (expr, _) = self.build_expr(expr);
        if expr_ty.needs_cast() {
            todo!()
        }
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
        let function_params = self
            .env
            .get_method(ty, method_name)
            .unwrap()
            .params()
            .to_vec();

        let mut args = self.build_args(args, function_params);

        let (expr, expr_ty) = self.build_expr(expr);
        if expr_ty.needs_cast() {
            todo!()
        }
        args[0] = (ty.size_of(), Value::Id(expr));

        let tmp = self.new_tmp();
        let funciton_id = self
            .globals_map
            .get(&(ty, method_name))
            .map(|&id| IrId::Global(id))
            .unwrap();
        let kind = InstrKind::AssignCall(tmp, funciton_id, args.into_boxed_slice());
        self.push_instr(kind, result_ty);

        (tmp, result_ty)
    }
}
