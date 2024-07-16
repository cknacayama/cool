use std::rc::Rc;

use crate::{
    ast::{
        BinOp, TypedAttribute, TypedCaseArm, TypedClass, TypedExpr, TypedExprKind, TypedFormal,
        TypedMethod, UnOp,
    },
    fxhash::FxHashMap,
    index_vec::{index_vec, Idx, IndexVec},
    types::{AttrData, ClassEnv, TypeId},
};

use super::{
    block::{Block, BlockId},
    types::Type,
    GlobalId, Instr, IrId, LocalId, Value,
};

pub type StringMap = FxHashMap<Rc<str>, GlobalId>;

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
        (0..self.locals.len()).map(LocalId::new)
    }

    fn instrs_with_nops(&self) -> impl Iterator<Item = &Instr> {
        self.blocks.inner().iter().flat_map(|b| b.instrs.iter())
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr> {
        self.blocks
            .inner()
            .iter()
            .flat_map(|b| b.instrs.iter().filter(|i| !i.is_nop()))
    }

    pub fn instrs_mut(&mut self) -> impl Iterator<Item = &mut Instr> {
        self.blocks
            .inner_mut()
            .iter_mut()
            .flat_map(|b| b.instrs.iter_mut().filter(|i| !i.is_nop()))
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
        self.blocks.last().unwrap().instrs.last().unwrap()
    }

    fn begin_block(&mut self) -> BlockId {
        assert!(self.blocks.len() < u32::MAX as usize);
        assert!(self.cur_locals.len() < u32::MAX as usize);
        let id = BlockId(self.blocks.len() as u32);
        let mut block = Block::new(id);
        let inserted_jmp = !self.last_instr().is_block_end();
        if inserted_jmp {
            self.push_instr(Instr::Jmp(id));
        }
        let cur_block = self.cur_block();
        block.push(Instr::Label(id));
        self.blocks.push(block);
        self.idoms.push(BlockId::ENTRY);
        self.dom_tree.push(Vec::new());
        if inserted_jmp {
            self.set_pred(id, cur_block);
        }
        id
    }

    fn patch_jmp_cond(&mut self, block: BlockId, on_true: BlockId, on_false: BlockId) {
        let instr = self.blocks[block].instrs.last_mut().unwrap();
        match instr {
            Instr::JmpCond { src, .. } => {
                let src = *src;
                *instr = Instr::JmpCond {
                    src,
                    on_true,
                    on_false,
                };
            }
            _ => unreachable!(),
        }

        self.set_pred(on_true, block);
        self.set_pred(on_false, block);
    }

    fn patch_jmp(&mut self, block: BlockId, target: BlockId) {
        let instr = self.blocks[block].instrs.last_mut().unwrap();
        match instr {
            Instr::Jmp(_) => {
                *instr = Instr::Jmp(target);
            }
            _ => unreachable!(),
        }
        self.set_pred(target, block);
    }

    fn patch_switch<I>(&mut self, block: BlockId, default: BlockId, cases: I)
    where
        I: Iterator<Item = BlockId> + Clone,
    {
        let instr = self.blocks[block].instrs.last_mut().unwrap();
        match instr {
            Instr::Switch { src, .. } => {
                let src = *src;
                let cases = cases
                    .clone()
                    .enumerate()
                    .map(|(i, case)| (i as i64, case))
                    .collect();
                *instr = Instr::Switch {
                    src,
                    cases,
                    default,
                };
            }
            _ => unreachable!(),
        }

        for case in std::iter::once(default).chain(cases) {
            self.set_pred(case, block);
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

    fn push_instr(&mut self, instr: Instr) {
        self.blocks.last_mut().unwrap().push(instr)
    }

    fn set_attrs(&mut self, attrs: &[(&'a str, AttrData, usize)], local_self: Option<IrId>) {
        if attrs.is_empty() {
            return;
        }

        let local_self = local_self.unwrap_or(IrId::LOCAL_SELF);

        let tmp1 = self.new_tmp();
        let kind = Instr::AssignLoad {
            dst: tmp1,
            ty:  Type::Object,
            src: local_self,
        };
        self.push_instr(kind);
        let tmp = self.new_tmp();
        let kind = Instr::AssignExtract {
            dst:    tmp,
            src_ty: Type::Object,
            src:    tmp1,
            ty:     Type::Ptr,
            offset: 0,
        };
        self.push_instr(kind);

        for (name, data, offset) in attrs {
            if self.get_local(name).is_some() {
                continue;
            }
            let ptr = self.new_ptr(name, data.ty);
            let kind = Instr::AssignGep {
                dst:    ptr,
                src:    tmp,
                offset: *offset,
            };
            self.push_instr(kind);
        }
    }

    fn set_self(&mut self) {
        let ty = if self.is_in_new {
            Type::Ptr
        } else {
            Type::Object
        };
        let dst = self.new_local("self", self.class);
        let kind = Instr::Local(ty, dst);
        self.push_instr(kind);

        let kind = Instr::Store {
            dst,
            ty,
            src: Value::Id(IrId::Tmp(0)),
        };
        self.push_instr(kind);
    }

    fn set_params(&mut self, locals: impl Iterator<Item = (&'a str, TypeId)>) {
        self.set_self();

        for (pos, (name, ty)) in locals.enumerate() {
            let dst = self.new_local(name, ty);
            let ty = ty.into();
            let kind = Instr::Local(ty, dst);
            self.push_instr(kind);

            let tmp = IrId::Tmp(pos as u32 + 1);
            let kind = Instr::Store {
                dst,
                ty,
                src: Value::Id(tmp),
            };
            self.push_instr(kind);
        }
    }

    fn new_local(&mut self, name: &'a str, ty: TypeId) -> IrId {
        assert!(self.locals.len() < u32::MAX as usize);
        let local = self.locals.len();
        let id = IrId::Local(LocalId::new(local));
        self.cur_scope_mut().insert(name, id);
        self.locals.push((ty, self.cur_block()));
        id
    }

    fn new_ptr(&mut self, name: &'a str, ty: TypeId) -> IrId {
        assert!(self.locals.len() < u32::MAX as usize);
        let local = self.locals.len();
        let id = IrId::Ptr(LocalId::new(local));
        self.cur_scope_mut().insert(name, id);
        self.locals.push((ty, self.cur_block()));
        id
    }

    fn push_local(&mut self, name: &'a str, ty: TypeId) -> IrId {
        let id = self.new_local(name, ty);
        let kind = Instr::Local(ty.into(), id);
        self.push_instr(kind);
        id
    }

    pub fn block_ids(&self) -> impl Iterator<Item = BlockId> {
        (0..self.blocks.len() as u32).map(BlockId)
    }
}

#[derive(Debug)]
pub enum GlobalValue<'a> {
    Function(FunctionBuilder<'a>),
    Vtable(Instr),
    String(Rc<str>),
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
    strings:            StringMap,
    class_sizes:        IndexVec<TypeId, usize>,
}

impl<'a> IrBuilder<'a> {
    pub fn new(env: ClassEnv<'a>) -> Self {
        let globals_map = FxHashMap::default();
        let globals = IndexVec::new();
        let strings = FxHashMap::default();

        let mut class_sizes = index_vec![0; env.classes().len()];
        class_sizes[TypeId::INT] = 8;
        class_sizes[TypeId::BOOL] = 1;
        class_sizes[TypeId::STRING] = 16;

        let mut new = Self {
            cur_class: TypeId::SelfType,
            cur_function: GlobalId(0),
            env: ClassEnv::default(),
            globals_map,
            globals,
            strings,
            class_sizes,
        };

        new.intern_string("");

        for (class, data) in env.classes() {
            let class_name = data.id();
            let vtable = match data.parent() {
                Some(parent) => {
                    let parent_vtable = new.globals_map[&(parent, "Table")];
                    vec![Some(parent_vtable)]
                }
                None => vec![None],
            };
            let vtable = data
                .vtable()
                .iter()
                .skip(1) // parent vtable
                .map(|(ty, function)| {
                    let ty_name = env.get_class_name(*ty);
                    new.new_function(*ty, ty_name, function)
                })
                .fold(vtable, |mut vtable, id| {
                    vtable.push(Some(id));
                    vtable
                })
                .into_boxed_slice();
            new.new_vtable(class, class_name, vtable);
        }

        new.env = env;

        new
    }

    pub fn strings(&self) -> &StringMap {
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

    fn instrs_with_nops(&self) -> impl Iterator<Item = &Instr> {
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
            .chain(self.functions().flat_map(|m| m.instrs_with_nops()));
        iter
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr> {
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

    fn push_instr(&mut self, kind: Instr) {
        self.cur_function_mut().push_instr(kind)
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

    fn patch_switch<I>(&mut self, block: BlockId, default: BlockId, cases: I)
    where
        I: Iterator<Item = BlockId> + Clone,
    {
        self.cur_function_mut().patch_switch(block, default, cases)
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

    fn get_global_id(&mut self, ty: TypeId, name: &'a str) -> Option<GlobalId> {
        self.globals_map.get(&(ty, name)).copied()
    }

    fn intern_string(&mut self, s: &str) -> (Rc<str>, GlobalId) {
        match self.strings.get_key_value(s) {
            Some((s, id)) => (s.clone(), *id),
            None => {
                let len = self.globals.len();
                assert!(len < u32::MAX as usize);
                let id = GlobalId(len as u32);
                let s: Rc<str> = s.into();
                self.strings.insert(s.clone(), id);
                let string_id = format!("str{}", id.0);
                let global = Global {
                    name:  string_id.into(),
                    value: Some(GlobalValue::String(s.clone())),
                };
                self.globals.push(global);
                (s, id)
            }
        }
    }

    fn new_vtable(
        &mut self,
        ty: TypeId,
        class_name: &'a str,
        vtable: Box<[Option<GlobalId>]>,
    ) -> GlobalId {
        use std::collections::hash_map::Entry;

        let name = format!("{}.Table", class_name);
        let (name, _) = self.intern_string(&name);

        match self.globals_map.entry((ty, "Table")) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let len = self.globals.len();
                assert!(len < u32::MAX as usize);
                let id = GlobalId(len as u32);
                entry.insert(id);
                let global = Global {
                    name,
                    value: Some(GlobalValue::Vtable(Instr::Vtable(id, vtable))),
                };
                self.globals.push(global);
                id
            }
        }
    }

    fn new_function(&mut self, ty: TypeId, class_name: &'a str, fn_name: &'a str) -> GlobalId {
        use std::collections::hash_map::Entry;

        let name = format!("{}.{}", class_name, fn_name);
        let (name, _) = self.intern_string(&name);

        match self.globals_map.entry((ty, fn_name)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let len = self.globals.len();
                assert!(len < u32::MAX as usize);
                let id = GlobalId(len as u32);
                entry.insert(id);
                let global = Global { name, value: None };
                self.globals.push(global);
                id
            }
        }
    }

    fn build_string(&mut self, id: GlobalId) -> IrId {
        let tmp = self.new_tmp();
        let kind = Instr::AssignLoad {
            dst: tmp,
            ty:  Type::String,
            src: IrId::Global(id),
        };
        self.push_instr(kind);
        tmp
    }

    fn build_default(&mut self, dst: IrId, ty: TypeId) {
        match ty {
            TypeId::INT => {
                let kind = Instr::Store {
                    dst,
                    ty: Type::I64,
                    src: Value::Int(0),
                };
                self.push_instr(kind);
            }
            TypeId::BOOL => {
                let kind = Instr::Store {
                    dst,
                    ty: Type::I1,
                    src: Value::Bool(false),
                };
                self.push_instr(kind);
            }
            TypeId::STRING => {
                let (_, s) = self.intern_string("");
                let s = self.build_string(s);
                let kind = Instr::Store {
                    dst,
                    ty: Type::String,
                    src: Value::Id(s),
                };
                self.push_instr(kind);
            }
            _ => {
                let kind = Instr::Store {
                    dst,
                    ty: Type::Object,
                    src: Value::Void,
                };
                self.push_instr(kind);
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
        let parent = self.env.get_class(ty).parent().unwrap();
        let parent_new = self.get_global_id(parent, "new").map(IrId::Global).unwrap();
        let mut function = FunctionBuilder::new(self.cur_class);
        let function_id = self.get_global_id(self.cur_class, "new").unwrap();
        self.cur_function = function_id;
        let function_params = Box::new([(Type::Ptr, function.new_tmp())]);

        function.begin_scope();
        let ret = ty.into();
        let kind = Instr::Function {
            id: function_id,
            ret,
            params: function_params,
        };
        function.push_instr(kind);
        function.set_params(std::iter::empty());
        self.globals[function_id].value = Some(GlobalValue::Function(function));

        let tmp1 = self.new_tmp();
        let kind = Instr::AssignCall(
            tmp1,
            ret,
            parent_new,
            Box::new([(Type::Ptr, Value::Id(IrId::Tmp(0)))]),
        );
        self.push_instr(kind);
        self.cur_function_mut().set_attrs(class_attrs, Some(tmp1));

        for TypedAttribute { id, init, ty, .. } in typed_attrs.into_vec() {
            let id = self.get_local(id);

            match init {
                Some(init) => {
                    let (init, init_ty) = self.build_expr(init);
                    let src = self.build_maybe_cast(ty, init, init_ty);
                    let kind = Instr::Store {
                        dst: id,
                        ty: ty.into(),
                        src,
                    };
                    self.push_instr(kind);
                }
                None => self.build_default(id, ty),
            }
        }

        let vtable = self.get_global_id(ty, "Table").unwrap();
        let vtable = Value::Id(IrId::Global(vtable));
        let tmp2 = self.new_tmp();
        let kind = Instr::AssignInsert {
            dst: tmp2,
            ty:  ret,
            src: tmp1,
            val: vtable,
            idx: 1,
        };
        self.push_instr(kind);

        let kind = Instr::Return(Value::Id(tmp2));
        self.push_instr(kind);

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
        let function_id = self.get_global_id(self.cur_class, id).unwrap();
        self.cur_function = function_id;
        let mut function_params = vec![(self.cur_class.into(), function.new_tmp())];
        function_params.extend(params.iter().map(|f| (f.ty.into(), function.new_tmp())));

        function.begin_scope();
        let instr = Instr::Function {
            id:     function_id,
            ret:    typed_method.return_ty().into(),
            params: function_params.into_boxed_slice(),
        };
        function.push_instr(instr);
        function.set_params(params.iter().map(|f| (f.id, f.ty)));
        function.set_attrs(class_attrs, None);
        self.globals[function_id].value = Some(GlobalValue::Function(function));

        let ret_ty = typed_method.return_ty();
        let (body, ty) = self.build_expr(typed_method.take_body());
        let body = self.build_maybe_cast(ret_ty, body, ty);
        let kind = Instr::Return(body);
        self.push_instr(kind);

        let ret = self.end_function();

        // println!();
        // for (_, global) in self.globals.iter() {
        //     println!("{:?}:", global.name);
        //     if let Some(GlobalValue::Vtable(Instr::Vtable(_, ref vtable))) = global.value {
        //         for &method in vtable {
        //             println!("  {:?}", self.globals[method].name);
        //         }
        //     }
        // }

        ret
    }

    fn build_cast(&mut self, expr: Value, expr_ty: TypeId) -> IrId {
        match expr_ty {
            TypeId::INT | TypeId::BOOL | TypeId::STRING => {
                let tmp = self.new_tmp();
                let function_id = self
                    .get_global_id(expr_ty, "Cast")
                    .map(IrId::Global)
                    .unwrap();
                let kind = Instr::AssignCall(
                    tmp,
                    Type::Object,
                    function_id,
                    Box::new([(expr_ty.into(), expr)]),
                );
                self.push_instr(kind);
                tmp
            }
            _ => match expr {
                Value::Id(id) => id,
                _ => unreachable!(),
            },
        }
    }

    fn build_maybe_cast(&mut self, ty: TypeId, expr: Value, expr_ty: TypeId) -> Value {
        match expr_ty {
            TypeId::INT | TypeId::BOOL | TypeId::STRING if ty != expr_ty => {
                let tmp = self.new_tmp();
                let function_id = self
                    .get_global_id(expr_ty, "Cast")
                    .map(IrId::Global)
                    .unwrap();
                let kind = Instr::AssignCall(
                    tmp,
                    Type::Object,
                    function_id,
                    Box::new([(expr_ty.into(), expr)]),
                );
                self.push_instr(kind);
                Value::Id(tmp)
            }
            TypeId::INT | TypeId::BOOL | TypeId::STRING => expr,
            _ => match ty {
                TypeId::INT => {
                    let tmp = self.new_tmp();
                    let function_id = self
                        .get_global_id(TypeId::OBJECT, "To_Int")
                        .map(IrId::Global)
                        .unwrap();
                    let kind = Instr::AssignCall(
                        tmp,
                        Type::I64,
                        function_id,
                        Box::new([(Type::Object, expr)]),
                    );
                    self.push_instr(kind);
                    Value::Id(tmp)
                }
                TypeId::BOOL => {
                    let tmp = self.new_tmp();
                    let function_id = self
                        .get_global_id(TypeId::OBJECT, "To_Bool")
                        .map(IrId::Global)
                        .unwrap();
                    let kind = Instr::AssignCall(
                        tmp,
                        Type::I1,
                        function_id,
                        Box::new([(Type::Object, expr)]),
                    );
                    self.push_instr(kind);
                    Value::Id(tmp)
                }
                TypeId::STRING => {
                    let tmp = self.new_tmp();
                    let function_id = self
                        .get_global_id(TypeId::OBJECT, "To_String")
                        .map(IrId::Global)
                        .unwrap();
                    let kind = Instr::AssignCall(
                        tmp,
                        Type::String,
                        function_id,
                        Box::new([(Type::Object, expr)]),
                    );
                    self.push_instr(kind);
                    Value::Id(tmp)
                }
                _ => expr,
            },
        }
    }

    fn build_expr(&mut self, expr: TypedExpr<'a>) -> (Value, TypeId) {
        let TypedExpr { kind, ty, .. } = expr;
        use TypedExprKind as TEK;
        match kind {
            TEK::IntLit(i) => (Value::Int(i), ty),
            TEK::BoolLit(b) => (Value::Bool(b), ty),
            TEK::StringLit(s) => {
                let (_, s) = self.intern_string(&s);
                let tmp = self.build_string(s);
                (Value::Id(tmp), ty)
            }
            TEK::Id(id) => {
                let dst = self.new_tmp();
                let src = self.get_local(id);
                let kind = Instr::AssignLoad {
                    dst,
                    ty: ty.into(),
                    src,
                };
                self.push_instr(kind);
                (Value::Id(dst), ty)
            }
            TEK::Unary(op, expr) => {
                let (src, src_ty) = self.build_expr(*expr);
                let src_ty = src_ty.into();
                match (op, src) {
                    (UnOp::IsVoid, Value::Id(src)) if src_ty == Type::Object => {
                        let tmp = self.new_tmp();
                        let kind = Instr::AssignExtract {
                            dst: tmp,
                            src_ty,
                            src,
                            ty: Type::Ptr,
                            offset: 0,
                        };
                        self.push_instr(kind);
                        let dst = self.new_tmp();
                        let kind = Instr::AssignUn {
                            dst,
                            op,
                            ty: Type::Ptr,
                            src: Value::Id(tmp),
                        };
                        self.push_instr(kind);
                        (Value::Id(dst), ty)
                    }
                    (UnOp::IsVoid, Value::Void) => (Value::Bool(true), ty),
                    (UnOp::IsVoid, _) => (Value::Bool(false), ty),
                    _ => {
                        let dst = self.new_tmp();
                        let kind = Instr::AssignUn {
                            dst,
                            op,
                            src,
                            ty: src_ty,
                        };
                        self.push_instr(kind);
                        (Value::Id(dst), ty)
                    }
                }
            }
            TEK::Binary(op, lhs, rhs) => {
                let (lhs, _) = self.build_expr(*lhs);
                let (rhs, _) = self.build_expr(*rhs);
                let dst = self.new_tmp();
                let kind = Instr::AssignBin { dst, op, lhs, rhs };
                self.push_instr(kind);
                (Value::Id(dst), ty)
            }
            TEK::New(ty) => {
                let allocator = self
                    .get_global_id(TypeId::ALLOCATOR, "alloc")
                    .map(IrId::Global)
                    .unwrap();
                let tmp1 = self.new_tmp();
                let size = self.class_sizes[ty] as i64;
                let kind = Instr::AssignCall(
                    tmp1,
                    Type::Ptr,
                    allocator,
                    Box::new([(Type::Object, Value::Void), (Type::I64, Value::Int(size))]),
                );
                self.push_instr(kind);

                let tmp1 = Value::Id(tmp1);
                let tmp2 = self.new_tmp();
                let function_id = self.get_global_id(ty, "new").map(IrId::Global).unwrap();
                let kind =
                    Instr::AssignCall(tmp2, ty.into(), function_id, Box::new([(Type::Ptr, tmp1)]));
                self.push_instr(kind);
                (Value::Id(tmp2), ty)
            }
            TEK::Assign(id, expr) => {
                let (expr, expr_ty) = self.build_expr(*expr);
                let src = self.build_maybe_cast(ty, expr, expr_ty);
                let id = self.get_local(id);
                let kind = Instr::Store {
                    dst: id,
                    ty: ty.into(),
                    src,
                };
                self.push_instr(kind);
                (src, ty)
            }
            TEK::SelfId => {
                let tmp = self.new_tmp();
                let kind = Instr::AssignLoad {
                    dst: tmp,
                    ty:  self.cur_class.into(),
                    src: IrId::LOCAL_SELF,
                };
                self.push_instr(kind);
                (Value::Id(tmp), ty)
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
                            let src = self.build_maybe_cast(ty, expr, expr_ty);
                            let kind = Instr::Store {
                                dst: id,
                                ty: ty.into(),
                                src,
                            };
                            self.push_instr(kind);
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
                exprs.fold((Value::Void, TypeId::SelfType), |_, expr| {
                    self.build_expr(expr)
                })
            }
            TEK::If(cond, then, els) => self.build_if(*cond, *then, *els, ty),
            TEK::While(cond, body) => self.build_while(*cond, *body),
            TEK::Case(expr, cases) => self.build_case(*expr, cases, ty),
        }
    }

    fn build_case(
        &mut self,
        expr: TypedExpr<'a>,
        cases: Box<[TypedCaseArm<'a>]>,
        case_expr_ty: TypeId,
    ) -> (Value, TypeId) {
        let (expr, expr_ty) = self.build_expr(expr);
        let src = self.build_cast(expr, expr_ty);
        let expr_table = self.new_tmp();
        self.push_instr(Instr::AssignExtract {
            dst: expr_table,
            src_ty: Type::Object,
            src,
            ty: Type::Ptr,
            offset: 1,
        });
        let expr_table = Value::Id(expr_table);
        self.begin_scope();
        let compare_fn = self
            .get_global_id(TypeId::COMPARATOR, "compare")
            .map(IrId::Global)
            .unwrap();
        let min = self.push_local("Min", TypeId::INT);
        self.push_instr(Instr::Store {
            dst: min,
            ty:  Type::I64,
            src: Value::Int(i64::MAX),
        });
        let min_ty = self.push_local("MinType", TypeId::INT);
        self.push_instr(Instr::Store {
            dst: min_ty,
            ty:  Type::I64,
            src: Value::Int(0),
        });
        for (i, ty) in cases.iter().map(|c| c.ty).enumerate() {
            let table = self.get_global_id(ty, "Table").unwrap();
            let table = Value::Id(IrId::Global(table));
            let dst = self.new_tmp();
            self.push_instr(Instr::AssignCall(
                dst,
                Type::I64,
                compare_fn,
                Box::new([
                    (Type::Object, Value::Void),
                    (Type::Ptr, expr_table),
                    (Type::Ptr, table),
                ]),
            ));

            let is_neg = self.new_tmp();
            self.push_instr(Instr::AssignBin {
                dst: is_neg,
                op:  BinOp::Lt,
                lhs: Value::Id(dst),
                rhs: Value::Int(0),
            });

            let is_neg_block = self.cur_block();
            self.push_instr(Instr::JmpCond {
                src:      Value::Id(is_neg),
                on_true:  is_neg_block,
                on_false: is_neg_block,
            });

            let cond_block = self.begin_block();
            let min_val = self.new_tmp();
            self.push_instr(Instr::AssignLoad {
                dst: min_val,
                ty:  Type::I64,
                src: min,
            });

            let tmp = self.new_tmp();
            self.push_instr(Instr::AssignBin {
                dst: tmp,
                op:  BinOp::Lt,
                lhs: Value::Id(dst),
                rhs: Value::Id(min_val),
            });

            let min_jmp = Instr::JmpCond {
                src:      Value::Id(tmp),
                on_true:  cond_block,
                on_false: cond_block,
            };
            self.push_instr(min_jmp);

            let then_block = self.begin_block();
            self.push_instr(Instr::Store {
                dst: min,
                ty:  Type::I64,
                src: Value::Id(dst),
            });
            self.push_instr(Instr::Store {
                dst: min_ty,
                ty:  Type::I64,
                src: Value::Int(i as i64),
            });
            let end_block = self.begin_block();

            self.patch_jmp_cond(is_neg_block, end_block, cond_block);
            self.set_dom(is_neg_block, cond_block);
            self.set_dom(is_neg_block, end_block);

            self.patch_jmp_cond(cond_block, then_block, end_block);
            self.set_dom(cond_block, then_block);
        }

        let cur_block = self.cur_block();
        let def_val = self.build_default_value(case_expr_ty);

        let tmp = self.new_tmp();
        self.push_instr(Instr::AssignLoad {
            dst: tmp,
            ty:  Type::I64,
            src: min_ty,
        });
        self.push_instr(Instr::Switch {
            src:     Value::Id(tmp),
            default: cur_block,
            cases:   Vec::new(),
        });

        let mut arms = Vec::with_capacity(cases.len());
        for TypedCaseArm { id, ty, expr, .. } in cases {
            self.begin_scope();
            let block = self.begin_block();
            self.set_dom(cur_block, block);
            self.set_pred(cur_block, block);

            let src = self.build_maybe_cast(ty, Value::Id(src), TypeId::OBJECT);

            let id = self.push_local(id, ty);
            self.push_instr(Instr::Store {
                dst: id,
                ty: ty.into(),
                src,
            });

            let (body_val, body_ty) = self.build_expr(expr);
            let body = self.build_maybe_cast(case_expr_ty, body_val, body_ty);

            let cur_block = self.cur_block();
            arms.push((body, block, cur_block));
            self.push_instr(Instr::Jmp(cur_block));

            self.end_scope();
        }

        self.end_scope();
        let end_block = self.begin_block();
        self.set_dom(cur_block, end_block);
        self.patch_switch(
            cur_block,
            end_block,
            arms.iter().map(|(_, block, _)| *block),
        );
        for (_, _, block) in arms.iter() {
            self.patch_jmp(*block, end_block);
        }
        let arms = std::iter::once((def_val, cur_block))
            .chain(arms.into_iter().map(|(val, _, block)| (val, block)))
            .collect();
        let tmp = self.new_tmp();
        self.push_instr(Instr::AssignPhi(tmp, case_expr_ty.into(), arms));

        (Value::Id(tmp), case_expr_ty)
    }

    fn build_default_value(&mut self, ty: TypeId) -> Value {
        match ty {
            TypeId::INT => Value::Int(0),
            TypeId::BOOL => Value::Bool(false),
            TypeId::STRING => {
                let (_, s) = self.intern_string("");
                Value::Id(self.build_string(s))
            }
            _ => Value::Void,
        }
    }

    fn build_while(&mut self, cond: TypedExpr<'a>, body: TypedExpr<'a>) -> (Value, TypeId) {
        let cur_block = self.cur_block();
        let cond_block = self.begin_block();
        self.set_dom(cur_block, cond_block);

        let (cond, _) = self.build_expr(cond);
        let cond_end_block = self.cur_block();
        let jmp_0_kind = Instr::JmpCond {
            src:      cond,
            on_true:  cond_block,
            on_false: cond_block,
        };
        self.push_instr(jmp_0_kind);

        let body_block = self.begin_block();
        let _ = self.build_expr(body);

        let jmp_kind = Instr::Jmp(cond_block);
        self.push_instr(jmp_kind);
        self.set_pred(cond_block, self.cur_block());

        let end_block = self.begin_block();
        self.patch_jmp_cond(cond_end_block, body_block, end_block);
        self.set_dom(cond_end_block, end_block);
        self.set_dom(cond_end_block, end_block);
        (Value::Void, TypeId::OBJECT)
    }

    fn build_if(
        &mut self,
        cond: TypedExpr<'a>,
        then: TypedExpr<'a>,
        els: TypedExpr<'a>,
        ty: TypeId,
    ) -> (Value, TypeId) {
        let (cond, _) = self.build_expr(cond);

        let cond_block = self.cur_block();
        let jmp_0_kind = Instr::JmpCond {
            src:      cond,
            on_true:  cond_block,
            on_false: cond_block,
        };
        self.push_instr(jmp_0_kind);

        let then_block = self.begin_block();
        let (then, then_ty) = self.build_expr(then);
        let then = self.build_maybe_cast(ty, then, then_ty);
        let jmp_kind = Instr::Jmp(then_block);
        self.push_instr(jmp_kind);
        let then_end_block = self.cur_block();

        let else_block = self.begin_block();
        let (els, els_ty) = self.build_expr(els);
        let els = self.build_maybe_cast(ty, els, els_ty);

        let end_block = self.begin_block();
        let tmp = self.new_tmp();
        let kind = Instr::AssignPhi(tmp, ty.into(), vec![(then, then_block), (els, else_block)]);
        self.push_instr(kind);
        self.patch_jmp_cond(cond_block, then_block, else_block);
        self.patch_jmp(then_end_block, end_block);
        self.set_dom(cond_block, then_block);
        self.set_dom(cond_block, else_block);
        self.set_dom(cond_block, end_block);

        (Value::Id(tmp), ty)
    }

    fn build_self_dispatch(
        &mut self,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
    ) -> (Value, TypeId) {
        let function_params = self
            .env
            .get_method(self.cur_class, method_name)
            .unwrap()
            .params()
            .to_vec();
        let mut args = self.build_args(args, function_params);

        let tmp1 = self.new_tmp();
        let kind = Instr::AssignLoad {
            dst: tmp1,
            ty:  self.cur_class.into(),
            src: IrId::LOCAL_SELF,
        };
        self.push_instr(kind);
        args[0] = (self.cur_class.into(), Value::Id(tmp1));

        let ptr = self.new_ptr("VTable", TypeId::INT);
        let kind = Instr::AssignExtract {
            dst:    ptr,
            src_ty: Type::Object,
            src:    tmp1,
            ty:     Type::Ptr,
            offset: 1,
        };
        self.push_instr(kind);

        let offset = self
            .env
            .get_class(self.cur_class)
            .get_vtable_offset(method_name)
            .unwrap();

        let tmp3 = self.new_tmp();
        let kind = Instr::AssignGep {
            dst: tmp3,
            src: ptr,
            offset,
        };
        self.push_instr(kind);
        let tmp4 = self.new_tmp();
        let kind = Instr::AssignLoad {
            dst: tmp4,
            ty:  Type::Ptr,
            src: tmp3,
        };
        self.push_instr(kind);

        let tmp5 = self.new_tmp();
        let kind = Instr::AssignCall(tmp5, ty.into(), tmp4, args.into_boxed_slice());
        self.push_instr(kind);

        (Value::Id(tmp5), ty)
    }

    /// first argument is 'empty'
    fn build_args(
        &mut self,
        args: Box<[TypedExpr<'a>]>,
        params: Vec<TypeId>,
    ) -> Vec<(Type, Value)> {
        let mut arg_ids = vec![(Type::I64, Value::Int(0))];
        for (arg, param) in args.into_vec().into_iter().zip(params) {
            let (arg, arg_ty) = self.build_expr(arg);
            let arg = self.build_maybe_cast(param, arg, arg_ty);
            arg_ids.push((param.into(), arg));
        }
        arg_ids
    }

    fn build_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        ty: TypeId,
    ) -> (Value, TypeId) {
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
        args[0] = (expr_ty.into(), expr);

        let ptr = self.new_ptr("VTable", TypeId::INT);
        let src = match expr {
            Value::Id(src) => src,
            Value::Void => {
                let tmp = self.new_tmp();
                let kind = Instr::Assign {
                    dst: tmp,
                    ty:  Type::Object,
                    src: Value::Void,
                };
                self.push_instr(kind);
                tmp
            }
            _ => unreachable!(),
        };
        let kind = Instr::AssignExtract {
            dst: ptr,
            src_ty: Type::Object,
            src,
            ty: Type::Ptr,
            offset: 1,
        };
        self.push_instr(kind);

        let offset = self
            .env
            .get_class(ty)
            .get_vtable_offset(method_name)
            .unwrap();

        let tmp2 = self.new_tmp();
        let kind = Instr::AssignGep {
            dst: tmp2,
            src: ptr,
            offset,
        };
        self.push_instr(kind);

        let tmp3 = self.new_tmp();
        let kind = Instr::AssignLoad {
            dst: tmp3,
            ty:  Type::Ptr,
            src: tmp2,
        };
        self.push_instr(kind);

        let tmp4 = self.new_tmp();
        let kind = Instr::AssignCall(tmp4, ty.into(), tmp3, args.into_boxed_slice());
        self.push_instr(kind);

        (Value::Id(tmp4), ty)
    }

    fn build_static_dispatch(
        &mut self,
        expr: TypedExpr<'a>,
        ty: TypeId,
        method_name: &'a str,
        args: Box<[TypedExpr<'a>]>,
        result_ty: TypeId,
    ) -> (Value, TypeId) {
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
        args[0] = (ty.into(), expr);

        let tmp = self.new_tmp();
        let funciton_id = self
            .get_global_id(ty, method_name)
            .map(IrId::Global)
            .unwrap();
        let kind = Instr::AssignCall(tmp, result_ty.into(), funciton_id, args.into_boxed_slice());
        self.push_instr(kind);

        (Value::Id(tmp), result_ty)
    }
}
