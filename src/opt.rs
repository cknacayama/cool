use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

use crate::{
    ast::{BinOp, UnOp},
    index_vec::{index_vec, IndexVec, Key},
    ir::{Id, Instr, InstrKind, Value},
    ir_builder::{Block, BlockId, Method},
    types::TypeId,
};

type Predecessors = IndexVec<BlockId, Vec<BlockId>>;
type ImmediateDominators = IndexVec<BlockId, BlockId>;
type DominatorTree = IndexVec<BlockId, Vec<BlockId>>;
type DominanceFrontiers = IndexVec<BlockId, Vec<BlockId>>;
type PhiPositions = IndexVec<BlockId, Vec<(Id, Vec<Id>, TypeId)>>;

impl<'a> Method<'a> {
    fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, mut f: F) {
        let block = &self.blocks[block];

        match block.instrs.back().unwrap().kind {
            InstrKind::Jmp(target) => {
                f(target);
            }
            InstrKind::JmpCond(_, on_true, on_false) => {
                f(on_true);
                f(on_false);
            }
            InstrKind::Return(_) => {}
            _ => unreachable!(),
        }
    }

    fn successors_inner_visit<F: FnMut(&mut Block<'a>)>(&mut self, block: BlockId, mut f: F) {
        let block = &self.blocks[block];

        match block.instrs.back().unwrap().kind {
            InstrKind::Jmp(target) => {
                f(self.blocks.get_mut(target).unwrap());
            }
            InstrKind::JmpCond(_, on_true, on_false) => {
                f(self.blocks.get_mut(on_true).unwrap());
                f(self.blocks.get_mut(on_false).unwrap());
            }
            InstrKind::Return(_) => {}
            _ => unreachable!(),
        }
    }

    fn predecessors(&self) -> Predecessors {
        let mut preds = index_vec![vec![]; self.blocks.len()];

        for bb in self.block_ids() {
            self.successors_visit(bb, |succ| preds[succ].push(bb));
        }

        preds
    }

    fn post_order(&self) -> Vec<BlockId> {
        let mut post_order = vec![];
        let mut visited = index_vec![false; self.blocks.len()];

        fn post_order_visit(
            fun: &Method<'_>,
            bb: BlockId,
            post_order: &mut Vec<BlockId>,
            visited: &mut IndexVec<BlockId, bool>,
        ) {
            fun.successors_visit(bb, |succ| {
                if !visited[succ] {
                    visited[succ] = true;
                    post_order_visit(fun, succ, post_order, visited);
                }
            });
            post_order.push(bb);
        }
        post_order_visit(self, BlockId::ENTRY, &mut post_order, &mut visited);

        post_order
    }

    fn post_order_indices(&self, post_order: &[BlockId]) -> IndexVec<BlockId, Option<u32>> {
        let mut post_idx = index_vec![None; self.blocks.len()];
        for (idx, &bb) in post_order.iter().enumerate() {
            post_idx[bb] = Some(idx as u32);
        }
        post_idx
    }

    fn idoms(&self, preds: &Predecessors) -> ImmediateDominators {
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
                    .copied()
                    .filter_map(|pred| post_idx.get(pred).and_then(|idx| *idx))
                {
                    if doms[pred].is_some() {
                        let mut y = pred;

                        while new_idom != y {
                            while new_idom < y {
                                new_idom = doms[new_idom].unwrap();
                            }
                            while y < new_idom {
                                y = doms[y].unwrap();
                            }
                        }
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
                    post_ord[idom_post_index.to_index()]
                }
                None => bb,
            })
            .collect()
    }

    fn dom_tree(&self, idoms: &ImmediateDominators) -> DominatorTree {
        let mut dom_tree = index_vec![vec![]; self.blocks.len()];

        for bb in self.block_ids().skip(1) {
            let idom = idoms[bb];
            dom_tree[idom].push(bb);
        }

        dom_tree
    }

    fn dom_frontiers(
        &self,
        preds: &Predecessors,
        idoms: &ImmediateDominators,
    ) -> DominanceFrontiers {
        let mut frontiers = index_vec![vec![]; self.blocks.len()];

        for bb in self.block_ids().filter(|b| preds[*b].len() >= 2) {
            let Some(idom) = idoms.get(bb).copied() else {
                continue;
            };

            for pred in preds[bb]
                .iter()
                .copied()
                .filter(|&pred| pred.is_entry() || pred != idoms[pred])
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

    fn all_stores(&self) -> IndexVec<Id, (&Block<'a>, Vec<BlockId>)> {
        let mut stores = IndexVec::with_capacity(self.locals().len());

        for local_block in self.locals().iter().map(|(_, (_, _, block))| *block) {
            stores.push((self.blocks.get(local_block).unwrap(), vec![]));
        }

        for (block_id, block) in self.blocks.iter() {
            for instr in block.instrs.iter() {
                if let InstrKind::Store(local, _, _) = instr.kind {
                    stores[local].1.push(block_id);
                }
            }
        }

        stores
    }

    fn all_uses(&self) -> IndexVec<Id, (BlockId, Vec<BlockId>)> {
        let mut uses = index_vec![(BlockId::ENTRY, vec![]); self.locals().len()];

        for (block_id, block) in self.blocks.iter() {
            for instr in block.instrs.iter() {
                let (decl, used) = instr.kind.uses();
                match decl {
                    Some(decl) if decl.is_local() => {
                        uses[decl].0 = block_id;
                    }
                    _ => {}
                }
                if let Some(used) = used {
                    for u in used.into_iter().filter(|u| u.is_local()) {
                        uses[u].1.push(block_id);
                    }
                }
            }
        }

        uses
    }

    fn liveness_check(&self, preds: &Predecessors) -> IndexVec<BlockId, Vec<Id>> {
        fn visit<'a>(
            method: &Method<'a>,
            block: BlockId,
            local: Id,
            decl_block: BlockId,
            preds: &Predecessors,
            live_out: &mut IndexVec<BlockId, Vec<Id>>,
            live_in: &mut IndexVec<BlockId, Vec<Id>>,
            visited: &mut IndexVec<BlockId, IndexVec<Id, bool>>,
        ) {
            if visited[block][local] {
                return;
            }
            live_in[block].push(local);
            visited[block][local] = true;
            for pred in preds[block].iter().copied() {
                live_out[pred].push(local);
                if pred != decl_block {
                    visit(
                        method, pred, local, decl_block, preds, live_out, live_in, visited,
                    );
                }
            }
        }

        let uses = self.all_uses();
        let mut live_out = index_vec![Vec::new(); self.blocks.len()];
        let mut live_in = index_vec![Vec::new(); self.blocks.len()];
        let mut visited = index_vec![index_vec![false; self.locals().len()]; self.blocks.len()];

        for (local, (decl_block, blocks)) in uses.iter() {
            for block in blocks {
                visit(
                    self,
                    *block,
                    local,
                    *decl_block,
                    preds,
                    &mut live_out,
                    &mut live_in,
                    &mut visited,
                );
            }
        }

        live_in
    }

    fn phi_positions(
        &self,
        preds: &Predecessors,
        dom_frontiers: &DominanceFrontiers,
    ) -> PhiPositions {
        let mut phi_positions: PhiPositions = index_vec![vec![]; self.blocks.len()];
        let liveness = self.liveness_check(preds);
        let stores = self.all_stores();

        for (local, mut stores) in stores.into_iter().map(|(l, (_, s))| (l, s)) {
            let mut visited = index_vec![false; self.blocks.len()];
            while let Some(block) = stores.pop() {
                for frontier in dom_frontiers[block]
                    .iter()
                    .filter(|frontier| liveness[**frontier].contains(&local))
                    .copied()
                {
                    let phis = &mut phi_positions[frontier];
                    if !phis.iter().any(|(l, _, _)| *l == local) {
                        let ty = self.locals()[local].1;
                        phis.push((local, vec![local; preds[frontier].len()], ty));
                        if !visited[frontier] {
                            visited[frontier] = true;
                            stores.push(frontier);
                        }
                    }
                }
            }
        }

        phi_positions
    }

    fn rename_locals(&mut self, preds: &Predecessors, dom_tree: &DominatorTree) {
        fn rename_block(
            method: &mut Method<'_>,
            cur_name: &mut Id,
            block: BlockId,
            renames: &mut HashMap<Id, Vec<Id>>,
            dom_tree: &DominatorTree,
            preds: &Predecessors,
        ) {
            for instr in method.blocks[block].instrs_mut() {
                instr.kind.rename(renames, cur_name);
            }

            method.successors_inner_visit(block, |s| {
                let j = preds[s.id()].iter().position(|&p| p == block).unwrap();
                for args in s.phis_args() {
                    let old_id = args[j];
                    args[j] = *renames[&old_id].last().unwrap();
                }
            });

            for child in dom_tree[block].iter().copied() {
                rename_block(method, cur_name, child, renames, dom_tree, preds);
            }
        }

        let mut renames = HashMap::new();
        let mut cur_tmp = Id::Renamed(0);
        rename_block(
            self,
            &mut cur_tmp,
            BlockId::ENTRY,
            &mut renames,
            dom_tree,
            preds,
        )
    }

    fn insert_phis(&mut self, phis: PhiPositions) {
        for (block, phis) in phis.into_iter() {
            let span = self.blocks[block].instrs.front().unwrap().span;
            for (local, args, ty) in phis {
                let kind = InstrKind::Phi(local, args);
                let instr = Instr::new(kind, span, ty);
                self.push_front(block, instr);
            }
        }
    }

    pub fn mem2reg(&mut self) {
        let preds = self.predecessors();
        let idoms = self.idoms(&preds);
        let dom_tree = self.dom_tree(&idoms);
        let dom_front = self.dom_frontiers(&preds, &idoms);
        let mut phis = self.phi_positions(&preds, &dom_front);
        self.insert_phis(phis);
        self.rename_locals(&preds, &dom_tree);
        self.set_labels();
    }

    pub fn dead_code_elimination(&mut self) {
        // contains the (use_count, decl_index, variables_used_in_decl)
        let mut counter: HashMap<Id, (usize, Option<&mut InstrKind>, Option<Vec<Id>>)> =
            HashMap::new();

        for instr in self.instrs_mut().map(|instr| &mut instr.kind) {
            let (decl, used) = instr.uses();
            if let Some(used) = used.as_ref() {
                for u in used {
                    let (used, _, _) = counter.entry(*u).or_insert((0, None, None));
                    *used += 1;
                }
            }
            if let Some(decl) = decl {
                let use_count = match counter.get(&decl) {
                    Some((use_count, _, _)) => *use_count,
                    None => 0,
                };
                counter.insert(decl, (use_count, Some(instr), used));
            }
        }

        while let Some(id) = counter.iter().find_map(
            |(id, (use_count, _, _))| {
                if *use_count == 0 {
                    Some(*id)
                } else {
                    None
                }
            },
        ) {
            let (_, instr, used) = counter.remove(&id).unwrap();
            let instr = instr.unwrap();
            *instr = InstrKind::Nop;
            if let Some(used) = used {
                for u in used {
                    let (used, _, _) = counter.get_mut(&u).unwrap();
                    *used -= 1;
                }
            }
        }
    }

    pub fn const_propagation(&mut self, default_string: Rc<str>) {
        let preds = self.predecessors();
        let idoms = self.idoms(&preds);
        let dom_tree = self.dom_tree(&idoms);

        let mut values = HashMap::new();
        let mut stack: Vec<BlockId> = vec![BlockId::ENTRY];

        while let Some(block) = stack.pop() {
            for instr in self.blocks[block].instrs_mut().map(|instr| &mut instr.kind) {
                match instr {
                    InstrKind::AssignInt(id, val) => {
                        values.insert(*id, Value::Int(*val));
                    }
                    InstrKind::AssignBool(id, val) => {
                        values.insert(*id, Value::Bool(*val));
                    }
                    InstrKind::AssignStr(id, val) => {
                        values.insert(*id, Value::Str(val.clone()));
                    }
                    InstrKind::AssignUn(op, id, arg) => {
                        let op = *op;
                        let id = *id;
                        Self::replace_id(&values, arg);
                        let arg = *arg;
                        Self::const_fold_un(&mut values, instr, op, id, arg);
                    }
                    InstrKind::AssignBin(op, id, lhs, rhs) => {
                        let op = *op;
                        let id = *id;
                        Self::replace_id(&values, lhs);
                        Self::replace_id(&values, rhs);
                        let lhs = *lhs;
                        let rhs = *rhs;
                        Self::const_fold_bin(&mut values, instr, op, id, lhs, rhs);
                    }
                    InstrKind::Assign(id1, id2) => match values.get(id2) {
                        Some(val) => {
                            let val = val.clone();
                            let id1 = *id1;
                            Self::const_fold_assign(&mut values, instr, id1, val);
                        }
                        None => {
                            values.insert(*id1, Value::Id(*id2));
                        }
                    },
                    InstrKind::AssignDefault(id, ty) => {
                        let id = *id;
                        let ty = *ty;
                        Self::const_fold_default(
                            &mut values,
                            default_string.clone(),
                            instr,
                            id,
                            ty,
                        );
                    }
                    InstrKind::JmpCond(id, on_true, on_false) => {
                        Self::replace_id(&values, id);
                        let id = *id;
                        let on_true = *on_true;
                        let on_false = *on_false;
                        Self::const_fold_jmp_cond(&mut values, instr, id, on_true, on_false);
                    }
                    InstrKind::AssignDispatch(_, id2, _, args) => {
                        for arg in args.iter_mut().map(|(_, id)| id) {
                            Self::replace_id(&values, arg);
                        }
                        Self::replace_id(&values, id2);
                    }
                    InstrKind::AssignStaticDispatch(_, id2, _, _, args) => {
                        for arg in args.iter_mut().map(|(_, id)| id) {
                            Self::replace_id(&values, arg);
                        }
                        Self::replace_id(&values, id2);
                    }
                    InstrKind::Phi(_, vals) => {
                        for id in vals.iter_mut() {
                            Self::replace_id(&values, id);
                        }
                    }
                    InstrKind::Return(id) => {
                        Self::replace_id(&values, id);
                    }
                    InstrKind::Store(_, _, id2) => {
                        Self::replace_id(&values, id2);
                    }
                    _ => {}
                }
            }
            for child in dom_tree[block].iter().copied() {
                stack.push(child);
            }
        }
    }

    fn replace_id(values: &HashMap<Id, Value>, id: &mut Id) {
        if let Some(Value::Id(val)) = values.get(id) {
            *id = *val;
        }
    }

    fn const_fold_jmp_cond(
        values: &mut HashMap<Id, Value>,
        instr: &mut InstrKind<'a>,
        id: Id,
        on_true: BlockId,
        on_false: BlockId,
    ) {
        if let Some(Value::Bool(val)) = values.get(&id) {
            if *val {
                *instr = InstrKind::Jmp(on_true);
            } else {
                *instr = InstrKind::Jmp(on_false);
            }
        }
    }

    fn const_fold_default(
        values: &mut HashMap<Id, Value>,
        default_string: Rc<str>,
        instr: &mut InstrKind<'a>,
        id: Id,
        ty: TypeId,
    ) {
        match ty {
            TypeId::INT => {
                values.insert(id, Value::Int(0));
                *instr = InstrKind::AssignInt(id, 0);
            }
            TypeId::BOOL => {
                values.insert(id, Value::Bool(false));
                *instr = InstrKind::AssignBool(id, false);
            }
            TypeId::STRING => {
                values.insert(id, Value::Str(default_string.clone()));
                *instr = InstrKind::AssignStr(id, default_string);
            }
            TypeId::OBJECT => {
                values.insert(id, Value::Void);
                *instr = InstrKind::AssignDefault(id, TypeId::OBJECT);
            }
            _ => {}
        }
    }

    fn const_fold_assign(
        values: &mut HashMap<Id, Value>,
        instr: &mut InstrKind<'a>,
        id: Id,
        val: Value,
    ) {
        values.insert(id, val.clone());
        match val {
            Value::Int(val) => *instr = InstrKind::AssignInt(id, val),
            Value::Bool(val) => *instr = InstrKind::AssignBool(id, val),
            Value::Str(val) => *instr = InstrKind::AssignStr(id, val.to_owned().into()),
            Value::Void => *instr = InstrKind::AssignDefault(id, TypeId::OBJECT),
            Value::Id(val) => *instr = InstrKind::Assign(id, val),
        }
    }

    fn const_fold_un(
        values: &mut HashMap<Id, Value>,
        instr: &mut InstrKind<'a>,
        op: UnOp,
        id: Id,
        arg: Id,
    ) {
        match op {
            UnOp::Complement => {
                if let Some(Value::Int(arg)) = values.get(&arg) {
                    let val = !arg;
                    values.insert(id, Value::Int(val));
                    *instr = InstrKind::AssignInt(id, val);
                }
            }
            UnOp::Not => {
                if let Some(Value::Bool(arg)) = values.get(&arg) {
                    let val = !arg;
                    values.insert(id, Value::Bool(val));
                    *instr = InstrKind::AssignBool(id, val);
                }
            }
            UnOp::IsVoid => match values.get(&arg) {
                Some(Value::Void) => {
                    values.insert(id, Value::Bool(true));
                    *instr = InstrKind::AssignBool(id, true);
                }
                Some(Value::Int(_)) | Some(Value::Str(_)) | Some(Value::Bool(_)) => {
                    values.insert(id, Value::Bool(false));
                    *instr = InstrKind::AssignBool(id, false);
                }
                _ => {}
            },
        }
    }

    fn const_fold_bin(
        values: &mut HashMap<Id, Value>,
        instr: &mut InstrKind<'a>,
        op: BinOp,
        id: Id,
        lhs: Id,
        rhs: Id,
    ) {
        if let (Some(Value::Int(lhs)), Some(Value::Int(rhs))) = (values.get(&lhs), values.get(&rhs))
        {
            match op {
                BinOp::Add => {
                    let val = lhs + rhs;
                    values.insert(id, Value::Int(val));
                    *instr = InstrKind::AssignInt(id, val);
                }
                BinOp::Sub => {
                    let val = lhs - rhs;
                    values.insert(id, Value::Int(val));
                    *instr = InstrKind::AssignInt(id, val);
                }
                BinOp::Mul => {
                    let val = lhs * rhs;
                    values.insert(id, Value::Int(val));
                    *instr = InstrKind::AssignInt(id, val);
                }
                BinOp::Div => {
                    let val = lhs / rhs;
                    values.insert(id, Value::Int(val));
                    *instr = InstrKind::AssignInt(id, val);
                }
                BinOp::Lt => {
                    let val = lhs < rhs;
                    values.insert(id, Value::Bool(val));
                    *instr = InstrKind::AssignBool(id, val);
                }
                BinOp::Le => {
                    let val = lhs <= rhs;
                    values.insert(id, Value::Bool(val));
                    *instr = InstrKind::AssignBool(id, val);
                }
                BinOp::Eq => {
                    let val = lhs == rhs;
                    values.insert(id, Value::Bool(val));
                    *instr = InstrKind::AssignBool(id, val);
                }
            }
        }
    }
}
