use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use crate::{
    ast::{BinOp, UnOp},
    index_vec::{index_vec, IndexVec, Key},
    ir::{
        block::{Block, BlockId},
        builder::Method,
        Instr, InstrKind, IrId, LocalId, Value,
    },
    types::TypeId,
};

pub fn optimize(method: &mut Method<'_>, default_string: &Rc<str>) {
    let preds = method.predecessors();
    let idoms = method.idoms(&preds);
    let dom_tree = method.dom_tree(&idoms);

    let dom_frontiers = method.dom_frontiers(&preds, &idoms);
    let phis = method.phi_positions(&preds, &dom_frontiers);

    method.mem2reg(&preds, &dom_tree, phis);

    while method.const_propagation(default_string) | method.dead_code_elimination() {}

    method.set_labels();
}

type Predecessors = IndexVec<BlockId, Vec<BlockId>>;
type ImmediateDominators = IndexVec<BlockId, BlockId>;
type DominatorTree = IndexVec<BlockId, Vec<BlockId>>;
type DominanceFrontiers = IndexVec<BlockId, Vec<BlockId>>;
type PhiPositions = IndexVec<BlockId, Vec<(LocalId, Box<[(LocalId, BlockId)]>, TypeId)>>;

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

    fn reachability_blocks(&self) -> IndexVec<BlockId, bool> {
        let mut visited = index_vec![false; self.blocks.len()];
        let mut stack = vec![BlockId::ENTRY];

        while let Some(block) = stack.pop() {
            visited[block] = true;
            self.successors_visit(block, |succ| {
                if !visited[succ] {
                    stack.push(succ);
                }
            });
        }

        visited
    }

    fn remove_dead_blocks(&mut self) -> bool {
        let mut new_blocks = index_vec![None; self.blocks.len()];
        let mut reachable = self.reachability_blocks().into_iter().map(|(_, v)| v);

        let len = self.blocks.len();
        self.blocks.retain(|_| reachable.next().unwrap());

        let removed = len != self.blocks.len();

        if removed {
            for (id, block) in self.blocks.iter_mut() {
                new_blocks[block.id()] = Some(id);
                block.id = id;
            }

            self.rename_blocks(&new_blocks);
        }

        removed
    }

    fn remove_unecessary_phis(&mut self, undeclared_vars: &HashSet<IrId>) -> bool {
        let mut changed = false;
        for instr in self.instrs_mut() {
            if let InstrKind::Phi(ref id, ref mut vals) = instr.kind {
                vals.retain(|(val, _)| match val {
                    Value::Id(id) if undeclared_vars.contains(id) => {
                        changed = true;
                        false
                    }
                    _ => true,
                });
                if vals.is_empty() {
                    instr.kind.replace_with_nop();
                } else if vals.len() == 1 {
                    instr.kind = match &vals[0].0 {
                        Value::Void => InstrKind::AssignDefault(*id, TypeId::OBJECT),
                        val => InstrKind::Assign(*id, val.clone()),
                    };
                }
            }
        }
        changed
    }

    fn rename_blocks(&mut self, new_blocks: &IndexVec<BlockId, Option<BlockId>>) {
        for instr in self.instrs_mut() {
            match instr.kind {
                InstrKind::Jmp(ref mut target) => {
                    if let Some(new_target) = new_blocks[*target] {
                        *target = new_target
                    }
                }
                InstrKind::JmpCond(_, ref mut on_true, ref mut on_false) => {
                    if let Some(new_target) = new_blocks[*on_true] {
                        *on_true = new_target
                    }
                    if let Some(new_target) = new_blocks[*on_false] {
                        *on_false = new_target
                    }
                }
                InstrKind::Phi(_, ref mut vals) => {
                    vals.retain_mut(|(_, block)| match new_blocks[*block] {
                        Some(new_block) => {
                            *block = new_block;
                            true
                        }
                        None => false,
                    });
                }
                _ => {}
            }
        }
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
            .map(|bb| match post_idx[bb] {
                Some(post_index) => {
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

    fn all_stores(&self) -> IndexVec<LocalId, (&Block<'a>, Vec<BlockId>)> {
        let mut stores = IndexVec::with_capacity(self.locals().len());

        for local_block in self.locals().iter().map(|(_, (_, block))| *block) {
            stores.push((self.blocks.get(local_block).unwrap(), vec![]));
        }

        for (block_id, block) in self.blocks.iter() {
            for instr in block.instrs.iter() {
                if let InstrKind::Store(local, _, _) = instr.kind {
                    stores[local.local_id().unwrap()].1.push(block_id);
                }
            }
        }

        stores
    }

    fn all_locals_uses(&self) -> IndexVec<LocalId, (BlockId, Vec<BlockId>)> {
        let mut uses = index_vec![(BlockId::ENTRY, vec![]); self.locals().len()];

        for (block_id, block) in self.blocks.iter() {
            for instr in block.instrs.iter() {
                let (decl, used) = instr.kind.uses();
                if let Some(decl) = decl.and_then(|id| id.local_id()) {
                    uses[decl].0 = block_id;
                }
                if let Some(used) = used {
                    for u in used.into_vec().into_iter().filter_map(|id| id.local_id()) {
                        uses[u].1.push(block_id);
                    }
                }
            }
        }

        uses
    }

    fn all_uses(&self) -> HashMap<IrId, (BlockId, Vec<BlockId>)> {
        let mut uses = HashMap::new();

        for (block_id, block) in self.blocks.iter() {
            for instr in block.instrs.iter() {
                let (decl, used) = instr.kind.uses();
                if let Some(decl) = decl {
                    uses.insert(decl, (block_id, vec![]));
                }
                if let Some(used) = used {
                    for u in used.into_vec().into_iter() {
                        uses.entry(u).and_modify(|(_, v)| v.push(block_id));
                    }
                }
            }
        }

        uses
    }

    fn liveness_check(
        &self,
        preds: &Predecessors,
    ) -> (
        IndexVec<BlockId, Vec<LocalId>>,
        IndexVec<BlockId, Vec<LocalId>>,
    ) {
        let uses = self.all_locals_uses();
        let mut live_out = index_vec![Vec::new(); self.blocks.len()];
        let mut live_in = index_vec![Vec::new(); self.blocks.len()];
        let mut visited = index_vec![index_vec![false; self.locals().len()]; self.blocks.len()];

        for (local, (decl_block, mut blocks)) in uses.into_iter() {
            while let Some(block) = blocks.pop() {
                if visited[block][local] {
                    continue;
                }
                live_in[block].push(local);
                visited[block][local] = true;
                for pred in preds[block].iter().copied() {
                    live_out[pred].push(local);
                    if pred != decl_block {
                        blocks.push(pred);
                    }
                }
            }
        }

        (live_in, live_out)
    }

    fn phi_positions(
        &self,
        preds: &Predecessors,
        dom_frontiers: &DominanceFrontiers,
    ) -> PhiPositions {
        let mut phi_positions: PhiPositions = index_vec![vec![]; self.blocks.len()];
        let (live_in, _) = self.liveness_check(preds);
        let stores = self.all_stores();

        for (local, mut stores) in stores.into_iter().map(|(l, (_, s))| (l, s)) {
            while let Some(block) = stores.pop() {
                for frontier in dom_frontiers[block]
                    .iter()
                    .filter(|frontier| live_in[**frontier].contains(&local))
                    .copied()
                {
                    let phis = &mut phi_positions[frontier];
                    if !phis.iter().any(|(l, _, _)| *l == local) {
                        let ty = self.locals()[local].0;
                        phis.push((
                            local,
                            vec![(local, frontier); preds[frontier].len()].into(),
                            ty,
                        ));
                        if !stores.contains(&frontier) {
                            stores.push(frontier);
                        }
                    }
                }
            }
        }

        phi_positions
    }

    fn rename_ids(&mut self, preds: &Predecessors, dom_tree: &DominatorTree) {
        fn rename(
            method: &mut Method<'_>,
            block: BlockId,
            renames: &mut HashMap<IrId, IrId>,
            cur_tmp: &mut IrId,
            preds: &Predecessors,
            dom_tree: &DominatorTree,
        ) {
            let cur = renames.clone();

            for instr in method.blocks[block].instrs_mut() {
                instr.kind.rename(renames, cur_tmp);
            }

            method.successors_inner_visit(block, |s| {
                let j = preds[s.id()].iter().position(|&p| p == block).unwrap();
                for args in s.phis_args() {
                    if let Value::Id(id) = args[j].0 {
                        args[j].0 = Value::Id(renames[&id]);
                        args[j].1 = block;
                    }
                }
            });

            for child in dom_tree[block].iter().copied() {
                rename(method, child, renames, cur_tmp, preds, dom_tree);
            }

            *renames = cur;
        }

        let mut renames = HashMap::new();
        let mut cur_tmp = IrId::Renamed(0);

        rename(
            self,
            BlockId::ENTRY,
            &mut renames,
            &mut cur_tmp,
            preds,
            dom_tree,
        );
    }

    fn insert_phis(&mut self, phis: PhiPositions) {
        for (block, phis) in phis.into_iter() {
            let span = self.blocks[block].instrs.front().unwrap().span;
            for (local, args, ty) in phis {
                let kind = InstrKind::Phi(
                    IrId::Local(local),
                    args.into_vec()
                        .into_iter()
                        .map(|(val, block)| (Value::Id(IrId::Local(val)), block))
                        .collect(),
                );
                let instr = Instr::new(kind, span, ty, block);
                self.push_front(block, instr);
            }
        }
    }

    fn mem2reg(&mut self, preds: &Predecessors, dom_tree: &DominatorTree, phis: PhiPositions) {
        self.insert_phis(phis);
        self.rename_ids(preds, dom_tree);
    }

    fn dead_code_elimination(&mut self) -> bool {
        let mut res = self.remove_dead_blocks();

        // contains the (use_count, decl_block, decl_ref, variables_used_in_decl)
        let mut counter = HashMap::new();

        for (block, instr) in self
            .instrs_mut()
            .map(|instr| (instr.block, &mut instr.kind))
        {
            let (decl, used) = instr.uses();
            if let Some(used) = used.as_ref() {
                for u in used.iter() {
                    let (used, _, _, _) = counter.entry(*u).or_insert((0, None, None, None));
                    *used += 1;
                }
            }
            if let Some(decl) = decl {
                let use_count = match counter.get(&decl) {
                    Some((use_count, _, _, _)) => *use_count,
                    None => 0,
                };
                counter.insert(decl, (use_count, Some(block), Some(instr), used));
            }
        }

        while let Some(id) = counter.iter().find_map(|(id, (use_count, _, i, _))| {
            if *use_count == 0 && !i.as_ref().unwrap().is_call() {
                Some(*id)
            } else {
                None
            }
        }) {
            res = true;
            let (_, _, instr, used) = counter.remove(&id).unwrap();
            let instr = instr.unwrap();
            instr.replace_with_nop();
            if let Some(used) = used {
                for u in used.iter() {
                    let (used, _, _, _) = counter.get_mut(u).unwrap();
                    *used -= 1;
                }
            }
        }

        let undeclared_vars: HashSet<_> = counter
            .into_iter()
            .filter_map(|(id, (_, decl, _, _))| match decl {
                Some(_) => None,
                None => Some(id),
            })
            .collect();
        res |= self.remove_unecessary_phis(&undeclared_vars);
        res |= self.merge_blocks();

        res
    }

    fn merge_blocks(&mut self) -> bool {
        let mut first = BlockId::ENTRY;
        let mut last = BlockId::ENTRY;
        let mut new_blocks = index_vec![None; self.blocks.len()];

        let method_instr = self.instrs().next().unwrap().clone();

        for (id, block) in self.blocks.iter() {
            if block.can_merge() {
                last = id;
            } else {
                if first != last {
                    for block in (first.to_index()..last.to_index()).map(BlockId::from_index) {
                        new_blocks[block] = Some(last);
                    }
                }
                first = BlockId::from_index(id.to_index() + 1);
                last = first;
            }
        }

        let len = self.blocks.len();

        self.blocks.retain(|block| new_blocks[block.id()].is_none());

        let removed = len != self.blocks.len();

        if removed {
            for (id, block) in self.blocks.iter_mut() {
                let block_id = block.id();
                for (_, new_name) in new_blocks.iter_mut().filter(|(_, v)| **v == Some(block_id)) {
                    *new_name = Some(id);
                }
                block.id = id;
                new_blocks[block_id] = Some(id);
            }
            self.rename_blocks(&new_blocks);
            if method_instr.kind != self.instrs().next().unwrap().kind {
                self.push_front(BlockId::ENTRY, method_instr);
            }
        }

        removed
    }

    fn const_propagation(&mut self, default_string: &Rc<str>) -> bool {
        let mut values = HashMap::new();
        let mut work_list = self.block_ids().collect::<VecDeque<_>>();
        let mut changed = false;
        let uses = self.all_uses();

        while let Some(block) = work_list.pop_front() {
            for instr in self.blocks[block].instrs_mut().map(|instr| &mut instr.kind) {
                if let Some(used) = instr.const_fold(&mut values, &uses, default_string) {
                    changed = true;
                    work_list.extend(used);
                }
            }
        }

        changed
    }
}

impl<'a> InstrKind<'a> {
    pub fn rename(&mut self, renames: &mut HashMap<IrId, IrId>, tmp: &mut IrId) -> bool {
        match self {
            Self::Nop | Self::Method(_, _, _, _) | Self::Label(_) | Self::Jmp(_) => {}

            Self::Local(_, _) => {
                *self = Self::Nop;
                return true;
            }

            Self::JmpCond(id, _, _) => {
                let cur_id = renames.get(id).unwrap();
                *id = *cur_id;
            }
            Self::Return(val) => {
                if let Value::Id(id) = val {
                    let cur_id = renames.get(id).unwrap();
                    *id = *cur_id;
                }
            }

            Self::Store(id1, _, id2) if id1.is_ptr() => {
                let cur_id1 = renames.get(id1).unwrap();
                *id1 = *cur_id1;
                if let Value::Id(id2) = id2 {
                    let cur_id2 = renames.get(id2).unwrap();
                    *id2 = *cur_id2;
                }
            }

            Self::Store(id1, _, val) => match val {
                Value::Id(id) => {
                    let cur_id = *renames.get(id).unwrap();
                    let new_id = tmp.next_mut();
                    renames.insert(*id1, new_id);
                    *self = Self::Assign(new_id, Value::Id(cur_id));
                }
                _ => {
                    let new_id = tmp.next_mut();
                    renames.insert(*id1, new_id);
                    *id1 = new_id;
                }
            },

            Self::AssignLoad(id1, _, id2, _) if id2.is_ptr() => {
                let cur_id2 = renames.get(id2).unwrap();
                *id2 = *cur_id2;
                let new_id = tmp.next_mut();
                renames.insert(*id1, new_id);
                *id1 = new_id;
            }

            Self::AssignLoad(id1, _, id2, _) => {
                let cur_id2 = *renames.get(id2).unwrap();
                let new_id = tmp.next_mut();
                renames.insert(*id1, new_id);
                *self = Self::Assign(new_id, Value::Id(cur_id2));
            }

            Self::AssignBin(_, id, Value::Id(lhs), Value::Id(rhs)) => {
                let cur_lhs = renames.get(lhs).unwrap();
                *lhs = *cur_lhs;
                let cur_rhs = renames.get(rhs).unwrap();
                *rhs = *cur_rhs;
                let new_id = tmp.next_mut();
                renames.insert(*id, new_id);
                *id = new_id;
            }

            Self::Assign(id1, Value::Id(id2))
            | Self::AssignUn(_, id1, id2)
            | Self::AssignToObj(id1, _, Value::Id(id2))
            | Self::AssignBin(_, id1, Value::Id(id2), _)
            | Self::AssignBin(_, id1, _, Value::Id(id2)) => {
                let cur_id = renames.get(id2).unwrap();
                *id2 = *cur_id;
                let new_id = tmp.next_mut();
                renames.insert(*id1, new_id);
                *id1 = new_id;
            }

            Self::AssignCall(id1, id2, args) => {
                let cur_id2 = renames.get(id2).unwrap();
                *id2 = *cur_id2;
                for id in args.iter_mut().filter_map(|(_, val)| match val {
                    Value::Id(id) => Some(id),
                    _ => None,
                }) {
                    let cur_id = renames.get(id).unwrap();
                    *id = *cur_id;
                }
                let new_id = tmp.next_mut();
                renames.insert(*id1, new_id);
                *id1 = new_id;
            }

            Self::AssignStaticCall(id1, _, _, args) => {
                for id in args.iter_mut().filter_map(|(_, val)| match val {
                    Value::Id(id) => Some(id),
                    _ => None,
                }) {
                    let cur_id = renames.get(id).unwrap();
                    *id = *cur_id;
                }
                let new_id = tmp.next_mut();
                renames.insert(*id1, new_id);
                *id1 = new_id;
            }

            Self::AssignExtract(id1, id2, _) => {
                let cur_id2 = renames.get(id2).unwrap();
                *id2 = *cur_id2;
                let new_id = tmp.next_mut();
                renames.insert(*id1, new_id);
                *id1 = new_id;
            }

            Self::Param(_, id)
            | Self::AssignDefault(id, _)
            | Self::Assign(id, _)
            | Self::Attr(_, id)
            | Self::AssignBin(_, id, _, _)
            | Self::AssignToObj(id, _, _) => {
                let new_id = tmp.next_mut();
                renames.insert(*id, new_id);
                *id = new_id;
            }

            Self::Phi(id, vals) => {
                if !id.is_local() {
                    for val in vals.iter_mut().filter_map(|(val, _)| match val {
                        Value::Id(id) if !id.is_renamed() => Some(id),
                        _ => None,
                    }) {
                        let cur_val = renames.get(val).unwrap();
                        *val = *cur_val;
                    }
                }
                let new_id = tmp.next_mut();
                renames.insert(*id, new_id);
                *id = new_id;
            }
        }

        false
    }

    fn replace_with_nop(&mut self) {
        *self = Self::Nop;
    }

    fn const_fold<'b>(
        &'b mut self,
        values: &mut HashMap<IrId, Value>,
        uses: &'b HashMap<IrId, (BlockId, Vec<BlockId>)>,
        default_string: &Rc<str>,
    ) -> Option<impl Iterator<Item = BlockId> + '_> {
        match self {
            InstrKind::Assign(id, val) => {
                Self::insert_val(values, *id, val.clone());
                let uses = uses[id].1.iter().copied();
                self.replace_with_nop();
                Some(uses)
            }
            InstrKind::AssignUn(op, id, arg) => {
                let op = *op;
                let id = *id;
                Self::replace_id(values, arg);
                let arg = *arg;
                if Self::const_fold_un(values, op, id, arg) {
                    let uses = uses[&id].1.iter().copied();
                    self.replace_with_nop();
                    Some(uses)
                } else {
                    None
                }
            }
            InstrKind::AssignBin(op, id, lhs, rhs) => {
                let op = *op;
                let id = *id;
                Self::try_replace(values, lhs);
                Self::try_replace(values, rhs);
                let lhs = lhs.clone();
                let rhs = rhs.clone();
                if Self::const_fold_bin(values, op, id, lhs, rhs) {
                    let uses = uses[&id].1.iter().copied();
                    self.replace_with_nop();
                    Some(uses)
                } else {
                    None
                }
            }
            InstrKind::AssignDefault(id, ty) => {
                let id = *id;
                let ty = *ty;
                if Self::const_fold_default(values, default_string.clone(), id, ty) {
                    let uses = uses[&id].1.iter().copied();
                    self.replace_with_nop();
                    Some(uses)
                } else {
                    None
                }
            }
            InstrKind::JmpCond(id, on_true, on_false) => {
                Self::replace_id(values, id);
                let id = *id;
                let on_true = *on_true;
                let on_false = *on_false;
                self.const_fold_jmp_cond(values, id, on_true, on_false);
                None
            }
            InstrKind::AssignCall(_, id, args) => {
                for arg in args.iter_mut().map(|(_, id)| id) {
                    Self::try_replace(values, arg);
                }
                Self::replace_id(values, id);
                None
            }
            InstrKind::AssignStaticCall(_, _, _, args) => {
                for arg in args.iter_mut().map(|(_, id)| id) {
                    Self::try_replace(values, arg);
                }
                None
            }
            InstrKind::AssignExtract(_, id, _) => {
                Self::replace_id(values, id);
                None
            }
            InstrKind::Phi(_, vals) => {
                for (val, _) in vals.iter_mut() {
                    Self::try_replace(values, val);
                }
                None
            }
            InstrKind::Return(id) => {
                Self::try_replace(values, id);
                None
            }
            InstrKind::Store(_, _, val) => {
                Self::try_replace(values, val);
                None
            }
            _ => None,
        }
    }

    fn try_replace(values: &HashMap<IrId, Value>, val: &mut Value) {
        if let Value::Id(id) = val {
            if let Some(new_val) = values.get(id) {
                *val = new_val.clone();
            }
        }
    }

    fn replace_id(values: &HashMap<IrId, Value>, id: &mut IrId) {
        if let Some(Value::Id(val)) = values.get(id) {
            *id = *val;
        }
    }

    fn const_fold_jmp_cond(
        &mut self,
        values: &mut HashMap<IrId, Value>,
        id: IrId,
        on_true: BlockId,
        on_false: BlockId,
    ) {
        if let Some(Value::Bool(val)) = values.get(&id) {
            if *val {
                *self = InstrKind::Jmp(on_true);
            } else {
                *self = InstrKind::Jmp(on_false);
            }
        }
    }

    fn const_fold_default(
        values: &mut HashMap<IrId, Value>,
        default_string: Rc<str>,
        id: IrId,
        ty: TypeId,
    ) -> bool {
        match ty {
            TypeId::INT => {
                Self::insert_val(values, id, Value::Int(0));
            }
            TypeId::BOOL => {
                Self::insert_val(values, id, Value::Bool(false));
            }
            TypeId::STRING => {
                Self::insert_val(values, id, Value::Str(default_string.clone()));
            }
            _ => {
                Self::insert_val(values, id, Value::Void);
            }
        }
        true
    }

    fn const_fold_un(values: &mut HashMap<IrId, Value>, op: UnOp, id: IrId, arg: IrId) -> bool {
        match op {
            UnOp::IsVoid if values.get(&arg) == Some(&Value::Void) => {
                Self::insert_val(values, id, Value::Bool(true));
                true
            }

            UnOp::IsVoid
                if matches!(
                    values.get(&arg),
                    Some(Value::Int(_) | Value::Str(_) | Value::Bool(_))
                ) =>
            {
                Self::insert_val(values, id, Value::Bool(false));
                true
            }

            UnOp::Complement => match values.get(&arg) {
                Some(Value::Int(arg)) => {
                    let val = -arg;
                    Self::insert_val(values, id, Value::Int(val));
                    true
                }
                _ => false,
            },

            UnOp::Not => match values.get(&arg) {
                Some(Value::Bool(arg)) => {
                    let val = !arg;
                    Self::insert_val(values, id, Value::Bool(val));
                    true
                }
                _ => false,
            },

            _ => false,
        }
    }

    fn const_fold_bin(
        values: &mut HashMap<IrId, Value>,
        op: BinOp,
        id: IrId,
        lhs: Value,
        rhs: Value,
    ) -> bool {
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                match op {
                    BinOp::Add => {
                        let val = lhs + rhs;
                        Self::insert_val(values, id, Value::Int(val));
                    }
                    BinOp::Sub => {
                        let val = lhs - rhs;
                        Self::insert_val(values, id, Value::Int(val));
                    }
                    BinOp::Mul => {
                        let val = lhs * rhs;
                        Self::insert_val(values, id, Value::Int(val));
                    }
                    BinOp::Div => {
                        let val = lhs / rhs;
                        Self::insert_val(values, id, Value::Int(val));
                    }
                    BinOp::Lt => {
                        let val = lhs < rhs;
                        Self::insert_val(values, id, Value::Bool(val));
                    }
                    BinOp::Le => {
                        let val = lhs <= rhs;
                        Self::insert_val(values, id, Value::Bool(val));
                    }
                    BinOp::Eq => {
                        let val = lhs == rhs;
                        Self::insert_val(values, id, Value::Bool(val));
                    }
                }
                true
            }
            _ => false,
        }
    }

    fn insert_val(values: &mut HashMap<IrId, Value>, id: IrId, val: Value) -> Option<Value> {
        values.insert(id, val)
    }
}
