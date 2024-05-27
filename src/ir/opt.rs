use std::{
    collections::{HashMap, HashSet, VecDeque},
    slice::IterMut,
};

use crate::{
    ast::{BinOp, UnOp},
    index_vec::{index_vec, IndexVec, Key},
    types::TypeId,
};

use super::{
    block::BlockId,
    builder::{FunctionBuilder, GlobalValue, IrBuilder},
    Instr, InstrKind, IrId, LocalId, Value,
};

type Predecessors = IndexVec<BlockId, Vec<BlockId>>;
type ImmediateDominators = IndexVec<BlockId, BlockId>;
type DominatorTree = IndexVec<BlockId, Vec<BlockId>>;
type Dominated = IndexVec<BlockId, Vec<BlockId>>;
type DominanceFrontiers = IndexVec<BlockId, Vec<BlockId>>;
type PhiPositions = IndexVec<BlockId, Vec<(LocalId, Box<[(LocalId, BlockId)]>, TypeId)>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstrId(usize);

impl InstrId {
    pub fn prev(self) -> Self {
        Self(self.0 - 1)
    }

    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

impl Key for InstrId {
    fn to_index(self) -> usize {
        self.0
    }

    fn from_index(index: usize) -> Self {
        Self(index)
    }
}

pub struct Function {
    instrs: IndexVec<InstrId, Instr>,
    blocks: IndexVec<BlockId, (InstrId, InstrId, BlockId)>,
}

impl Function {
    pub fn new(
        instrs: IndexVec<InstrId, Instr>,
        blocks: IndexVec<BlockId, (InstrId, InstrId, BlockId)>,
    ) -> Self {
        Self { instrs, blocks }
    }

    fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, mut f: F) {
        let (_, block_end, _) = &self.blocks[block];

        let block_end = block_end.prev();

        match self.instrs[block_end].kind {
            InstrKind::Jmp(target) => {
                f(target);
            }
            InstrKind::JmpCond {
                on_true, on_false, ..
            } => {
                f(on_true);
                f(on_false);
            }
            InstrKind::Return(_) => {}
            _ => unreachable!(),
        }
    }

    fn successors_instrs_visit<F: FnMut((BlockId, IterMut<Instr>))>(
        &mut self,
        block: BlockId,
        mut f: F,
    ) {
        let (_, block_end, _) = &self.blocks[block];
        let block_end = block_end.prev();

        match self.instrs[block_end].kind {
            InstrKind::Jmp(target) => {
                f((target, self.instrs_block_mut(target).iter_mut()));
            }
            InstrKind::JmpCond {
                on_true, on_false, ..
            } => {
                f((on_true, self.instrs_block_mut(on_true).iter_mut()));
                f((on_false, self.instrs_block_mut(on_false).iter_mut()));
            }
            InstrKind::Return(_) => {}
            _ => unreachable!(),
        }
    }

    fn instrs_block(&self, block: BlockId) -> &[Instr] {
        let (start, end, _) = self.blocks[block];
        &self.instrs[start..end]
    }

    fn instrs_block_mut(&mut self, block: BlockId) -> &mut [Instr] {
        let (start, end, _) = self.blocks[block];
        &mut self.instrs[start..end]
    }
}

pub struct FunctionOptmizer {
    function: Function,
    locals:   IndexVec<LocalId, (TypeId, BlockId)>,
    idoms:    ImmediateDominators,
    dom_tree: DominatorTree,
}

impl FunctionOptmizer {
    pub fn from_builder(mut builder: FunctionBuilder) -> Self {
        builder.set_labels();

        let mut instrs = IndexVec::new();
        let mut blocks = IndexVec::with_capacity(builder.blocks.len());
        let mut idoms = IndexVec::with_capacity(blocks.len());
        let mut dom_tree = IndexVec::with_capacity(blocks.len());

        for block in builder.blocks.into_inner() {
            let start = instrs.len();
            instrs.extend(block.instrs);
            let end = instrs.len();
            blocks.push((InstrId(start), InstrId(end), block.id));
            idoms.push(block.idom.unwrap_or(BlockId::ENTRY));
            dom_tree.push(block.doms);
        }

        Self {
            function: Function { instrs, blocks },
            idoms,
            dom_tree,
            locals: builder.locals,
        }
    }

    fn block_ids(&self) -> impl Iterator<Item = BlockId> {
        self.function.blocks.indices()
    }

    fn blocks(&self) -> &IndexVec<BlockId, (InstrId, InstrId, BlockId)> {
        &self.function.blocks
    }

    fn blocks_mut(&mut self) -> &mut IndexVec<BlockId, (InstrId, InstrId, BlockId)> {
        &mut self.function.blocks
    }

    fn instrs_mut(&mut self) -> &mut IndexVec<InstrId, Instr> {
        &mut self.function.instrs
    }

    fn instrs(&self) -> &IndexVec<InstrId, Instr> {
        &self.function.instrs
    }

    fn instrs_block(&self, block: BlockId) -> &[Instr] {
        self.function.instrs_block(block)
    }

    fn blocks_count(&self) -> usize {
        self.function.blocks.len()
    }

    pub fn optimize(&mut self) {
        let preds = self.predecessors();
        let dom_frontiers = self.dominance_frontiers(&preds);
        let phis = self.phi_positions(&preds, &dom_frontiers);

        self.mem2reg(&preds, phis);

        while self.const_propagation() | self.dead_code_elimination() {}
    }

    fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, f: F) {
        self.function.successors_visit(block, f)
    }

    fn predecessors(&self) -> Predecessors {
        let mut preds = index_vec![vec![]; self.blocks_count()];

        for bb in self.block_ids() {
            self.successors_visit(bb, |succ| preds[succ].push(bb));
        }

        preds
    }

    fn reachability_blocks(&self) -> IndexVec<BlockId, bool> {
        let mut visited = index_vec![false; self.blocks_count()];
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
        let mut new_blocks = index_vec![None; self.blocks_count()];
        let mut reachable = self.reachability_blocks().into_iter().map(|(_, v)| v);

        let len = self.blocks_count();
        self.blocks_mut().retain(|_| reachable.next().unwrap());

        let removed = len != self.blocks_count();

        if removed {
            for (id, (_, _, block)) in self.blocks_mut() {
                new_blocks[*block] = Some(id);
                *block = id;
            }

            self.rename_blocks(&new_blocks);
        }

        removed
    }

    fn remove_unecessary_phis(&mut self, undeclared_vars: &HashSet<IrId>) -> bool {
        let mut changed = false;
        for (_, instr) in self.instrs_mut() {
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
                    let val = vals[0].0.clone();
                    instr.kind = InstrKind::Assign(*id, val);
                }
            }
        }
        changed
    }

    fn rename_blocks(&mut self, new_blocks: &IndexVec<BlockId, Option<BlockId>>) {
        let mut remove = false;
        for (_, instr) in self.instrs_mut() {
            if remove {
                match instr.kind {
                    InstrKind::Label(_) => remove = false,
                    _ => instr.kind.replace_with_nop(),
                }
            }
            match instr.kind {
                InstrKind::Label(ref mut block) => match new_blocks[*block] {
                    Some(new_block) => *block = new_block,
                    None => {
                        instr.kind.replace_with_nop();
                        remove = true;
                    }
                },
                InstrKind::Jmp(ref mut target) => {
                    if let Some(new_target) = new_blocks[*target] {
                        *target = new_target
                    }
                }
                InstrKind::JmpCond {
                    ref mut on_true,
                    ref mut on_false,
                    ..
                } => {
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

    pub fn dominators(&self, doms: &Dominated) -> (ImmediateDominators, DominatorTree) {
        let mut idoms = index_vec![BlockId::ENTRY; self.blocks_count()];
        let mut dom_tree = index_vec![vec![]; self.blocks_count()];

        for dom in self.block_ids().skip(1) {
            for block in doms[dom]
                .iter()
                .filter_map(|&d| if d != dom { Some(d) } else { None })
            {
                if self.block_ids().all(|other| {
                    if other == block || other == dom || !doms[other].contains(&block) {
                        true
                    } else {
                        doms[other].contains(&dom)
                    }
                }) {
                    idoms[block] = dom;
                    dom_tree[dom].push(block);
                }
            }
        }

        (idoms, dom_tree)
    }

    pub fn dominated_blocks(&self) -> Dominated {
        let mut dom_tree = IndexVec::with_capacity(self.blocks_count());
        dom_tree.push(self.block_ids().collect::<Vec<_>>());

        for block in self.block_ids().skip(1) {
            let mut visited = index_vec![false; self.blocks_count()];
            visited[BlockId::ENTRY] = true;

            let mut work_list = vec![BlockId::ENTRY];

            while let Some(bb) = work_list.pop() {
                self.successors_visit(bb, |succ| {
                    if succ != block && !visited[succ] {
                        visited[succ] = true;
                        work_list.push(succ);
                    }
                });
            }

            dom_tree.push(
                self.block_ids()
                    .filter(|bb| !visited[*bb])
                    .collect::<Vec<_>>(),
            );
        }

        dom_tree
    }

    fn dominance_frontiers(&self, preds: &Predecessors) -> DominanceFrontiers {
        let mut frontiers = index_vec![vec![]; self.blocks_count()];

        for bb in self.block_ids().filter(|b| preds[*b].len() >= 2) {
            let idom = self.idoms[bb];

            for pred in preds[bb].iter().copied() {
                let mut runner = pred;
                while runner != idom {
                    let df = &mut frontiers[runner];
                    if !df.contains(&bb) {
                        df.push(bb);
                    }
                    runner = self.idoms[runner];
                }
            }
        }

        frontiers
    }

    fn all_stores(&self) -> IndexVec<LocalId, (BlockId, Vec<BlockId>)> {
        let mut stores = IndexVec::with_capacity(self.locals.len());

        for local_block in self.locals.inner().iter().map(|(_, block)| *block) {
            stores.push((local_block, vec![]));
        }

        for block_id in self.block_ids() {
            for instr in self.instrs_block(block_id) {
                if let InstrKind::Store(local, _, _) = instr.kind {
                    stores[local.local_id().unwrap()].1.push(block_id);
                }
            }
        }

        stores
    }

    fn all_locals_uses(&self) -> IndexVec<LocalId, (BlockId, Vec<BlockId>)> {
        let mut uses = index_vec![(BlockId::ENTRY, vec![]); self.locals.len()];

        for block_id in self.block_ids() {
            for instr in self.instrs_block(block_id) {
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

    fn all_uses(&self) -> HashMap<IrId, (InstrId, Vec<InstrId>)> {
        let mut uses = HashMap::new();

        for (id, instr) in self.instrs() {
            let (decl, used) = instr.kind.uses();
            if let Some(decl) = decl {
                uses.entry(decl).or_insert((id, vec![])).0 = id;
            }
            if let Some(used) = used {
                for u in used.into_vec() {
                    uses.entry(u).or_insert((id, vec![])).1.push(id);
                }
            }
        }

        uses
    }

    fn local_liveness_check(
        &self,
        preds: &Predecessors,
    ) -> (
        IndexVec<BlockId, Vec<LocalId>>,
        IndexVec<BlockId, Vec<LocalId>>,
    ) {
        let uses = self.all_locals_uses();
        let mut live_out = index_vec![Vec::new(); self.blocks_count()];
        let mut live_in = index_vec![Vec::new(); self.blocks_count()];
        let mut visited = index_vec![index_vec![false; self.locals.len()]; self.blocks_count()];

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
        let mut phi_positions: PhiPositions = index_vec![vec![]; self.blocks_count()];
        let (live_in, _) = self.local_liveness_check(preds);
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
                        let ty = self.locals[local].0;
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

    fn rename_ids(&mut self, preds: &Predecessors) {
        fn rename(
            function: &mut Function,
            block: BlockId,
            renames: &mut HashMap<IrId, IrId>,
            cur_tmp: &mut IrId,
            preds: &Predecessors,
            dom_tree: &DominatorTree,
        ) {
            let cur = renames.clone();

            for instr in function.instrs_block_mut(block) {
                instr.kind.rename(renames, cur_tmp);
            }

            function.successors_instrs_visit(block, |(s, i)| {
                let j = preds[s].iter().position(|&p| p == block).unwrap();
                for args in i.filter_map(|instr| match instr.kind {
                    InstrKind::Phi(_, ref mut args) => Some(args),
                    _ => None,
                }) {
                    if let Value::Id(id) = args[j].0 {
                        args[j].0 = Value::Id(renames[&id]);
                        args[j].1 = block;
                    }
                }
            });

            for child in dom_tree[block].iter().copied() {
                rename(function, child, renames, cur_tmp, preds, dom_tree);
            }

            *renames = cur;
        }

        let mut renames = HashMap::new();
        let mut cur_tmp = IrId::Renamed(0);

        rename(
            &mut self.function,
            BlockId::ENTRY,
            &mut renames,
            &mut cur_tmp,
            preds,
            &self.dom_tree,
        );
    }

    fn insert_phis(&mut self, phis: PhiPositions) {
        for (block, phis) in phis.into_iter() {
            for (local, args, ty) in phis {
                let kind = InstrKind::Phi(
                    IrId::Local(local),
                    args.into_vec()
                        .into_iter()
                        .map(|(val, block)| (Value::Id(IrId::Local(val)), block))
                        .collect(),
                );
                let instr = Instr::new(kind, ty);
                let (start, _, _) = self.blocks()[block];
                self.instrs_mut().insert(start.next(), instr);
                let (_, end, _) = &mut self.blocks_mut()[block];
                *end = end.next();

                for block in self.blocks_mut()[block..].iter_mut().skip(1) {
                    block.0 = block.0.next();
                    block.1 = block.1.next();
                }
            }
        }
    }

    fn mem2reg(&mut self, preds: &Predecessors, phis: PhiPositions) {
        self.insert_phis(phis);
        self.rename_ids(preds);
    }

    fn dead_code_elimination(&mut self) -> bool {
        let mut res = self.remove_dead_blocks();

        // contains the (use_count, decl_ref, variables_used_in_decl)
        let mut counter = HashMap::new();

        for instr in self
            .instrs_mut()
            .into_iter()
            .map(|(_, instr)| &mut instr.kind)
        {
            let (decl, used) = instr.uses();
            if let Some(used) = used.as_ref() {
                for u in used.iter() {
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

        while let Some(id) = counter.iter().find_map(|(id, (use_count, i, _))| {
            if *use_count == 0 && !i.as_ref().unwrap().is_function() {
                Some(*id)
            } else {
                None
            }
        }) {
            res = true;
            let (_, instr, used) = counter.remove(&id).unwrap();
            let instr = instr.unwrap();
            instr.replace_with_nop();
            if let Some(used) = used {
                for u in used.iter() {
                    let (used, _, _) = counter.get_mut(u).unwrap();
                    *used -= 1;
                }
            }
        }

        let undeclared_vars: HashSet<_> = counter
            .into_iter()
            .filter_map(|(id, (_, decl, _))| match decl {
                Some(_) => None,
                None => Some(id),
            })
            .collect();
        res |= self.remove_unecessary_phis(&undeclared_vars);

        res
    }

    fn const_propagation(&mut self) -> bool {
        let mut values = HashMap::new();
        let mut work_list = (0..self.instrs().len())
            .map(InstrId)
            .collect::<VecDeque<_>>();
        let mut changed = false;
        let uses = self.all_uses();

        while let Some(id) = work_list.pop_front() {
            let instr = &mut self.instrs_mut()[id];
            if let Some(used) = instr.kind.const_fold(&mut values, &uses) {
                changed = true;
                work_list.extend(used);
            }
        }

        changed
    }
}

impl InstrKind {
    pub fn rename(&mut self, renames: &mut HashMap<IrId, IrId>, tmp: &mut IrId) -> bool {
        match self {
            Self::Nop
            | Self::Function { .. }
            | Self::Vtable(_, _)
            | Self::Label(_)
            | Self::Jmp(_) => {}

            Self::Local(_, _) => {
                *self = Self::Nop;
                return true;
            }

            Self::JmpCond { src: id, .. } => {
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

            Self::AssignLoad { dst, src, .. } if src.is_ptr() => {
                let cur_id2 = renames.get(src).unwrap();
                *src = *cur_id2;
                let new_id = tmp.next_mut();
                renames.insert(*dst, new_id);
                *dst = new_id;
            }

            Self::AssignLoad { dst, src, .. } => {
                let cur_id2 = *renames.get(src).unwrap();
                let new_id = tmp.next_mut();
                renames.insert(*dst, new_id);
                *self = Self::Assign(new_id, Value::Id(cur_id2));
            }

            Self::AssignBin {
                dst,
                lhs: Value::Id(lhs),
                rhs: Value::Id(rhs),
                ..
            } => {
                let cur_lhs = renames.get(lhs).unwrap();
                *lhs = *cur_lhs;
                let cur_rhs = renames.get(rhs).unwrap();
                *rhs = *cur_rhs;
                let new_id = tmp.next_mut();
                renames.insert(*dst, new_id);
                *dst = new_id;
            }

            Self::Assign(dst, Value::Id(src))
            | Self::AssignUn { dst, src, .. }
            | Self::AssignToObj(dst, _, Value::Id(src))
            | Self::AssignBin {
                dst,
                lhs: Value::Id(src),
                ..
            }
            | Self::AssignBin {
                dst,
                rhs: Value::Id(src),
                ..
            } => {
                let cur_id = renames.get(src).unwrap();
                *src = *cur_id;
                let new_id = tmp.next_mut();
                renames.insert(*dst, new_id);
                *dst = new_id;
            }

            Self::AssignCall(id1, id2, args) => {
                if !id2.is_global() {
                    let cur_id2 = renames.get(id2).unwrap();
                    *id2 = *cur_id2;
                }
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

            Self::Param(_, dst)
            | Self::Assign(dst, _)
            | Self::Attr(_, dst)
            | Self::AssignBin { dst, .. }
            | Self::AssignToObj(dst, _, _) => {
                let new_id = tmp.next_mut();
                renames.insert(*dst, new_id);
                *dst = new_id;
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
        uses: &'b HashMap<IrId, (InstrId, Vec<InstrId>)>,
    ) -> Option<impl Iterator<Item = InstrId> + '_> {
        match self {
            InstrKind::Assign(id, val) => {
                Self::insert_val(values, *id, val.clone());
                let uses = uses[id].1.iter().copied();
                self.replace_with_nop();
                Some(uses)
            }
            InstrKind::AssignUn { op, dst, src } => {
                let op = *op;
                let dst = *dst;
                Self::replace_id(values, src);
                let src = *src;
                if Self::const_fold_un(values, op, dst, src) {
                    let uses = uses[&dst].1.iter().copied();
                    self.replace_with_nop();
                    Some(uses)
                } else {
                    None
                }
            }
            InstrKind::AssignBin { op, dst, lhs, rhs } => {
                let op = *op;
                let dst = *dst;
                Self::try_replace(values, lhs);
                Self::try_replace(values, rhs);
                let lhs = lhs.clone();
                let rhs = rhs.clone();
                if Self::const_fold_bin(values, op, dst, lhs, rhs) {
                    let uses = uses[&dst].1.iter().copied();
                    self.replace_with_nop();
                    Some(uses)
                } else {
                    None
                }
            }

            InstrKind::JmpCond {
                src,
                on_true,
                on_false,
            } => {
                Self::replace_id(values, src);
                let id = *src;
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

pub struct IrOptmizer {
    functions: Vec<FunctionOptmizer>,
    vtables:   Vec<InstrKind>,
}

impl IrOptmizer {
    pub fn from_builder(builder: IrBuilder) -> Self {
        let mut functions = vec![];
        let mut vtables = vec![];

        for global in builder.globals.into_iter().filter_map(|(_, val)| val) {
            match global {
                GlobalValue::Vtable(kind) => vtables.push(kind),
                GlobalValue::Function(function) => {
                    functions.push(FunctionOptmizer::from_builder(function))
                }
            }
        }

        Self { functions, vtables }
    }

    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionOptmizer> {
        self.functions.iter_mut()
    }

    pub fn optimize(&mut self) {
        for function in self.functions.iter_mut() {
            function.optimize();
        }
    }

    fn instrs_with_nops(&self) -> impl Iterator<Item = &InstrKind> {
        self.vtables.iter().chain(
            self.functions
                .iter()
                .flat_map(|m| m.instrs().inner().iter().map(|i| &i.kind)),
        )
    }

    pub fn instrs(&self) -> impl Iterator<Item = &InstrKind> {
        self.instrs_with_nops().filter(|i| !i.is_nop())
    }
}
