use std::{collections::VecDeque, slice::IterMut};

use crate::{
    ast::{BinOp, UnOp},
    fxhash::FxHashMap,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug)]
pub struct BlockData {
    start: InstrId,
    end:   InstrId,
    id:    BlockId,
}

#[derive(Debug)]
pub struct Function {
    instrs: IndexVec<InstrId, Instr>,
    blocks: IndexVec<BlockId, BlockData>,
}

impl Function {
    pub fn new(instrs: IndexVec<InstrId, Instr>, blocks: IndexVec<BlockId, BlockData>) -> Self {
        Self { instrs, blocks }
    }

    fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, mut f: F) {
        let BlockData { end, .. } = self.blocks[block];

        let block_end = end.prev();

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
        let BlockData { end, .. } = self.blocks[block];
        let end = end.prev();

        match self.instrs[end].kind {
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
        let BlockData { start, end, .. } = self.blocks[block];
        &self.instrs[start..end]
    }

    fn instrs_block_mut(&mut self, block: BlockId) -> &mut [Instr] {
        let BlockData { start, end, .. } = self.blocks[block];
        &mut self.instrs[start..end]
    }
}

#[derive(Debug)]
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
            let block_data = BlockData {
                start: InstrId(start),
                end:   InstrId(end),
                id:    block.id,
            };
            blocks.push(block_data);
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

    fn blocks(&self) -> &IndexVec<BlockId, BlockData> {
        &self.function.blocks
    }

    fn blocks_mut(&mut self) -> &mut IndexVec<BlockId, BlockData> {
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

    fn instrs_count(&self) -> usize {
        self.function.instrs.len()
    }

    fn phis_count(phis: &PhiPositions) -> usize {
        phis.iter().map(|(_, phis)| phis.len()).sum()
    }

    pub fn optimize(&mut self) {
        let preds = self.predecessors();
        let dom_frontiers = self.dominance_frontiers(&preds);
        let phis = self.phi_positions(&preds, &dom_frontiers);

        self.mem2reg(&preds, phis);

        while self.const_propagation() | self.dead_code_elimination() {}

        self.remove_nops();
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
            for (id, block) in self
                .blocks_mut()
                .into_iter()
                .map(|(id, block)| (id, &mut block.id))
            {
                new_blocks[*block] = Some(id);
                *block = id;
            }

            self.rename_blocks(&new_blocks);
        }

        removed
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
                InstrKind::Phi(id, ref mut vals) => {
                    vals.retain_mut(|(_, block)| match new_blocks[*block] {
                        Some(new_block) => {
                            *block = new_block;
                            true
                        }
                        None => false,
                    });
                    if vals.is_empty() {
                        instr.kind.replace_with_nop();
                    } else if vals.len() == 1 {
                        let val = vals[0].0.clone();
                        instr.kind = InstrKind::Assign(id, val);
                    }
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

    fn all_uses(&self) -> FxHashMap<IrId, (InstrId, Vec<InstrId>)> {
        let mut uses = FxHashMap::default();

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
            renames: &mut FxHashMap<IrId, Vec<IrId>>,
            cur_tmp: &mut IrId,
            preds: &Predecessors,
            dom_tree: &DominatorTree,
        ) {
            let mut defined = vec![];
            for instr in function.instrs_block_mut(block) {
                if let Some(id) = instr.kind.rename(renames, cur_tmp) {
                    defined.push(id);
                }
            }

            function.successors_instrs_visit(block, |(s, i)| {
                let j = preds[s].iter().position(|&p| p == block).unwrap();
                for args in i.filter_map(|instr| match instr.kind {
                    InstrKind::Phi(_, ref mut args) => Some(args),
                    _ => None,
                }) {
                    if let Value::Id(id) = args[j].0 {
                        args[j].0 = Value::Id(*renames[&id].last().unwrap());
                        args[j].1 = block;
                    }
                }
            });

            for child in dom_tree[block].iter().copied() {
                rename(function, child, renames, cur_tmp, preds, dom_tree);
            }

            for id in defined.iter() {
                renames.get_mut(id).unwrap().pop();
            }
        }

        let mut renames = FxHashMap::default();
        let mut cur_tmp = IrId::Renamed(0);

        rename(
            &mut self.function,
            BlockId::ENTRY,
            &mut renames,
            &mut cur_tmp,
            preds,
            &self.dom_tree,
        )
    }

    fn insert_phis(&mut self, phis: PhiPositions) {
        let mut instrs = IndexVec::with_capacity(self.instrs_count() + Self::phis_count(&phis));
        std::mem::swap(&mut instrs, &mut self.function.instrs);

        let mut instrs = instrs.into_inner().into_iter();

        self.instrs_mut().push(instrs.next().unwrap());

        for (block, phis) in phis.into_iter() {
            if !block.is_entry() {
                self.instrs_mut()
                    .push(Instr::new(InstrKind::Label(block), TypeId::SelfType));
            }
            self.instrs_mut()
                .extend(phis.into_iter().map(|(local, args, ty)| {
                    let kind = InstrKind::Phi(
                        IrId::Local(local),
                        args.into_vec()
                            .into_iter()
                            .map(|(val, block)| (Value::Id(IrId::Local(val)), block))
                            .collect(),
                    );
                    Instr::new(kind, ty)
                }));
            let block_len =
                self.blocks()[block].end.to_index() - self.blocks()[block].start.to_index();

            self.instrs_mut().extend(
                instrs
                    .by_ref()
                    .skip_while(|instr| matches!(instr.kind, InstrKind::Label(_)))
                    .take(block_len - 1),
            );
        }

        self.update_blocks();
    }

    fn remove_nops(&mut self) {
        self.function.instrs.retain(|instr| !instr.kind.is_nop());
        self.function.instrs.shrink_to_fit();
        self.update_blocks();
    }

    fn update_blocks(&mut self) {
        let mut cur_block = BlockId::ENTRY;
        let mut cur_start = InstrId(0);
        let mut blocks = IndexVec::with_capacity(self.blocks_count());

        for (id, instr) in self.instrs() {
            match instr.kind {
                InstrKind::Label(block) => {
                    let data = BlockData {
                        start: cur_start,
                        end:   id,
                        id:    cur_block,
                    };

                    blocks.push(data);

                    cur_block = block;
                    cur_start = id;
                }
                InstrKind::Return(_) => {
                    let data = BlockData {
                        start: cur_start,
                        end:   id.next(),
                        id:    cur_block,
                    };

                    blocks.push(data);
                }
                _ => {}
            }
        }

        self.function.blocks = blocks;
    }

    fn mem2reg(&mut self, preds: &Predecessors, phis: PhiPositions) {
        self.insert_phis(phis);
        self.rename_ids(preds);
    }

    fn dead_code_elimination(&mut self) -> bool {
        let mut res = self.remove_dead_blocks();

        // contains the (use_count, decl_ref, variables_used_in_decl)
        let mut counter = FxHashMap::default();

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

        res
    }

    fn const_propagation(&mut self) -> bool {
        let mut values = FxHashMap::default();
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
    pub fn rename(
        &mut self,
        renames: &mut FxHashMap<IrId, Vec<IrId>>,
        tmp: &mut IrId,
    ) -> Option<IrId> {
        match self {
            Self::Nop | Self::Vtable(_, _) | Self::Label(_) | Self::Jmp(_) => {}

            Self::Function { params, .. } => {
                for (_, param) in params.iter_mut() {
                    let new_id = tmp.next_mut();
                    let old_id = *param;
                    renames.entry(old_id).or_default().push(new_id);
                    *param = new_id;
                }
            }

            Self::Local(_, _) => {
                *self = Self::Nop;
            }

            Self::JmpCond { src: id, .. } => {
                let cur_id = renames.get(id).unwrap().last().unwrap();
                *id = *cur_id;
            }

            Self::Return(val) => {
                if let Value::Id(id) = val {
                    let cur_id = renames.get(id).unwrap().last().unwrap();
                    *id = *cur_id;
                }
            }

            Self::Store(id1, _, id2) if id1.is_ptr() => {
                let cur_id1 = renames.get(id1).unwrap().last().unwrap();
                *id1 = *cur_id1;
                if let Value::Id(id2) = id2 {
                    let cur_id2 = renames.get(id2).unwrap().last().unwrap();
                    *id2 = *cur_id2;
                }
            }

            Self::Store(id1, _, val) => match val {
                Value::Id(id) => {
                    let cur_id = *renames.get(id).unwrap().last().unwrap();
                    let new_id = tmp.next_mut();
                    let old_id = *id1;
                    renames.entry(old_id).or_default().push(new_id);
                    *self = Self::Assign(new_id, Value::Id(cur_id));
                    return Some(old_id);
                }
                val => {
                    let new_id = tmp.next_mut();
                    let old_id = *id1;
                    renames.entry(old_id).or_default().push(new_id);
                    let val = std::mem::take(val);
                    *self = Self::Assign(new_id, val);
                    return Some(old_id);
                }
            },

            Self::AssignLoad { dst, src, .. } if src.is_ptr() => {
                let cur_id2 = renames.get(src).unwrap().last().unwrap();
                *src = *cur_id2;
                let new_id = tmp.next_mut();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                return Some(old_id);
            }

            Self::AssignLoad { dst, src, .. } => {
                let cur_id2 = *renames.get(src).unwrap().last().unwrap();
                let new_id = tmp.next_mut();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *self = Self::Assign(new_id, Value::Id(cur_id2));
                return Some(old_id);
            }

            Self::AssignBin {
                dst,
                lhs: Value::Id(lhs),
                rhs: Value::Id(rhs),
                ..
            } => {
                let cur_lhs = renames.get(lhs).unwrap().last().unwrap();
                *lhs = *cur_lhs;
                let cur_rhs = renames.get(rhs).unwrap().last().unwrap();
                *rhs = *cur_rhs;
                let new_id = tmp.next_mut();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                return Some(old_id);
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
                let cur_id = renames.get(src).unwrap().last().unwrap();
                *src = *cur_id;
                let new_id = tmp.next_mut();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                return Some(old_id);
            }

            Self::AssignCall(id1, id2, args) => {
                if !id2.is_global() {
                    let cur_id2 = renames.get(id2).unwrap().last().unwrap();
                    *id2 = *cur_id2;
                }
                for id in args.iter_mut().filter_map(|(_, val)| match val {
                    Value::Id(id) => Some(id),
                    _ => None,
                }) {
                    let cur_id = renames.get(id).unwrap().last().unwrap();
                    *id = *cur_id;
                }
                let new_id = tmp.next_mut();
                let old_id = *id1;
                renames.entry(old_id).or_default().push(new_id);
                *id1 = new_id;
                return Some(old_id);
            }

            Self::AssignExtract(id1, id2, _) => {
                let cur_id2 = renames.get(id2).unwrap().last().unwrap();
                *id2 = *cur_id2;
                let new_id = tmp.next_mut();
                let old_id = *id1;
                renames.entry(old_id).or_default().push(new_id);
                *id1 = new_id;
                return Some(old_id);
            }

            Self::Assign(dst, _) | Self::AssignBin { dst, .. } | Self::AssignToObj(dst, _, _) => {
                let new_id = tmp.next_mut();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                return Some(old_id);
            }

            Self::Phi(id, vals) => {
                if !id.is_local() {
                    for val in vals.iter_mut().filter_map(|(val, _)| match val {
                        Value::Id(id) if !id.is_renamed() => Some(id),
                        _ => None,
                    }) {
                        let cur_val = renames.get(val).unwrap().last().unwrap();
                        *val = *cur_val;
                    }
                }
                let new_id = tmp.next_mut();
                let old_id = *id;
                renames.entry(old_id).or_default().push(new_id);
                *id = new_id;
                return Some(old_id);
            }
        }

        None
    }

    fn replace_with_nop(&mut self) {
        *self = Self::Nop;
    }

    fn const_fold<'b>(
        &'b mut self,
        values: &mut FxHashMap<IrId, Value>,
        uses: &'b FxHashMap<IrId, (InstrId, Vec<InstrId>)>,
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

    fn try_replace(values: &FxHashMap<IrId, Value>, val: &mut Value) {
        while let Value::Id(id) = val {
            match values.get(id) {
                Some(new_val) => *val = new_val.clone(),
                None => break,
            }
        }
    }

    fn replace_id(values: &FxHashMap<IrId, Value>, id: &mut IrId) {
        if let Some(Value::Id(val)) = values.get(id) {
            *id = *val;
        }
    }

    fn const_fold_jmp_cond(
        &mut self,
        values: &mut FxHashMap<IrId, Value>,
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

    fn const_fold_un(values: &mut FxHashMap<IrId, Value>, op: UnOp, id: IrId, arg: IrId) -> bool {
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
        values: &mut FxHashMap<IrId, Value>,
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

    fn insert_val(values: &mut FxHashMap<IrId, Value>, id: IrId, val: Value) -> Option<Value> {
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
