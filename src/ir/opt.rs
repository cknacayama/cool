use std::{collections::VecDeque, rc::Rc};

use crate::{
    ast::{BinOp, UnOp},
    fxhash::{FxHashMap, FxHashSet},
    index_vec::{index_vec, Idx, IndexVec},
    types::TypeId,
};

use super::{
    block::BlockId,
    builder::{FunctionBuilder, GlobalValue, IrBuilder},
    GlobalId, Instr, IrId, LocalId, Value,
};

pub type Predecessors = IndexVec<BlockId, Vec<BlockId>>;
pub type Successors = IndexVec<BlockId, Vec<BlockId>>;
pub type ImmediateDominators = IndexVec<BlockId, BlockId>;
pub type DominatorTree = IndexVec<BlockId, Vec<BlockId>>;
pub type Dominated = IndexVec<BlockId, FxHashSet<BlockId>>;
pub type DominanceFrontiers = IndexVec<BlockId, Vec<BlockId>>;
pub type PhiPositions = IndexVec<BlockId, Vec<(LocalId, Box<[(LocalId, BlockId)]>, TypeId)>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstrId(usize);

impl Idx for InstrId {
    fn index(self) -> usize {
        self.0
    }

    fn new(index: usize) -> Self {
        Self(index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarUses {
    decl: InstrId,
    uses: FxHashSet<InstrId>,
}

impl VarUses {
    pub fn new(decl: InstrId) -> Self {
        Self {
            decl,
            uses: FxHashSet::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockData {
    start: InstrId,
    end:   InstrId,
    id:    BlockId,
}

#[derive(Debug, Clone)]
pub struct Function {
    instrs: IndexVec<InstrId, Instr>,
    blocks: IndexVec<BlockId, BlockData>,
}

impl Function {
    pub fn blocks(&self) -> &IndexVec<BlockId, BlockData> {
        &self.blocks
    }

    pub fn instrs(&self) -> &IndexVec<InstrId, Instr> {
        &self.instrs
    }

    pub fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, mut f: F) {
        let BlockData { end, .. } = self.blocks[block];

        let block_end = end.prev().unwrap();

        match self.instrs[block_end] {
            Instr::Jmp(target) => {
                f(target);
            }
            Instr::JmpCond {
                on_true, on_false, ..
            } => {
                f(on_true);
                f(on_false);
            }
            Instr::Return(_) => {}
            _ => unreachable!(),
        }
    }

    fn predecessors(&self) -> Predecessors {
        let mut preds = index_vec![vec![]; self.blocks.len()];

        for bb in self.blocks.indices() {
            self.successors_visit(bb, |succ| preds[succ].push(bb));
        }

        preds
    }

    fn rename_phi_arg(
        &mut self,
        pred: BlockId,
        block: BlockId,
        renames: &FxHashMap<IrId, Vec<IrId>>,
        position: usize,
        uses: &mut FxHashMap<IrId, VarUses>,
    ) {
        let iter = self
            .instrs_block_iter_mut(block)
            .filter_map(|(id, instr)| match instr {
                Instr::AssignPhi(_, args) => Some((id, args)),
                _ => None,
            });

        for (instr, args) in iter {
            if let Value::Id(id) = args[position].0 {
                let cur_id = renames[&id].last().unwrap();
                args[position].0 = Value::Id(*cur_id);
                update_var_uses(uses, cur_id, instr);
                args[position].1 = pred;
            }
        }
    }

    fn rename_successor_phi_arg(
        &mut self,
        block: BlockId,
        renames: &FxHashMap<IrId, Vec<IrId>>,
        preds: &Predecessors,
        uses: &mut FxHashMap<IrId, VarUses>,
    ) {
        let BlockData { end, .. } = self.blocks[block];
        let end = end.prev().unwrap();

        match self.instrs[end] {
            Instr::Jmp(target) => {
                let position = preds[target].iter().position(|&p| p == block).unwrap();
                self.rename_phi_arg(block, target, renames, position, uses);
            }

            Instr::JmpCond {
                on_true, on_false, ..
            } => {
                let position = preds[on_true].iter().position(|&p| p == block).unwrap();
                self.rename_phi_arg(block, on_true, renames, position, uses);
                let position = preds[on_false].iter().position(|&p| p == block).unwrap();
                self.rename_phi_arg(block, on_false, renames, position, uses);
            }
            Instr::Return(_) => {}
            _ => unreachable!(),
        }
    }

    fn instrs_block(&self, block: BlockId) -> &[Instr] {
        let BlockData { start, end, .. } = self.blocks[block];
        &self.instrs[start..end]
    }

    fn instrs_block_iter_mut(
        &mut self,
        block: BlockId,
    ) -> impl Iterator<Item = (InstrId, &mut Instr)> {
        let BlockData { start, end, .. } = self.blocks[block];
        self.instrs[start..end]
            .iter_mut()
            .enumerate()
            .map(move |(i, instr)| (start.plus(i), instr))
    }

    fn replace_block_with_nop(&mut self, block: BlockId) {
        let BlockData { start, end, .. } = self.blocks[block];
        for instr in &mut self.instrs[start..end] {
            instr.replace_with_nop();
        }
    }

    fn filter_replace_block_with_nop<P: FnMut(&Instr) -> bool>(
        &mut self,
        block: BlockId,
        mut pred: P,
    ) {
        let BlockData { start, end, .. } = self.blocks[block];
        for instr in &mut self.instrs[start..end] {
            if pred(instr) {
                instr.replace_with_nop();
            }
        }
    }
}

#[derive(Debug)]
pub struct FunctionOptmizer {
    pub(crate) function: Function,
    locals:              IndexVec<LocalId, (TypeId, BlockId)>,
    idoms:               ImmediateDominators,
    dom_tree:            DominatorTree,
}

impl FunctionOptmizer {
    pub fn from_builder(mut builder: FunctionBuilder) -> Self {
        builder.set_labels();

        let mut instrs = IndexVec::new();
        let mut blocks = IndexVec::with_capacity(builder.blocks.len());

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
        }

        let function = Function { instrs, blocks };

        Self {
            function,
            idoms: builder.idoms,
            dom_tree: builder.dom_tree,
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

    pub fn instrs(&self) -> &IndexVec<InstrId, Instr> {
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

    pub fn optimize(&mut self) -> FxHashMap<IrId, VarUses> {
        let preds = self.predecessors();
        let dom_frontiers = self.dominance_frontiers(&preds);
        let phis = self.phi_positions(&preds, &dom_frontiers);

        let mut uses = self.mem2reg(&preds, phis);

        while self.const_propagation(&mut uses) | self.dead_code_elimination(&mut uses) {}

        self.try_merge_blocks();

        uses
    }

    fn can_merge(&self, block: BlockId) -> bool {
        self.instrs_block(block).iter().all(|instr| match instr {
            Instr::Label(_) | Instr::Nop | Instr::Return(_) | Instr::Function { .. } => true,

            Instr::Jmp(target) => *target == block.plus(1),

            _ => false,
        })
    }

    fn try_merge_blocks(&mut self) {
        let mut start = BlockId::ENTRY;
        let mut end = BlockId::ENTRY;
        let mut merged = false;

        for block in self.block_ids() {
            if self.can_merge(block) {
                end = block;
            } else {
                if start != end {
                    merged = true;
                    self.merge_blocks(start, end);
                }
                start = block.next();
                end = start;
            }
        }
        if start != end {
            merged = true;
            self.merge_blocks(start, end);
        }

        if !merged {
            self.remove_nops();
            return;
        }

        let mut new_blocks = index_vec![BlockId::ENTRY; self.blocks_count()];
        self.remove_nops();

        for (id, block) in self
            .blocks_mut()
            .into_iter()
            .map(|(id, block)| (id, &mut block.id))
        {
            new_blocks[*block] = id;
            *block = id;
        }
        self.rename_merged_blocks(&new_blocks);
    }

    fn merge_blocks(&mut self, start: BlockId, end: BlockId) {
        self.function.filter_replace_block_with_nop(start, |kind| {
            !matches!(kind, Instr::Label(_) | Instr::Function { .. })
        });

        for block in (start.plus(1).index()..end.index()).map(BlockId::new) {
            self.function.replace_block_with_nop(block);
        }

        self.function.filter_replace_block_with_nop(end, |kind| {
            !matches!(kind, Instr::Jmp(_) | Instr::Return(_))
        });
    }

    fn successors_visit<F: FnMut(BlockId)>(&self, block: BlockId, f: F) {
        self.function.successors_visit(block, f)
    }

    fn predecessors(&self) -> Predecessors {
        self.function.predecessors()
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

    fn rename_merged_blocks(&mut self, new_blocks: &IndexVec<BlockId, BlockId>) {
        for (_, instr) in self.instrs_mut() {
            match instr {
                Instr::Jmp(target) => {
                    *target = new_blocks[*target];
                }
                Instr::JmpCond {
                    on_true, on_false, ..
                } => {
                    *on_true = new_blocks[*on_true];
                    *on_false = new_blocks[*on_false];
                }
                Instr::AssignPhi(_, vals) => {
                    vals.iter_mut().for_each(|(_, block)| {
                        *block = new_blocks[*block];
                    });
                }
                Instr::Label(block) => {
                    *block = new_blocks[*block];
                }
                _ => {}
            }
        }
    }

    fn rename_blocks(&mut self, new_blocks: &IndexVec<BlockId, Option<BlockId>>) {
        let mut remove = false;
        for (_, instr) in self.instrs_mut() {
            if remove {
                match instr {
                    Instr::Label(_) => remove = false,
                    _ => instr.replace_with_nop(),
                }
            }
            match instr {
                Instr::Label(block) => match new_blocks[*block] {
                    Some(new_block) => *block = new_block,
                    None => {
                        instr.replace_with_nop();
                        remove = true;
                    }
                },
                Instr::Jmp(target) => {
                    if let Some(new_target) = new_blocks[*target] {
                        *target = new_target
                    }
                }
                Instr::JmpCond {
                    on_true, on_false, ..
                } => {
                    if let Some(new_target) = new_blocks[*on_true] {
                        *on_true = new_target
                    }
                    if let Some(new_target) = new_blocks[*on_false] {
                        *on_false = new_target
                    }
                }
                Instr::AssignPhi(id, vals) => {
                    vals.retain_mut(|(_, block)| match new_blocks[*block] {
                        Some(new_block) => {
                            *block = new_block;
                            true
                        }
                        None => false,
                    });
                    if vals.is_empty() {
                        instr.replace_with_nop();
                    } else if vals.len() == 1 {
                        let val = std::mem::take(&mut vals[0].0);
                        *instr = Instr::Assign(*id, val);
                    }
                }
                _ => {}
            }
        }
    }

    pub fn dominators(&self, doms: &Dominated) -> (ImmediateDominators, DominatorTree) {
        let _ = doms;
        todo!()
    }

    pub fn dominated_blocks(&self) -> Dominated {
        let mut dom_tree = IndexVec::with_capacity(self.blocks_count());
        dom_tree.push(self.block_ids().collect());

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

            dom_tree.push(self.block_ids().filter(|bb| !visited[*bb]).collect());
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

        for block in self.locals.inner().iter().map(|(_, block)| *block) {
            stores.push((block, vec![]));
        }

        for block_id in self.block_ids() {
            for instr in self.instrs_block(block_id) {
                if let Instr::Store { dst, .. } = instr {
                    if let Some(dst) = dst.local_id() {
                        stores[dst].1.push(block_id);
                    }
                }
            }
        }

        stores
    }

    fn all_locals_uses(&self) -> IndexVec<LocalId, (BlockId, Vec<BlockId>)> {
        let mut uses = index_vec![(BlockId::ENTRY, vec![]); self.locals.len()];

        for block_id in self.block_ids() {
            for instr in self.instrs_block(block_id) {
                let (decl, used) = instr.uses();
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
                    let phis = phi_positions.get_mut(frontier).unwrap();
                    if !phis.iter().any(|(l, _, _)| *l == local) {
                        let ty = self.locals[local].0;
                        let args = vec![(local, frontier); preds[frontier].len()].into();
                        phis.push((local, args, ty));
                        if !stores.contains(&frontier) {
                            stores.push(frontier);
                        }
                    }
                }
            }
        }

        phi_positions
    }

    fn rename_ids(&mut self, preds: &Predecessors) -> FxHashMap<IrId, VarUses> {
        fn rename(
            function: &mut Function,
            block: BlockId,
            renames: &mut FxHashMap<IrId, Vec<IrId>>,
            cur_tmp: &mut IrId,
            preds: &Predecessors,
            dom_tree: &DominatorTree,
            uses: &mut FxHashMap<IrId, VarUses>,
        ) {
            let mut defined = vec![];

            for (id, instr) in function.instrs_block_iter_mut(block) {
                if let Some(id) = instr.rename(id, renames, cur_tmp, uses) {
                    defined.push(id);
                }
            }

            function.rename_successor_phi_arg(block, renames, preds, uses);

            for child in dom_tree[block].iter().copied() {
                rename(function, child, renames, cur_tmp, preds, dom_tree, uses);
            }

            for id in defined.iter() {
                renames.get_mut(id).unwrap().pop();
            }
        }

        let mut renames = FxHashMap::default();
        let mut cur_tmp = IrId::Renamed(0);
        let mut uses = FxHashMap::default();

        rename(
            &mut self.function,
            BlockId::ENTRY,
            &mut renames,
            &mut cur_tmp,
            preds,
            &self.dom_tree,
            &mut uses,
        );

        uses
    }

    fn insert_phis(&mut self, phis: PhiPositions) {
        let mut instrs = IndexVec::with_capacity(self.instrs_count() + Self::phis_count(&phis));
        std::mem::swap(&mut instrs, &mut self.function.instrs);

        let mut instrs = instrs.into_inner().into_iter();

        self.instrs_mut().push(instrs.next().unwrap());

        for (block, phis) in phis.into_iter() {
            if !block.is_entry() {
                self.instrs_mut().push(Instr::Label(block));
            }
            self.instrs_mut()
                .extend(phis.into_iter().map(|(local, args, _)| {
                    Instr::AssignPhi(
                        IrId::Local(local),
                        args.into_vec()
                            .into_iter()
                            .map(|(val, block)| (Value::Id(IrId::Local(val)), block))
                            .collect(),
                    )
                }));
            let block_len = self.blocks()[block].end.index() - self.blocks()[block].start.index();

            self.instrs_mut().extend(
                instrs
                    .by_ref()
                    .skip_while(|instr| matches!(instr, Instr::Label(_)))
                    .take(block_len - 1),
            );
        }

        self.update_blocks();
    }

    fn remove_nops(&mut self) {
        self.function.instrs.retain(|instr| !instr.is_nop());
        self.function.instrs.shrink_to_fit();
        self.update_blocks();
    }

    fn update_blocks(&mut self) {
        let mut cur_block = BlockId::ENTRY;
        let mut cur_start = InstrId(0);
        let mut blocks = IndexVec::with_capacity(self.blocks_count());

        for (id, instr) in self.instrs() {
            match instr {
                Instr::Label(block) => {
                    let data = BlockData {
                        start: cur_start,
                        end:   id,
                        id:    cur_block,
                    };

                    blocks.push(data);

                    cur_block = *block;
                    cur_start = id;
                }
                Instr::Return(_) => {
                    let data = BlockData {
                        start: cur_start,
                        end:   id.plus(1),
                        id:    cur_block,
                    };

                    blocks.push(data);
                }
                _ => {}
            }
        }

        self.function.blocks = blocks;
    }

    fn mem2reg(&mut self, preds: &Predecessors, phis: PhiPositions) -> FxHashMap<IrId, VarUses> {
        self.insert_phis(phis);
        self.rename_ids(preds)
    }

    fn dead_code_elimination(&mut self, uses: &mut FxHashMap<IrId, VarUses>) -> bool {
        let mut res = self.remove_dead_blocks();

        while let Some(id) = uses.iter().find_map(|(id, VarUses { decl, uses })| {
            if uses.is_empty() && !self.instrs()[*decl].is_function() {
                Some(*id)
            } else {
                None
            }
        }) {
            res = true;
            let VarUses { decl, .. } = uses.remove(&id).unwrap();
            self.instrs_mut()[decl].replace_with_nop();
            for uses in uses.values_mut() {
                uses.uses.remove(&decl);
            }
        }

        res
    }

    fn const_propagation(&mut self, uses: &mut FxHashMap<IrId, VarUses>) -> bool {
        let mut values = FxHashMap::default();
        let mut work_list = (0..self.instrs().len())
            .map(InstrId)
            .collect::<VecDeque<_>>();
        let mut changed = false;

        while let Some(id) = work_list.pop_front() {
            let instr = &mut self.instrs_mut()[id];
            if let Some(uses) = instr.const_fold(&mut values, uses) {
                changed = true;
                work_list.extend(uses.uses);
            }
        }

        changed
    }
}

fn update_var_use_decl(uses: &mut FxHashMap<IrId, VarUses>, id: IrId, instr_id: InstrId) {
    uses.entry(id)
        .or_insert_with(|| VarUses::new(instr_id))
        .decl = instr_id;
}

fn update_var_uses(uses: &mut FxHashMap<IrId, VarUses>, id: &IrId, instr_id: InstrId) {
    uses.get_mut(id).unwrap().uses.insert(instr_id);
}

impl Instr {
    pub fn rename(
        &mut self,
        instr_id: InstrId,
        renames: &mut FxHashMap<IrId, Vec<IrId>>,
        tmp: &mut IrId,
        uses: &mut FxHashMap<IrId, VarUses>,
    ) -> Option<IrId> {
        match self {
            Self::Nop | Self::Vtable(_, _) | Self::Label(_) | Self::Jmp(_) => {}

            Self::Function { id, params, .. } => {
                for (_, param) in params.iter_mut() {
                    let new_id = tmp.next();
                    let old_id = *param;
                    renames.entry(old_id).or_default().push(new_id);
                    *param = new_id;
                    update_var_use_decl(uses, new_id, instr_id);
                }
                update_var_use_decl(uses, IrId::Global(*id), instr_id);
            }

            Self::Local(_, _) => {
                *self = Self::Nop;
            }

            Self::JmpCond { src: id, .. } => {
                let cur_id = renames.get(id).unwrap().last().unwrap();
                *id = *cur_id;
                update_var_uses(uses, cur_id, instr_id);
            }

            Self::Return(val) => {
                if let Value::Id(id) = val {
                    let cur_id = renames.get(id).unwrap().last().unwrap();
                    *id = *cur_id;
                    update_var_uses(uses, cur_id, instr_id);
                }
            }

            Self::Store { dst, src, .. } if dst.is_global() => {
                if let Value::Id(id) = src {
                    let cur_id = renames.get(id).unwrap().last().unwrap();
                    *id = *cur_id;
                    update_var_uses(uses, cur_id, instr_id);
                }
            }

            Self::Store { dst, src, .. } if dst.is_ptr() => {
                let cur_id1 = renames.get(dst).unwrap().last().unwrap();
                *dst = *cur_id1;
                update_var_uses(uses, cur_id1, instr_id);
                if let Value::Id(id2) = src {
                    let cur_id2 = renames.get(id2).unwrap().last().unwrap();
                    *id2 = *cur_id2;
                    update_var_uses(uses, cur_id2, instr_id);
                }
            }

            Self::Store { dst, src, .. } => match src {
                Value::Id(id) => {
                    let cur_id = *renames.get(id).unwrap().last().unwrap();
                    let new_id = tmp.next();
                    let old_id = *dst;
                    renames.entry(old_id).or_default().push(new_id);
                    *self = Self::Assign(new_id, Value::Id(cur_id));
                    update_var_uses(uses, &cur_id, instr_id);
                    update_var_use_decl(uses, new_id, instr_id);
                    return Some(old_id);
                }
                val => {
                    let new_id = tmp.next();
                    let old_id = *dst;
                    renames.entry(old_id).or_default().push(new_id);
                    let val = std::mem::take(val);
                    *self = Self::Assign(new_id, val);
                    update_var_use_decl(uses, new_id, instr_id);
                    return Some(old_id);
                }
            },

            Self::AssignLoad { dst, src, .. } if src.is_global() => {
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }

            Self::AssignLoad { dst, src, .. } if src.is_local() => {
                let cur_id2 = *renames.get(src).unwrap().last().unwrap();
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *self = Self::Assign(new_id, Value::Id(cur_id2));
                update_var_uses(uses, &cur_id2, instr_id);
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }

            Self::AssignLoad { dst, src, .. } => {
                let cur_id2 = renames.get(src).unwrap().last().unwrap();
                *src = *cur_id2;
                update_var_uses(uses, cur_id2, instr_id);
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }

            Self::AssignInsert {
                dst,
                src,
                val: Value::Id(val),
                ..
            } if !val.is_global() => {
                let cur_src = renames.get(src).unwrap().last().unwrap();
                *src = *cur_src;
                update_var_uses(uses, cur_src, instr_id);
                let cur_val = renames.get(val).unwrap().last().unwrap();
                *val = *cur_val;
                update_var_uses(uses, cur_val, instr_id);
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                update_var_use_decl(uses, new_id, instr_id);
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
                update_var_uses(uses, cur_lhs, instr_id);
                let cur_rhs = renames.get(rhs).unwrap().last().unwrap();
                *rhs = *cur_rhs;
                update_var_uses(uses, cur_rhs, instr_id);
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }

            Self::Assign(dst, Value::Id(src))
            | Self::AssignUn { dst, src, .. }
            | Self::AssignBin {
                dst,
                lhs: Value::Id(src),
                ..
            }
            | Self::AssignInsert { dst, src, .. }
            | Self::AssignBin {
                dst,
                rhs: Value::Id(src),
                ..
            }
            | Self::AssignExtract { dst, src, .. }
            | Self::AssignGep { dst, src, .. } => {
                let cur_id = renames.get(src).unwrap().last().unwrap();
                *src = *cur_id;
                update_var_uses(uses, cur_id, instr_id);
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }

            Self::AssignCall(id1, _, id2, args) => {
                if !id2.is_global() {
                    let cur_id2 = renames.get(id2).unwrap().last().unwrap();
                    *id2 = *cur_id2;
                    update_var_uses(uses, cur_id2, instr_id);
                }
                for id in args.iter_mut().filter_map(|(_, val)| match val {
                    Value::Id(id) => Some(id),
                    _ => None,
                }) {
                    let cur_id = renames.get(id).unwrap().last().unwrap();
                    *id = *cur_id;
                    update_var_uses(uses, cur_id, instr_id);
                }
                let new_id = tmp.next();
                let old_id = *id1;
                renames.entry(old_id).or_default().push(new_id);
                *id1 = new_id;
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }

            Self::Assign(dst, _) | Self::AssignBin { dst, .. } | Self::AssignPhi(dst, _) => {
                let new_id = tmp.next();
                let old_id = *dst;
                renames.entry(old_id).or_default().push(new_id);
                *dst = new_id;
                update_var_use_decl(uses, new_id, instr_id);
                return Some(old_id);
            }
        }

        None
    }

    fn replace_with_nop(&mut self) {
        *self = Self::Nop;
    }

    #[inline]
    fn const_fold(
        &mut self,
        values: &mut FxHashMap<IrId, Value>,
        uses: &mut FxHashMap<IrId, VarUses>,
    ) -> Option<VarUses> {
        match self {
            Instr::Assign(id, val) => {
                let mut val = std::mem::take(val);
                Self::try_replace(values, &mut val);
                let folded = uses.remove(id).unwrap();
                if let Value::Id(ref id) = val {
                    let uses_mut = uses.get_mut(id).unwrap();
                    uses_mut.uses.extend(folded.uses.iter());
                    uses_mut.uses.remove(&folded.decl);
                }
                values.insert(*id, val);
                self.replace_with_nop();
                Some(folded)
            }
            Instr::AssignUn { op, dst, src, .. } => {
                let op = *op;
                let dst = *dst;
                Self::try_replace_id(values, src);
                if Self::const_fold_un(values, op, dst, src) {
                    let folded = uses.remove(&dst).unwrap();
                    uses.get_mut(src).unwrap().uses.remove(&folded.decl);
                    self.replace_with_nop();
                    Some(folded)
                } else {
                    None
                }
            }
            Instr::AssignBin { op, dst, lhs, rhs } => {
                let op = *op;
                let dst = *dst;
                Self::try_replace(values, lhs);
                Self::try_replace(values, rhs);
                if Self::const_fold_bin(values, op, dst, lhs, rhs) {
                    let folded = uses.remove(&dst).unwrap();
                    if let Value::Id(lhs) = lhs {
                        uses.get_mut(lhs).unwrap().uses.remove(&folded.decl);
                    }
                    if let Value::Id(rhs) = rhs {
                        uses.get_mut(rhs).unwrap().uses.remove(&folded.decl);
                    }
                    self.replace_with_nop();
                    Some(folded)
                } else {
                    None
                }
            }

            Instr::JmpCond {
                src,
                on_true,
                on_false,
            } => {
                Self::try_replace_id(values, src);
                let id = *src;
                let on_true = *on_true;
                let on_false = *on_false;
                self.const_fold_jmp_cond(values, id, on_true, on_false);
                None
            }

            Instr::AssignCall(_, _, id, args) => {
                for arg in args.iter_mut().map(|(_, id)| id) {
                    Self::try_replace(values, arg);
                }
                Self::try_replace_id(values, id);
                None
            }

            Instr::AssignExtract { src, .. } | Instr::AssignGep { src, .. } => {
                Self::try_replace_id(values, src);
                None
            }
            Instr::AssignInsert { val, .. } => {
                Self::try_replace(values, val);
                None
            }
            Instr::AssignPhi(_, vals) => {
                for (val, _) in vals.iter_mut() {
                    Self::try_replace(values, val);
                }
                None
            }
            Instr::Return(id) => {
                Self::try_replace(values, id);
                None
            }
            Instr::Store { src, .. } => {
                Self::try_replace(values, src);
                None
            }
            Instr::AssignLoad { src, .. } => {
                Self::try_replace_id(values, src);
                None
            }

            Instr::Local(_, _)
            | Instr::Jmp(_)
            | Instr::Function { .. }
            | Instr::Label(_)
            | Instr::Nop
            | Instr::Vtable(_, _) => None,
        }
    }

    #[inline(always)]
    fn try_replace(values: &FxHashMap<IrId, Value>, val: &mut Value) {
        while let Value::Id(id) = val {
            match values.get(id) {
                Some(new_val) => *val = new_val.clone(),
                None => break,
            }
        }
    }

    #[inline(always)]
    fn try_replace_id(values: &FxHashMap<IrId, Value>, id: &mut IrId) {
        while let Some(Value::Id(val)) = values.get(id) {
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
                *self = Instr::Jmp(on_true);
            } else {
                *self = Instr::Jmp(on_false);
            }
        }
    }

    fn const_fold_un(values: &mut FxHashMap<IrId, Value>, op: UnOp, id: IrId, arg: &IrId) -> bool {
        match op {
            UnOp::IsVoid => match values.get(arg) {
                Some(Value::Int(_) | Value::Bool(_)) => {
                    values.insert(id, Value::Bool(false));
                    true
                }
                Some(Value::Void) => {
                    values.insert(id, Value::Bool(true));
                    true
                }
                _ => false,
            },

            UnOp::Complement => match values.get(arg) {
                Some(Value::Int(arg)) => {
                    let val = -arg;
                    values.insert(id, Value::Int(val));
                    true
                }
                _ => false,
            },

            UnOp::Not => match values.get(arg) {
                Some(Value::Bool(arg)) => {
                    let val = !arg;
                    values.insert(id, Value::Bool(val));
                    true
                }
                _ => false,
            },
        }
    }

    fn const_fold_bin(
        values: &mut FxHashMap<IrId, Value>,
        op: BinOp,
        id: IrId,
        lhs: &Value,
        rhs: &Value,
    ) -> bool {
        match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => {
                match op {
                    BinOp::Add => {
                        let val = lhs + rhs;
                        values.insert(id, Value::Int(val));
                    }
                    BinOp::Sub => {
                        let val = lhs - rhs;
                        values.insert(id, Value::Int(val));
                    }
                    BinOp::Mul => {
                        let val = lhs * rhs;
                        values.insert(id, Value::Int(val));
                    }
                    BinOp::Div => {
                        let val = lhs / rhs;
                        values.insert(id, Value::Int(val));
                    }
                    BinOp::Lt => {
                        let val = lhs < rhs;
                        values.insert(id, Value::Bool(val));
                    }
                    BinOp::Le => {
                        let val = lhs <= rhs;
                        values.insert(id, Value::Bool(val));
                    }
                    BinOp::Eq => {
                        let val = lhs == rhs;
                        values.insert(id, Value::Bool(val));
                    }
                }
                true
            }
            _ => false,
        }
    }
}

pub struct IrOptmizer {
    pub functions: Vec<FunctionOptmizer>,
    pub vtables:   Vec<Instr>,
    pub globals:   IndexVec<GlobalId, Rc<str>>,
    pub strings:   FxHashMap<GlobalId, Rc<str>>,
}

impl IrOptmizer {
    pub fn from_builder(builder: IrBuilder) -> Self {
        let mut functions = vec![];
        let empty_string = builder.strings().get_key_value("").unwrap().0.clone();
        let mut globals = index_vec![empty_string; builder.globals.len()];
        let mut vtables = vec![];
        let mut strings = FxHashMap::default();

        for (id, global) in builder.globals.into_iter() {
            match global.value {
                Some(GlobalValue::Vtable(kind)) => vtables.push(kind),
                Some(GlobalValue::Function(function)) => {
                    functions.push(FunctionOptmizer::from_builder(function))
                }
                Some(GlobalValue::String(string)) => {
                    strings.insert(id, string);
                }
                None => {}
            }
            globals[id] = global.name;
        }

        Self {
            functions,
            vtables,
            globals,
            strings,
        }
    }

    pub fn globals(&self) -> &IndexVec<GlobalId, Rc<str>> {
        &self.globals
    }

    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut FunctionOptmizer> {
        self.functions.iter_mut()
    }

    pub fn optimize(&mut self) {
        for function in self.functions.iter_mut() {
            function.optimize();
        }
    }

    fn instrs_with_nops(&self) -> impl Iterator<Item = &Instr> {
        self.vtables.iter().chain(
            self.functions
                .iter()
                .flat_map(|m| m.instrs().inner().iter()),
        )
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr> {
        self.instrs_with_nops().filter(|i| !i.is_nop())
    }
}
