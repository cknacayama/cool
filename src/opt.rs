use crate::{
    index_vec::{index_vec, IndexVec, Key},
    ir::{Id, Instr, InstrKind},
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

    fn contains_store(&self, block: BlockId, local: Id) -> bool {
        let block = &self.blocks[block];

        block
            .instrs
            .iter()
            .any(|instr| matches!(instr.kind, InstrKind::Store(id, _, _) if id == local))
    }

    fn find_decl(&self, local: Id) -> Option<(BlockId, &Block<'a>)> {
        self.blocks.iter().find(|(_, block)| {
            block
                .instrs
                .iter()
                .any(|instr| matches!(instr.kind, InstrKind::Local(_, id) if id == local))
        })
    }

    fn all_stores(&self) -> impl Iterator<Item = (Id, &Block<'a>, Vec<BlockId>)> + '_ {
        self.local_ids().map(|local| {
            (
                local,
                self.find_decl(local).unwrap().1,
                self.block_ids()
                    .filter(|b| self.contains_store(*b, local))
                    .collect(),
            )
        })
    }

    fn phi_positions(
        &self,
        preds: &Predecessors,
        dom_frontiers: &DominanceFrontiers,
    ) -> PhiPositions {
        let mut phi_positions: PhiPositions = index_vec![vec![]; self.blocks.len()];
        let stores = self.all_stores();

        for (local, decl_block, mut stores) in stores {
            let mut visited = index_vec![false; self.blocks.len()];
            while let Some(block) = stores.pop() {
                for frontier in dom_frontiers[block]
                    .iter()
                    .filter(|&&b| decl_block.id() <= b)
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

    fn rename_locals(&mut self, phis: &mut PhiPositions, dom_tree: &DominatorTree) {
        fn rename_block(
            method: &mut Method<'_>,
            cur_tmp: &mut Id,
            block: BlockId,
            renames: &mut IndexVec<Id, Option<Id>>,
            phis: &mut PhiPositions,
            dom_tree: &DominatorTree,
        ) {
            for instr in method.blocks[block].instrs_mut() {
                match &instr.kind {
                    InstrKind::Local(_, _) => {
                        instr.kind = InstrKind::Nop;
                    }
                    InstrKind::AssignLoad(tmp, _, id) if id.is_local() => {
                        let new_id = renames[*id].unwrap();
                        instr.kind = InstrKind::Assign(*tmp, new_id);
                    }
                    InstrKind::Store(id, _, tmp) if id.is_local() => {
                        let new_id = cur_tmp.new_mut();
                        renames[*id] = Some(new_id);
                        instr.kind = InstrKind::Assign(new_id, *tmp);
                    }

                    _ => {}
                }
            }

            method.successors_visit(block, |s| {
                for (l, args, _) in phis[s].iter_mut() {
                    let Some(to_rename) = args.iter_mut().find(|arg| arg.is_local()) else {
                        let tmp = cur_tmp.new_mut();
                        renames[*l] = Some(tmp);
                        *l = tmp;
                        return;
                    };
                    *to_rename = renames[*to_rename].unwrap();
                }
            });

            for child in dom_tree[block].iter().copied() {
                rename_block(method, cur_tmp, child, renames, phis, dom_tree);
            }
        }

        let mut renames: IndexVec<Id, Option<Id>> = index_vec![None; self.locals().len()];
        let mut cur_tmp = Id::Renamed(0);
        rename_block(
            self,
            &mut cur_tmp,
            BlockId::ENTRY,
            &mut renames,
            phis,
            dom_tree,
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
        let dom_front = self.dom_frontiers(&preds, &idoms);
        let dom_tree = self.dom_tree(&idoms);
        let mut phis = self.phi_positions(&preds, &dom_front);
        self.rename_locals(&mut phis, &dom_tree);
        self.insert_phis(phis);
        self.set_labels();
    }
}
