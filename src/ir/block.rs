use std::collections::VecDeque;

use crate::index_vec::Idx;

use super::Instr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub u32);

impl BlockId {
    pub const ENTRY: BlockId = BlockId(0);

    pub fn is_entry(self) -> bool {
        self == Self::ENTRY
    }

    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }

    pub fn prev(self) -> Option<Self> {
        if self.is_entry() {
            None
        } else {
            Some(Self(self.0 - 1))
        }
    }
}

impl Idx for BlockId {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn new(index: usize) -> Self {
        Self(index as u32)
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "block{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id:     BlockId,
    pub instrs: VecDeque<Instr>,
    pub preds:  Vec<BlockId>,
    pub succs:  Vec<BlockId>,
}

impl Block {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            instrs: VecDeque::new(),
            preds: Vec::new(),
            succs: Vec::new(),
        }
    }

    pub fn push_front(&mut self, instr: Instr) {
        self.instrs.push_front(instr)
    }

    pub fn push_back(&mut self, kind: Instr) {
        self.instrs.push_back(kind)
    }

    pub fn set_label(&mut self) {
        if self.id.is_entry() {
            return;
        }
        let instr = Instr::Label(self.id);
        self.push_front(instr)
    }
}
