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
        write!(f, "b{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id:     BlockId,
    pub instrs: Vec<Instr>,
    pub preds:  Vec<BlockId>,
    pub succs:  Vec<BlockId>,
}

impl Block {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            instrs: Vec::new(),
            preds: Vec::new(),
            succs: Vec::new(),
        }
    }

    pub fn push(&mut self, instr: Instr) {
        self.instrs.push(instr)
    }
}
