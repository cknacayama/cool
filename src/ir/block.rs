use std::collections::VecDeque;

use crate::{index_vec::Key, types::TypeId};

use super::{Instr, InstrKind};

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

impl Key for BlockId {
    fn to_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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

    pub fn push_back(&mut self, kind: InstrKind, ty: TypeId) {
        self.instrs.push_back(Instr::new(kind, ty))
    }

    pub fn set_label(&mut self) {
        if self.id.is_entry() {
            return;
        }
        let kind = InstrKind::Label(self.id);
        let instr = Instr::new(kind, TypeId::SelfType);
        self.push_front(instr)
    }
}
