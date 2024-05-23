use std::collections::VecDeque;

use crate::{
    index_vec::Key,
    ir::{Instr, InstrKind, Value},
    span::Span,
    types::TypeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub u32);

impl BlockId {
    pub const ENTRY: BlockId = BlockId(0);

    pub fn is_entry(self) -> bool {
        self == Self::ENTRY
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block<'a> {
    pub id:     BlockId,
    pub scope:  u32,
    pub instrs: VecDeque<Instr<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(id: BlockId, scope: u32) -> Self {
        Self {
            id,
            scope,
            instrs: VecDeque::new(),
        }
    }

    pub fn can_merge(&self) -> bool {
        !self.instrs.iter().any(|i| {
            !matches!(
                i.kind,
                InstrKind::Jmp(_) | InstrKind::Label(_) | InstrKind::Nop
            )
        })
    }

    pub fn id(&self) -> BlockId {
        self.id
    }

    pub fn push_front(&mut self, instr: Instr<'a>) {
        self.instrs.push_front(instr)
    }

    pub fn push_back(&mut self, kind: InstrKind<'a>, span: Span, ty: TypeId) {
        self.instrs.push_back(Instr::new(kind, span, ty, self.id))
    }

    pub fn instrs(&self) -> impl Iterator<Item = &Instr<'a>> {
        self.instrs.iter()
    }

    pub fn remove_nops(&mut self) {
        self.instrs.retain(|i| !i.kind.is_nop());
        self.instrs.shrink_to_fit();
    }

    pub fn set_label(&mut self) {
        if self.id.is_entry() {
            return;
        }
        let kind = InstrKind::Label(self.id);
        let instr = Instr::new(kind, Span::default(), TypeId::SelfType, self.id);
        self.push_front(instr)
    }

    pub fn instrs_mut(&mut self) -> impl Iterator<Item = &mut Instr<'a>> {
        self.instrs.iter_mut()
    }

    pub fn phis_args(&mut self) -> Vec<&mut Vec<(Value, BlockId)>> {
        let mut phis = vec![];
        for instr in self.instrs.iter_mut() {
            if let InstrKind::Phi(_, args) = &mut instr.kind {
                phis.push(args);
            }
        }
        phis
    }
}
