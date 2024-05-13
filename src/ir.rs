use std::borrow::Cow;

use crate::{
    ast::{BinOp, UnOp},
    index_vec::Key,
    ir_builder::BlockId,
    span::Span,
    types::TypeId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrKind<'a> {
    Method(&'a str, &'a str),
    BeginMethod(usize),
    EndMethod(Id),
    Label(BlockId),
    Jmp(BlockId),
    // (src, on_zero, on_non_zero)
    JmpIfZero(Id, BlockId, BlockId),
    Id(Id),
    Assign(Id, Id),
    AssignDefault(Id),
    AssignNew(Id, TypeId),
    AssignInt(Id, i64),
    AssignBool(Id, bool),
    AssignStr(Id, Cow<'a, str>),
    AssignBin(BinOp, Id, Id, Id),
    AssignUn(UnOp, Id, Id),
    AssignToObj(Id, TypeId, Id),
    AssignDispatch(Id, Id, &'a str, usize),
    AssignStaticDispatch(Id, Id, TypeId, &'a str, usize),
    Phi(Id, Vec<(Id, BlockId)>),
    PushArg(Id),
    PopArgs(usize),

    // before mem2reg
    Local(Id, usize),
    AssignLoad(Id, Id),
    Store(Id, Id),
}

impl<'a> InstrKind<'a> {
    pub fn block_end(&self) -> bool {
        matches!(
            self,
            Self::EndMethod(_) | Self::Jmp(_) | Self::JmpIfZero(_, _, _)
        )
    }
}

impl<'a> std::fmt::Display for InstrKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrKind::Method(ty, name) => write!(f, "{}.{}:", ty, name),
            InstrKind::BeginMethod(subs) => write!(f, "    begin_method {}", subs),
            InstrKind::EndMethod(id) => write!(f, "    end_method {}", id),
            InstrKind::Label(subs) => write!(f, "block{}:", subs),
            InstrKind::Jmp(subs) => write!(f, "    jmp block{}", subs),
            InstrKind::JmpIfZero(id, on_zero, on_non_zero) => {
                write!(f, "    {} ? block{} : block{}", id, on_non_zero, on_zero)
            }
            InstrKind::Id(id) => write!(f, "{}", id),
            InstrKind::Assign(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignDefault(id) => write!(f, "    {} = default", id),
            InstrKind::AssignNew(id, ty) => write!(f, "    {} = new {}", id, ty),
            InstrKind::AssignInt(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignBool(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignStr(id, val) => write!(f, "    {} = {:?}", id, val),
            InstrKind::AssignBin(op, id, lhs, rhs) => {
                write!(f, "    {} = {} {}, {}", id, op.to_ir_string(), lhs, rhs)
            }
            InstrKind::AssignUn(op, id, val) => {
                write!(f, "    {} = {} {}", id, op.to_ir_string(), val)
            }
            InstrKind::AssignDispatch(id, obj, name, arg_count) => {
                write!(f, "    {} = dispatch {}, {}.{}()", id, arg_count, obj, name)
            }
            InstrKind::AssignStaticDispatch(id, obj, ty, name, arg_count) => {
                write!(
                    f,
                    "    {} = dispatch {}, {}@{}.{}()",
                    arg_count, id, obj, ty, name
                )
            }
            InstrKind::AssignToObj(id, ty, val) => write!(f, "    {} = cast {}, {}", id, ty, val),
            InstrKind::Phi(id, vals) => {
                write!(f, "    {} = phi (", id)?;
                for (i, val) in vals.iter().enumerate() {
                    write!(f, "{:?}", val)?;
                    if i != vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            InstrKind::PushArg(id) => write!(f, "    push_arg {}", id),
            InstrKind::PopArgs(subs) => write!(f, "    pop_args {}", subs),
            InstrKind::Local(name, size) => write!(f, "    local {}, {}", name, size),
            InstrKind::AssignLoad(id, name) => write!(f, "    {} = load {}", id, name),
            InstrKind::Store(name, id) => write!(f, "    store {}, {}", name, id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instr<'a> {
    pub kind: InstrKind<'a>,
    pub span: Span,
    pub ty:   Option<TypeId>,
}

impl<'a> Instr<'a> {
    pub fn new(kind: InstrKind<'a>, span: Span, ty: Option<TypeId>) -> Self {
        Self { kind, span, ty }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Tmp(usize),
    Local(usize),
}

impl Id {
    pub const LOCAL_SELF: Self = Self::Local(0);

    pub fn get_subs(&self) -> usize {
        match self {
            Self::Tmp(subs) | Self::Local(subs) => *subs,
        }
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tmp(subs) => write!(f, "t{}", subs),
            Self::Local(subs) => write!(f, "l{}", subs),
        }
    }
}

impl Key for Id {
    fn to_index(self) -> usize {
        self.get_subs()
    }
}
