use core::panic;
use std::rc::Rc;

use crate::{
    ast::{BinOp, UnOp},
    index_vec::Key,
    ir_builder::BlockId,
    span::Span,
    types::TypeId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Void,
    Id(Id),
    Int(i64),
    Bool(bool),
    Str(Rc<str>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrKind<'a> {
    Nop,

    Method(TypeId, &'a str, TypeId, usize),
    Param(TypeId, Id),
    Return(Id),
    Label(BlockId),
    Jmp(BlockId),
    /// (src, on_true, on_false)
    JmpCond(Id, BlockId, BlockId),
    Assign(Id, Id),
    AssignDefault(Id, TypeId),
    AssignInt(Id, i64),
    AssignBool(Id, bool),
    AssignStr(Id, Rc<str>),
    AssignBin(BinOp, Id, Id, Id),
    AssignUn(UnOp, Id, Id),
    AssignToObj(Id, TypeId, Id),
    AssignDispatch(Id, Id, &'a str, Box<[(TypeId, Id)]>),
    AssignStaticDispatch(Id, Id, TypeId, &'a str, Box<[(TypeId, Id)]>),
    Phi(Id, Box<[Id]>),

    // before mem2reg
    Local(TypeId, Id),
    AssignLoad(Id, TypeId, Id),
    Store(Id, TypeId, Id),
    Attr(TypeId, Id),
}

impl<'a> InstrKind<'a> {
    pub fn is_block_end(&self) -> bool {
        matches!(
            self,
            Self::Return(_) | Self::Jmp(_) | Self::JmpCond(_, _, _)
        )
    }

    pub fn is_nop(&self) -> bool {
        matches!(self, Self::Nop)
    }

    pub fn uses(&self) -> (Option<Id>, Option<Box<[Id]>>) {
        match self {
            Self::Nop | Self::Method(_, _, _, _) | Self::Label(_) | Self::Jmp(_) => (None, None),

            Self::Param(_, id)
            | Self::AssignDefault(id, _)
            | Self::AssignInt(id, _)
            | Self::AssignBool(id, _)
            | Self::AssignStr(id, _)
            | Self::Local(_, id)
            | Self::Attr(_, id) => (Some(*id), None),

            Self::JmpCond(id, _, _) | Self::Return(id) => (None, Some([*id].into())),

            Self::Assign(id1, id2)
            | Self::AssignUn(_, id1, id2)
            | Self::AssignToObj(id1, _, id2)
            | Self::Store(id1, _, id2)
            | Self::AssignLoad(id1, _, id2) => (Some(*id1), Some([*id2].into())),

            Self::AssignBin(_, id, lhs, rhs) => (Some(*id), Some([*lhs, *rhs].into())),

            Self::AssignDispatch(id1, id2, _, args)
            | Self::AssignStaticDispatch(id1, id2, _, _, args) => (
                Some(*id1),
                Some(
                    args.iter()
                        .map(|(_, id)| *id)
                        .fold(vec![*id2], |mut vars, id| {
                            vars.push(id);
                            vars
                        })
                        .into(),
                ),
            ),

            Self::Phi(id, vals) => (Some(*id), Some(vals.clone())),
        }
    }
}

impl<'a> std::fmt::Display for InstrKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrKind::Nop => write!(f, "    nop"),
            InstrKind::Method(ty, name, ret_ty, arity) => {
                write!(f, "method {} {}.{} {} {{", ret_ty, ty, name, arity)
            }
            InstrKind::Param(ty, id) => write!(f, "    param {} {}", ty, id),
            InstrKind::Return(id) => write!(f, "    ret {}\n}}\n", id),
            InstrKind::Label(subs) => write!(f, "block{}:", subs),
            InstrKind::Jmp(subs) => write!(f, "    jmp block{}", subs),
            InstrKind::JmpCond(id, on_true, on_false) => {
                write!(f, "    {} ? block{} : block{}", id, on_true, on_false)
            }
            InstrKind::Assign(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignDefault(id, ty) => write!(f, "    {} = default {}", id, ty),
            InstrKind::AssignInt(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignBool(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignStr(id, val) => write!(f, "    {} = {:?}", id, val),
            InstrKind::AssignBin(op, id, lhs, rhs) => {
                write!(f, "    {} = {} {}, {}", id, op.to_ir_string(), lhs, rhs)
            }
            InstrKind::AssignUn(op, id, val) => {
                write!(f, "    {} = {} {}", id, op.to_ir_string(), val)
            }
            InstrKind::AssignDispatch(id, obj, name, args) => {
                write!(f, "    {} = dispatch {}.{}(", id, obj, name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{} {}", arg.0, arg.1)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            InstrKind::AssignStaticDispatch(id, obj, ty, name, args) => {
                write!(f, "    {} = dispatch {} @ {}.{}(", id, obj, ty, name).unwrap();
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{} {}", arg.0, arg.1)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            InstrKind::AssignToObj(id, ty, val) => {
                write!(f, "    {} = cast {}, {}", id, ty, val)
            }
            InstrKind::Phi(id, vals) => {
                write!(f, "    {} = phi ", id)?;
                for (i, val) in vals.iter().enumerate() {
                    write!(f, "{}", val)?;
                    if i != vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
            InstrKind::Local(ty, name) => write!(f, "    local {} {}", ty, name),
            InstrKind::AssignLoad(id, ty, name) => write!(f, "    {} = load {} {}", id, ty, name),
            InstrKind::Store(name, ty, id) => write!(f, "    store {}, {} {}", name, ty, id),
            InstrKind::Attr(ty, name) => write!(f, "    attr {} {}", ty, name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instr<'a> {
    pub kind: InstrKind<'a>,
    pub span: Span,
    pub ty:   TypeId,
}

impl<'a> Instr<'a> {
    pub fn new(kind: InstrKind<'a>, span: Span, ty: TypeId) -> Self {
        Self { kind, span, ty }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Tmp(usize),
    Local(usize),
    Renamed(usize),
    Ptr(usize),
}

impl Id {
    pub const LOCAL_SELF: Self = Self::Local(0);

    pub fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
    }

    pub fn is_tmp(&self) -> bool {
        matches!(self, Self::Tmp(_))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Self::Ptr(_))
    }

    pub fn is_renamed(&self) -> bool {
        matches!(self, Self::Renamed(_))
    }

    pub fn get_subs(&self) -> usize {
        match self {
            Self::Tmp(subs) | Self::Local(subs) | Self::Renamed(subs) | Self::Ptr(subs) => *subs,
        }
    }

    pub fn new(&self) -> Self {
        match self {
            Self::Tmp(index) => Self::Tmp(index + 1),
            Self::Local(index) => Self::Local(index + 1),
            Self::Renamed(index) => Self::Renamed(index + 1),
            Self::Ptr(index) => Self::Ptr(index + 1),
        }
    }

    pub fn new_mut(&mut self) -> Self {
        *self = self.new();
        *self
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tmp(subs) => write!(f, "$t{}", subs),
            Self::Local(subs) => write!(f, "$l{}", subs),
            Self::Renamed(subs) => write!(f, "$r{}", subs),
            Self::Ptr(subs) => write!(f, "$p{}", subs),
        }
    }
}

impl Key for Id {
    fn to_index(self) -> usize {
        match self {
            Self::Ptr(id) | Self::Local(id) => id,
            _ => panic!("cannot index this id"),
        }
    }

    fn from_index(index: usize) -> Self {
        Self::Local(index)
    }
}
