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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Id(id) => write!(f, "{}", id),
            Value::Int(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Str(val) => write!(f, "{:?}", val),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrKind<'a> {
    Nop,

    Method(TypeId, &'a str, TypeId, usize),
    Param(TypeId, Id),
    Return(Value),
    Label(BlockId),
    Jmp(BlockId),
    /// (src, on_true, on_false)
    JmpCond(Id, BlockId, BlockId),
    Assign(Id, Value),
    AssignDefault(Id, TypeId),
    AssignBin(BinOp, Id, Value, Value),
    AssignUn(UnOp, Id, Id),
    AssignToObj(Id, TypeId, Value),
    AssignCall(Id, Id, Box<[(TypeId, Value)]>),
    AssignStaticCall(Id, TypeId, &'a str, Box<[(TypeId, Value)]>),
    AssignExtract(Id, Id, usize),
    Phi(Id, Vec<(Value, BlockId)>),

    // before mem2reg
    Local(TypeId, Id),
    AssignLoad(Id, TypeId, Id, usize),
    Store(Id, TypeId, Value),
    Attr(TypeId, Id),
}

impl<'a> InstrKind<'a> {
    pub fn is_block_end(&self) -> bool {
        matches!(
            self,
            Self::Return(_) | Self::Jmp(_) | Self::JmpCond(_, _, _)
        )
    }

    pub fn is_call(&self) -> bool {
        matches!(
            self,
            Self::AssignCall(_, _, _) | Self::AssignStaticCall(_, _, _, _)
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
            | Self::Local(_, id)
            | Self::Attr(_, id) => (Some(*id), None),

            Self::JmpCond(id, _, _) => (None, Some([*id].into())),
            Self::Return(val) => {
                if let Value::Id(id) = val {
                    (None, Some([*id].into()))
                } else {
                    (None, None)
                }
            }

            Self::Store(_, _, val) => {
                if let Value::Id(id) = val {
                    (None, Some([*id].into()))
                } else {
                    (None, None)
                }
            }

            Self::AssignBin(_, id, Value::Id(lhs), Value::Id(rhs)) => {
                (Some(*id), Some([*lhs, *rhs].into()))
            }

            Self::AssignBin(_, id1, _, Value::Id(id2))
            | Self::AssignBin(_, id1, Value::Id(id2), _)
            | Self::Assign(id1, Value::Id(id2))
            | Self::AssignUn(_, id1, id2)
            | Self::AssignToObj(id1, _, Value::Id(id2))
            | Self::AssignLoad(id1, _, id2, _)
            | Self::AssignExtract(id1, id2, _) => (Some(*id1), Some([*id2].into())),

            Self::Assign(id1, _) | Self::AssignToObj(id1, _, _) | Self::AssignBin(_, id1, _, _) => {
                (Some(*id1), None)
            }

            Self::AssignCall(id1, id2, args) => {
                let used_ids = args
                    .iter()
                    .filter_map(|(_, val)| match val {
                        Value::Id(id) => Some(*id),
                        _ => None,
                    })
                    .fold(vec![*id2], |mut vars, id| {
                        vars.push(id);
                        vars
                    });
                (Some(*id1), Some(used_ids.into_boxed_slice()))
            }

            Self::AssignStaticCall(id1, _, _, args) => {
                let used_ids = args
                    .iter()
                    .filter_map(|(_, val)| match val {
                        Value::Id(id) => Some(*id),
                        _ => None,
                    })
                    .fold(vec![], |mut vars, id| {
                        vars.push(id);
                        vars
                    });
                (Some(*id1), Some(used_ids.into_boxed_slice()))
            }

            Self::Phi(id, vals) => (
                Some(*id),
                Some(
                    vals.iter()
                        .filter_map(|(val, _)| {
                            if let Value::Id(id) = val {
                                Some(*id)
                            } else {
                                None
                            }
                        })
                        .collect(),
                ),
            ),
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
            InstrKind::Return(id) => write!(f, "    ret {}\n}}", id),
            InstrKind::Label(subs) => write!(f, "block{}:", subs),
            InstrKind::Jmp(subs) => write!(f, "    jmp block{}", subs),
            InstrKind::JmpCond(id, on_true, on_false) => {
                write!(f, "    {} ? block{} : block{}", id, on_true, on_false)
            }
            InstrKind::Assign(id, val) => write!(f, "    {} = {}", id, val),
            InstrKind::AssignDefault(id, ty) => write!(f, "    {} = default {}", id, ty),
            InstrKind::AssignBin(op, id, lhs, rhs) => {
                write!(f, "    {} = {} {}, {}", id, op.to_ir_string(), lhs, rhs)
            }
            InstrKind::AssignUn(op, id, val) => {
                write!(f, "    {} = {} {}", id, op.to_ir_string(), val)
            }
            InstrKind::AssignCall(id, func, args) => {
                write!(f, "    {} = call {}(", id, func)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{} {}", arg.0, arg.1)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            InstrKind::AssignStaticCall(id, ty, name, args) => {
                write!(f, "    {} = dispatch {}_{}(", id, ty, name)?;
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
            InstrKind::AssignExtract(id, obj, index) => {
                write!(f, "    {} = extract {}, {}", id, obj, index)
            }
            InstrKind::Phi(id, vals) => {
                write!(f, "    {} = phi ", id)?;
                for (i, (val, block)) in vals.iter().enumerate() {
                    write!(f, "[{}, block{}]", val, block)?;
                    if i != vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
            InstrKind::Local(ty, name) => write!(f, "    local {} {}", ty, name),
            InstrKind::AssignLoad(id, ty, name, offset) => {
                write!(f, "    {} = load {} {}, {}", id, ty, name, offset)
            }
            InstrKind::Store(name, ty, id) => write!(f, "    store {}, {} {}", name, ty, id),
            InstrKind::Attr(ty, name) => write!(f, "    attr {} {}", ty, name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instr<'a> {
    pub kind:  InstrKind<'a>,
    pub span:  Span,
    pub ty:    TypeId,
    pub block: BlockId,
}

impl<'a> Instr<'a> {
    pub fn new(kind: InstrKind<'a>, span: Span, ty: TypeId, block: BlockId) -> Self {
        Self {
            kind,
            span,
            ty,
            block,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Id {
    Tmp(u32),
    Local(u32),
    Renamed(u32),
    Ptr(u32),
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

    pub fn get_subs(&self) -> u32 {
        match self {
            Self::Tmp(subs) | Self::Local(subs) | Self::Renamed(subs) | Self::Ptr(subs) => *subs,
        }
    }

    pub fn next(&self) -> Self {
        match self {
            Self::Tmp(index) => Self::Tmp(index + 1),
            Self::Local(index) => Self::Local(index + 1),
            Self::Renamed(index) => Self::Renamed(index + 1),
            Self::Ptr(index) => Self::Ptr(index + 1),
        }
    }

    pub fn next_mut(&mut self) -> Self {
        *self = self.next();
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
            Self::Ptr(id) | Self::Local(id) => id as usize,
            _ => panic!("cannot index this id"),
        }
    }

    fn from_index(index: usize) -> Self {
        Self::Local(index as u32)
    }
}
