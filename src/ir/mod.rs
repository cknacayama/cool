use std::{fmt, rc::Rc};

use crate::{
    ast::{BinOp, UnOp},
    index_vec::{Idx, IndexVec},
};

use block::BlockId;
use types::Type;

pub mod block;
pub mod builder;
pub mod opt;
pub mod types;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Value {
    #[default]
    Void,

    Id(IrId),
    Int(i64),
    Bool(bool),
}

impl fmt::Display for Value {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "$void"),
            Value::Id(id) => write!(f, "{}", id),
            Value::Int(val) => write!(f, "${}", val),
            Value::Bool(val) => write!(f, "${}", val),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringId(u32);

impl Idx for StringId {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn new(index: usize) -> Self {
        Self(index as u32)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    Nop,

    Vtable(GlobalId, Box<[Option<GlobalId>]>),
    Function {
        id:     GlobalId,
        ret:    Type,
        params: Box<[(Type, IrId)]>,
    },
    Return(Value),
    Label(BlockId),

    Jmp(BlockId),
    JmpCond {
        src:      Value,
        on_true:  BlockId,
        on_false: BlockId,
    },
    Switch {
        src:     Value,
        default: BlockId,
        cases:   Vec<(i64, BlockId)>,
    },

    Assign {
        dst: IrId,
        ty:  Type,
        src: Value,
    },
    AssignBin {
        dst: IrId,
        op:  BinOp,
        lhs: Value,
        rhs: Value,
    },
    AssignUn {
        dst: IrId,
        op:  UnOp,
        ty:  Type,
        src: Value,
    },
    AssignCall(IrId, Type, IrId, Box<[(Type, Value)]>),
    AssignExtract {
        dst:    IrId,
        src_ty: Type,
        src:    IrId,
        ty:     Type,
        offset: usize,
    },
    AssignInsert {
        dst: IrId,
        ty:  Type,
        src: IrId,
        val: Value,
        idx: usize,
    },
    AssignGep {
        dst:    IrId,
        src:    IrId,
        offset: usize,
    },
    AssignPhi(IrId, Type, Vec<(Value, BlockId)>),

    // before mem2reg
    Local(Type, IrId),

    AssignLoad {
        dst: IrId,
        ty:  Type,
        src: IrId,
    },
    Store {
        dst: IrId,
        ty:  Type,
        src: Value,
    },
}

impl Instr {
    pub fn is_block_end(&self) -> bool {
        matches!(
            self,
            Self::Return(_) | Self::Jmp(_) | Self::JmpCond { .. } | Self::Switch { .. }
        )
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Self::AssignCall(_, _, _, _) | Self::Function { .. })
    }

    pub fn is_nop(&self) -> bool {
        matches!(self, Self::Nop)
    }

    pub fn is_phi(&self) -> bool {
        matches!(self, Self::AssignPhi(_, _, _))
    }

    pub fn uses(&self) -> (Option<IrId>, Option<Box<[IrId]>>) {
        match self {
            Self::Nop | Self::Label(_) | Self::Jmp(_) => (None, None),

            Self::Function { id, params, .. } => (
                Some(IrId::Global(*id)),
                Some(params.iter().map(|(_, id)| *id).collect()),
            ),

            Self::Local(_, id) => (Some(*id), None),

            Self::Vtable(dst, ids) => (
                Some(IrId::Global(*dst)),
                Some(
                    ids.iter()
                        .filter_map(|id| match id {
                            Some(id) => Some(IrId::Global(*id)),
                            None => None,
                        })
                        .collect(),
                ),
            ),

            Self::JmpCond {
                src: Value::Id(src),
                ..
            }
            | Self::Switch {
                src: Value::Id(src),
                ..
            } => (None, Some([*src].into())),

            Self::JmpCond { .. } | Self::Switch { .. } => (None, None),

            Self::Return(val) => {
                if let Value::Id(id) = val {
                    (None, Some([*id].into()))
                } else {
                    (None, None)
                }
            }

            Self::Store { dst, src, .. } => {
                if let Value::Id(id) = src {
                    (None, Some([*dst, *id].into()))
                } else {
                    (None, Some([*dst].into()))
                }
            }

            Self::AssignBin {
                dst,
                lhs: Value::Id(src1),
                rhs: Value::Id(src2),
                ..
            }
            | Self::AssignInsert {
                dst,
                src: src1,
                val: Value::Id(src2),
                ..
            } => (Some(*dst), Some([*src1, *src2].into())),

            Self::Assign {
                dst,
                src: Value::Id(src),
                ..
            }
            | Self::AssignBin {
                dst,
                lhs: Value::Id(src),
                ..
            }
            | Self::AssignBin {
                dst,
                rhs: Value::Id(src),
                ..
            }
            | Self::AssignInsert { dst, src, .. }
            | Self::AssignUn {
                dst,
                src: Value::Id(src),
                ..
            }
            | Self::AssignLoad { dst, src, .. }
            | Self::AssignExtract { dst, src, .. }
            | Self::AssignGep { dst, src, .. } => (Some(*dst), Some([*src].into())),

            Self::Assign { dst, .. } | Self::AssignBin { dst, .. } | Self::AssignUn { dst, .. } => {
                (Some(*dst), None)
            }

            Self::AssignCall(dst, _, id2, args) => {
                let used_ids = args
                    .iter()
                    .filter_map(|(_, val)| match val {
                        Value::Id(id) => Some(id),
                        _ => None,
                    })
                    .fold(vec![*id2], |mut vars, id| {
                        vars.push(*id);
                        vars
                    });
                (Some(*dst), Some(used_ids.into_boxed_slice()))
            }

            Self::AssignPhi(dst, _, vals) => (
                Some(*dst),
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

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Nop => write!(f, "    nop"),
            Instr::Vtable(dst, ids) => {
                write!(f, "{} = [", dst)?;
                for (i, id) in ids.iter().enumerate() {
                    match id {
                        Some(id) => write!(f, "{}", id)?,
                        None => write!(f, "null")?,
                    }
                    if i != ids.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Instr::Function { id, ret, params } => {
                write!(f, "fn {} {}(", ret, id)?;
                for (i, (ty, id)) in params.iter().enumerate() {
                    write!(f, "{} {}", ty, id)?;
                    if i != params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {{")
            }
            Instr::Return(id) => write!(f, "    ret {}\n}}", id),
            Instr::Label(subs) => write!(f, "{}:", subs),
            Instr::Jmp(subs) => write!(f, "    jmp {}", subs),
            Instr::JmpCond {
                src,
                on_true,
                on_false,
            } => {
                write!(f, "    {} ? {} : {}", src, on_true, on_false)
            }
            Instr::Switch {
                src,
                default,
                cases,
            } => {
                write!(f, "    switch {} default: {}, [", src, default)?;
                for (i, (val, block)) in cases.iter().enumerate() {
                    write!(f, "{}: {}", val, block)?;
                    if i != cases.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Instr::Assign { dst, ty, src } => {
                write!(f, "    {} = {} {}", dst, ty, src)
            }
            Instr::AssignBin { op, dst, lhs, rhs } => {
                write!(f, "    {} = {} {}, {}", dst, op.to_ir_string(), lhs, rhs)
            }
            Instr::AssignUn { op, dst, ty, src } => {
                write!(f, "    {} = {} {} {}", dst, op.to_ir_string(), ty, src)
            }
            Instr::AssignCall(id, ty, func, args) => {
                write!(f, "    {} = call {} {}(", id, ty, func)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{} {}", arg.0, arg.1)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Instr::AssignExtract {
                dst,
                src_ty,
                src,
                ty,
                offset,
            } => {
                write!(
                    f,
                    "    {} = extract {} {}, {}, {}",
                    dst, src_ty, src, ty, offset
                )
            }
            Instr::AssignInsert {
                dst,
                ty,
                src,
                val,
                idx,
            } => {
                write!(f, "    {} = insert {} {}, {}, {}", dst, ty, src, val, idx)
            }
            Instr::AssignGep { dst, src, offset } => {
                write!(f, "    {} = gep {}, {}", dst, src, offset)
            }
            Instr::AssignPhi(id, ty, vals) => {
                write!(f, "    {} = phi {}", id, ty)?;
                for (i, (val, block)) in vals.iter().enumerate() {
                    write!(f, "[{}, {}]", val, block)?;
                    if i != vals.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                Ok(())
            }
            Instr::Local(ty, name) => write!(f, "    local {} {}", ty, name),
            Instr::AssignLoad { dst, ty, src } => {
                write!(f, "    {} = load {} {}", dst, ty, src)
            }
            Instr::Store { dst, ty: size, src } => {
                write!(f, "    store {}, {} {}", dst, size, src)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrId {
    Tmp(u32),
    Renamed(u32),
    Local(LocalId),
    Ptr(LocalId),
    Global(GlobalId),
}

impl Idx for IrId {
    fn index(self) -> usize {
        match self {
            Self::Tmp(subs) | Self::Renamed(subs) => subs as usize,
            Self::Local(LocalId(subs)) | Self::Ptr(LocalId(subs)) => subs as usize,
            Self::Global(GlobalId(subs)) => subs as usize,
        }
    }

    fn new(_: usize) -> Self {
        panic!()
    }

    #[inline]
    fn plus(self, increment: usize) -> Self {
        match self {
            Self::Tmp(subs) => Self::Tmp(subs + increment as u32),
            Self::Renamed(subs) => Self::Renamed(subs + increment as u32),
            Self::Local(LocalId(subs)) => Self::Local(LocalId(subs + increment as u32)),
            Self::Ptr(LocalId(subs)) => Self::Ptr(LocalId(subs + increment as u32)),
            Self::Global(GlobalId(subs)) => Self::Global(GlobalId(subs + increment as u32)),
        }
    }

    #[inline]
    fn minus(self, decrement: usize) -> Option<Self> {
        match self {
            Self::Tmp(subs) if subs > 0 => Some(Self::Tmp(subs - decrement as u32)),
            Self::Renamed(subs) if subs > 0 => Some(Self::Renamed(subs - decrement as u32)),
            Self::Local(LocalId(subs)) if subs > 0 => {
                Some(Self::Local(LocalId(subs - decrement as u32)))
            }
            Self::Ptr(LocalId(subs)) if subs > 0 => {
                Some(Self::Ptr(LocalId(subs - decrement as u32)))
            }
            Self::Global(GlobalId(subs)) if subs > 0 => {
                Some(Self::Global(GlobalId(subs - decrement as u32)))
            }
            _ => None,
        }
    }
}

impl IrId {
    pub const LOCAL_SELF: Self = Self::Local(LocalId(0));

    pub fn local_id(self) -> Option<LocalId> {
        match self {
            Self::Local(id) | Self::Ptr(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_local(self) -> bool {
        matches!(self, Self::Local(_))
    }

    pub fn is_tmp(self) -> bool {
        matches!(self, Self::Tmp(_))
    }

    pub fn is_ptr(self) -> bool {
        matches!(self, Self::Ptr(_))
    }

    pub fn is_renamed(self) -> bool {
        matches!(self, Self::Renamed(_))
    }

    pub fn is_global(self) -> bool {
        matches!(self, Self::Global(_))
    }

    pub fn get_subs(self) -> u32 {
        match self {
            Self::Tmp(subs)
            | Self::Local(LocalId(subs))
            | Self::Ptr(LocalId(subs))
            | Self::Global(GlobalId(subs))
            | Self::Renamed(subs) => subs,
        }
    }
}

impl fmt::Display for IrId {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tmp(subs) => write!(f, "%t{}", subs),
            Self::Local(LocalId(subs)) => write!(f, "%local_{}", subs),
            Self::Ptr(LocalId(subs)) => write!(f, "%ptr_{}", subs),
            Self::Global(GlobalId(subs)) => write!(f, "@globl_{}", subs),
            Self::Renamed(subs) => write!(f, "%r{}", subs),
        }
    }
}

impl fmt::Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%local_{}", self.0)
    }
}

impl fmt::Display for GlobalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@globl_{}", self.0)
    }
}

impl Idx for LocalId {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn new(index: usize) -> Self {
        Self(index as u32)
    }
}

impl Idx for GlobalId {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn new(index: usize) -> Self {
        Self(index as u32)
    }
}

impl GlobalId {
    #[inline]
    pub fn to_ident(self, globals: &IndexVec<GlobalId, Rc<str>>) -> &str {
        &globals[self]
    }
}
impl IrId {
    #[inline]
    pub fn to_ir_string(self, globals: &IndexVec<GlobalId, Rc<str>>) -> String {
        match self {
            Self::Global(id) => format!("@{}", globals[id]),
            _ => format!("{}", self),
        }
    }
}

impl Value {
    #[inline]
    pub fn to_ir_string(&self, globals: &IndexVec<GlobalId, Rc<str>>) -> String {
        match self {
            Value::Id(id) => id.to_ir_string(globals),
            _ => format!("{}", self),
        }
    }
}

impl Instr {
    pub fn to_ir_string(&self, globals: &IndexVec<GlobalId, Rc<str>>) -> String {
        match self {
            Instr::Nop => "    nop".to_string(),
            Instr::Vtable(dst, ids) => {
                let mut s = format!("@{} = [", globals[*dst]);
                for (i, id) in ids.iter().enumerate() {
                    match id {
                        Some(id) => s.push_str(&format!("@{}", globals[*id])),
                        None => s.push_str("null"),
                    }
                    if i != ids.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push(']');
                s
            }
            Instr::Function { id, ret, params } => {
                let mut s = format!("fn {} {}(", ret, globals[*id]);
                for (i, (ty, id)) in params.iter().enumerate() {
                    s.push_str(&format!("{} {}", ty, id.to_ir_string(globals)));
                    if i != params.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") {{");
                s
            }
            Instr::Return(val) => format!("    ret {}\n}}", val.to_ir_string(globals)),
            Instr::Label(subs) => format!("{}:", subs),
            Instr::Jmp(subs) => format!("    jmp {}", subs),
            Instr::JmpCond {
                src,
                on_true,
                on_false,
            } => {
                format!(
                    "    {} ? {} : {}",
                    src.to_ir_string(globals),
                    on_true,
                    on_false
                )
            }
            Instr::Switch {
                src,
                default,
                cases,
            } => {
                let mut s = format!(
                    "    switch {} default: {}, [",
                    src.to_ir_string(globals),
                    default
                );
                for (i, (val, block)) in cases.iter().enumerate() {
                    s.push_str(&format!("{}: {}", val, block));
                    if i != cases.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push(']');
                s
            }
            Instr::Assign { dst, ty, src } => {
                format!(
                    "    {} = {} {}",
                    dst.to_ir_string(globals),
                    ty,
                    src.to_ir_string(globals)
                )
            }
            Instr::AssignBin { op, dst, lhs, rhs } => {
                format!(
                    "    {} = {} {}, {}",
                    dst,
                    op.to_ir_string(),
                    lhs.to_ir_string(globals),
                    rhs.to_ir_string(globals)
                )
            }
            Instr::AssignUn { op, dst, ty, src } => {
                format!(
                    "    {} = {} {} {}",
                    dst,
                    op.to_ir_string(),
                    ty,
                    src.to_ir_string(globals)
                )
            }
            Instr::AssignCall(id, ty, func, args) => {
                let mut s = format!("    {} = call {} {}(", id, ty, func.to_ir_string(globals));
                for (i, arg) in args.iter().enumerate() {
                    s.push_str(&format!("{} {}", arg.0, arg.1.to_ir_string(globals)));
                    if i != args.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push(')');
                s
            }
            Instr::AssignExtract {
                dst,
                src_ty,
                src,
                ty,
                offset,
            } => {
                format!(
                    "    {} = extract {} {}, {}, {}",
                    dst.to_ir_string(globals),
                    src_ty,
                    src.to_ir_string(globals),
                    ty,
                    offset
                )
            }
            Instr::AssignInsert {
                dst,
                ty,
                src,
                val,
                idx,
            } => {
                format!(
                    "    {} = insert {} {}, {}, {}",
                    dst.to_ir_string(globals),
                    ty,
                    src.to_ir_string(globals),
                    val.to_ir_string(globals),
                    idx
                )
            }
            Instr::AssignGep { dst, src, offset } => {
                format!(
                    "    {} = gep {}, {}",
                    dst.to_ir_string(globals),
                    src.to_ir_string(globals),
                    offset
                )
            }
            Instr::AssignPhi(id, ty, vals) => {
                let mut s = format!("    {} = phi {} ", id, ty);
                for (i, (val, block)) in vals.iter().enumerate() {
                    s.push_str(&format!("[{}, {}]", val.to_ir_string(globals), block));
                    if i != vals.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s
            }
            Instr::Local(ty, name) => format!("    local {} {}", ty, name),
            Instr::AssignLoad { dst, ty, src } => {
                format!("    {} = load {} {}", dst, ty, src.to_ir_string(globals),)
            }
            Instr::Store { dst, ty, src } => {
                format!("    store {}, {} {}", dst.to_ir_string(globals), ty, src)
            }
        }
    }
}
