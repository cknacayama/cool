use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeErrorKind<'a> {
    ExpectedType { expected: TypeId, found: TypeId },
    ExpectedArity { expected: usize, found: usize },
    ExpectedBool(TypeId),
    ExpectedInt(TypeId),
    UndefinedClass(&'a str),
    UndefinedClassId(TypeId),
    CannotIndexSelfType,
    UndefinedMethod(TypeId, &'a str),
    UndefinedObject(&'a str),
    RedefinedClass(TypeId),
    RedefinedMethod(TypeId, &'a str),
    RedefinedAttribute(TypeId, &'a str),
    CannotInheritFromSelf,
    CannotInheritFromBool,
    CannotInheritFromInt,
    CannotInheritFromString,
    NotSubtype(TypeId, TypeId),
    EmptyJoin,
    NoneType,
}

impl<'a> std::fmt::Display for TypeErrorKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeErrorKind::ExpectedType { expected, found } => {
                write!(f, "expected {}, found {}", expected, found)
            }
            TypeErrorKind::ExpectedArity { expected, found } => {
                write!(f, "expected {} arguments, found {}", expected, found)
            }
            TypeErrorKind::ExpectedBool(ty) => write!(f, "expected Bool, found {}", ty),
            TypeErrorKind::ExpectedInt(ty) => write!(f, "expected Int, found {}", ty),
            TypeErrorKind::UndefinedClass(id) => write!(f, "undefined class: {}", id),
            TypeErrorKind::UndefinedClassId(id) => write!(f, "undefined class id: {}", id),
            TypeErrorKind::CannotIndexSelfType => write!(f, "cannot index SELF_TYPE"),
            TypeErrorKind::UndefinedMethod(class, method) => {
                write!(f, "undefined method: {}.{}", class, method)
            }
            TypeErrorKind::UndefinedObject(id) => write!(f, "undefined object: {}", id),
            TypeErrorKind::RedefinedClass(id) => write!(f, "redefined class: {}", id),
            TypeErrorKind::RedefinedMethod(class, method) => {
                write!(f, "redefined method: {}.{}", class, method)
            }
            TypeErrorKind::RedefinedAttribute(class, attr) => {
                write!(f, "redefined attribute: {}.{}", class, attr)
            }
            TypeErrorKind::CannotInheritFromSelf => write!(f, "cannot inherit from SELF_TYPE"),
            TypeErrorKind::CannotInheritFromBool => write!(f, "cannot inherit from Bool"),
            TypeErrorKind::CannotInheritFromInt => write!(f, "cannot inherit from Int"),
            TypeErrorKind::CannotInheritFromString => write!(f, "cannot inherit from String"),
            TypeErrorKind::NotSubtype(lhs, rhs) => {
                write!(f, "{} is not a subtype of {}", lhs, rhs)
            }
            TypeErrorKind::EmptyJoin => write!(f, "empty join"),
            TypeErrorKind::NoneType => write!(f, "none type"),
        }
    }
}

impl<'a> std::error::Error for TypeErrorKind<'a> {
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeError<'a> {
    pub kind: TypeErrorKind<'a>,
    pub span: Span,
}

impl<'a> TypeError<'a> {
    pub fn new(kind: TypeErrorKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

impl<'a> std::fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} at {}", self.kind, self.span)
    }
}

impl<'a> std::error::Error for TypeError<'a> {
}

pub type TypeResult<'a, T> = Result<T, TypeError<'a>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type<'a> {
    Object,
    Int,
    Bool,
    String,
    SelfType,
    Class(&'a str),
}

pub const IO_CLASS: Type<'static> = Type::Class("IO");

impl<'a> Type<'a> {
    pub fn to_str(&self) -> &'a str {
        match self {
            Type::Object => "Object",
            Type::Int => "Int",
            Type::Bool => "Bool",
            Type::String => "String",
            Type::SelfType => "SELF_TYPE",
            Type::Class(id) => id,
        }
    }
    pub fn is_self_type(&self) -> bool {
        matches!(self, Type::SelfType)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Type::Object)
    }

    pub fn is_io(&self) -> bool {
        matches!(self, &IO_CLASS)
    }

    pub fn is_inheritable(&self) -> bool {
        !matches!(self, Type::SelfType | Type::Bool | Type::Int | Type::String)
    }

    pub fn check_inheritance(&self) -> Result<(), TypeErrorKind<'a>> {
        match self {
            Type::SelfType => Err(TypeErrorKind::CannotInheritFromSelf),
            Type::Bool => Err(TypeErrorKind::CannotInheritFromBool),
            Type::Int => Err(TypeErrorKind::CannotInheritFromInt),
            Type::String => Err(TypeErrorKind::CannotInheritFromString),
            _ => Ok(()),
        }
    }

    pub fn map_self_type<F>(self, f: F) -> Self
    where
        F: FnOnce() -> Type<'a>,
    {
        match self {
            Type::SelfType => f(),
            ty => ty,
        }
    }
}

impl<'a> std::fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Object => write!(f, "Object"),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::SelfType => write!(f, "Type::SelfType"),
            Type::Class(id) => write!(f, "{}", id),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeId {
    SelfType,
    Class(NonZeroU32),
}

pub const OBJECT_ID: TypeId = TypeId::Class(unsafe { NonZeroU32::new_unchecked(1) });
pub const INT_ID: TypeId = TypeId::Class(unsafe { NonZeroU32::new_unchecked(2) });
pub const BOOL_ID: TypeId = TypeId::Class(unsafe { NonZeroU32::new_unchecked(3) });
pub const STRING_ID: TypeId = TypeId::Class(unsafe { NonZeroU32::new_unchecked(4) });
pub const IO_ID: TypeId = TypeId::Class(unsafe { NonZeroU32::new_unchecked(5) });

impl TypeId {
    pub fn id(self) -> Option<NonZeroU32> {
        match self {
            TypeId::Class(id) => Some(id),
            _ => None,
        }
    }

    pub fn to_index(self) -> Option<usize> {
        match self {
            TypeId::Class(id) => Some((id.get() - 1) as usize),
            _ => None,
        }
    }

    pub fn to_index_or_err(self) -> Result<usize, TypeErrorKind<'static>> {
        self.to_index().ok_or(TypeErrorKind::UndefinedClassId(self))
    }

    pub fn is_self_type(self) -> bool {
        matches!(self, TypeId::SelfType)
    }

    pub fn is_object(self) -> bool {
        self == OBJECT_ID
    }

    pub fn is_int(self) -> bool {
        self == INT_ID
    }

    pub fn is_bool(self) -> bool {
        self == BOOL_ID
    }

    pub fn is_string(self) -> bool {
        self == STRING_ID
    }

    pub fn is_io(self) -> bool {
        self == IO_ID
    }

    pub fn is_inheritable(self) -> bool {
        self == OBJECT_ID || self >= IO_ID
    }

    pub fn check_inheritance(self) -> Result<(), TypeErrorKind<'static>> {
        match self {
            Self::SelfType => Err(TypeErrorKind::CannotInheritFromSelf),
            BOOL_ID => Err(TypeErrorKind::CannotInheritFromBool),
            INT_ID => Err(TypeErrorKind::CannotInheritFromInt),
            STRING_ID => Err(TypeErrorKind::CannotInheritFromString),
            _ => Ok(()),
        }
    }

    pub fn map_self_type<F>(self, f: F) -> Self
    where
        F: FnOnce() -> TypeId,
    {
        match self {
            Self::SelfType => f(),
            ty => ty,
        }
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::SelfType => write!(f, "SELF_TYPE"),
            &OBJECT_ID => write!(f, "Object"),
            &INT_ID => write!(f, "Int"),
            &BOOL_ID => write!(f, "Bool"),
            &STRING_ID => write!(f, "String"),
            &IO_ID => write!(f, "IO"),
            TypeId::Class(id) => write!(f, "Class{}", id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodTypeData {
    pub params:    Box<[TypeId]>,
    pub return_ty: TypeId,
}

impl MethodTypeData {
    pub fn new(params: Box<[TypeId]>, return_ty: TypeId) -> Self {
        Self { params, return_ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassTypeData<'a> {
    parent:     Option<TypeId>,
    attributes: HashMap<&'a str, TypeId>,
    methods:    HashMap<&'a str, MethodTypeData>,
    children:   HashSet<TypeId>,
}

impl<'a> ClassTypeData<'a> {
    pub fn new(
        parent: Option<TypeId>,
        attributes: HashMap<&'a str, TypeId>,
        methods: HashMap<&'a str, MethodTypeData>,
        children: HashSet<TypeId>,
    ) -> Result<Self, TypeErrorKind<'static>> {
        match parent {
            Some(TypeId::SelfType) => Err(TypeErrorKind::CannotInheritFromSelf),
            Some(BOOL_ID) => Err(TypeErrorKind::CannotInheritFromBool),
            Some(INT_ID) => Err(TypeErrorKind::CannotInheritFromInt),
            Some(STRING_ID) => Err(TypeErrorKind::CannotInheritFromString),
            _ => Ok(Self {
                parent,
                attributes,
                methods,
                children,
            }),
        }
    }

    /// # Safety
    ///
    /// This function is unsafe because it allows creating a ClassTypeData with an invalid parent.
    /// The parent must be a valid inheritable type.
    pub unsafe fn new_unchecked(
        parent: Option<TypeId>,
        attributes: HashMap<&'a str, TypeId>,
        methods: HashMap<&'a str, MethodTypeData>,
        children: HashSet<TypeId>,
    ) -> Self {
        Self {
            parent,
            attributes,
            methods,
            children,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectEnv<'a> {
    objects: HashMap<&'a str, TypeId>,
}

impl<'a> Default for ObjectEnv<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ObjectEnv<'a> {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
        }
    }

    pub fn get(&self, id: &'a str) -> Option<TypeId> {
        self.objects.get(id).copied()
    }

    pub fn insert(&mut self, id: &'a str, ty: TypeId) -> Option<TypeId> {
        self.objects.insert(id, ty)
    }
}

#[derive(Debug)]
pub struct ClassEnv<'a> {
    types:   HashMap<Type<'a>, TypeId>,
    classes: Vec<ClassTypeData<'a>>,
}

impl<'a> Default for ClassEnv<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ClassEnv<'a> {
    pub fn new() -> Self {
        Self {
            types:   HashMap::new(),
            classes: vec![],
        }
    }

    /// Create a new ClassEnv with the built-in classes.
    pub fn with_builtin() -> Self {
        let mut classes = vec![];

        let object = unsafe {
            ClassTypeData::new_unchecked(
                None,
                HashMap::new(),
                HashMap::from([
                    ("abort", MethodTypeData::new(Box::new([]), OBJECT_ID)),
                    ("type_name", MethodTypeData::new(Box::new([]), STRING_ID)),
                    ("copy", MethodTypeData::new(Box::new([]), TypeId::SelfType)),
                ]),
                HashSet::from([INT_ID, BOOL_ID, STRING_ID]),
            )
        };
        let bool_ = unsafe {
            ClassTypeData::new_unchecked(
                Some(OBJECT_ID),
                HashMap::new(),
                HashMap::new(),
                HashSet::new(),
            )
        };
        let int = unsafe {
            ClassTypeData::new_unchecked(
                Some(OBJECT_ID),
                HashMap::new(),
                HashMap::new(),
                HashSet::new(),
            )
        };
        let string = unsafe {
            ClassTypeData::new_unchecked(
                Some(OBJECT_ID),
                HashMap::new(),
                HashMap::from([
                    ("length", MethodTypeData::new(Box::new([]), INT_ID)),
                    (
                        "concat",
                        MethodTypeData::new(Box::from([STRING_ID]), STRING_ID),
                    ),
                    (
                        "substr",
                        MethodTypeData::new(Box::from([INT_ID, INT_ID]), STRING_ID),
                    ),
                ]),
                HashSet::new(),
            )
        };
        let io = unsafe {
            ClassTypeData::new_unchecked(
                Some(OBJECT_ID),
                HashMap::new(),
                HashMap::from([
                    (
                        "out_string",
                        MethodTypeData::new(Box::from([STRING_ID]), TypeId::SelfType),
                    ),
                    (
                        "out_int",
                        MethodTypeData::new(Box::from([INT_ID]), TypeId::SelfType),
                    ),
                    ("in_string", MethodTypeData::new(Box::from([]), STRING_ID)),
                    ("in_int", MethodTypeData::new(Box::from([]), INT_ID)),
                ]),
                HashSet::new(),
            )
        };

        classes.push(object);
        classes.push(int);
        classes.push(bool_);
        classes.push(string);
        classes.push(io);

        let types = HashMap::from([
            (Type::Object, OBJECT_ID),
            (Type::Int, INT_ID),
            (Type::Bool, BOOL_ID),
            (Type::String, STRING_ID),
            (Type::SelfType, TypeId::SelfType),
            (IO_CLASS, IO_ID),
        ]);

        Self { types, classes }
    }

    pub fn get_parent(&self, ty: TypeId) -> Result<Option<TypeId>, TypeErrorKind<'a>> {
        match ty.to_index() {
            None => Ok(None),
            Some(index) => self
                .classes
                .get(index)
                .map(|class| class.parent)
                .ok_or(TypeErrorKind::UndefinedClassId(ty)),
        }
    }

    pub fn get_class(&self, ty: TypeId) -> Option<&ClassTypeData<'a>> {
        self.classes.get(ty.to_index()?)
    }

    pub fn get_type(&self, ty: &Type<'a>) -> Option<TypeId> {
        self.types.get(ty).copied()
    }

    fn insert_type(&mut self, ty: Type<'a>) -> Result<TypeId, TypeErrorKind<'a>> {
        let id: u32 = (self.classes.len() + 1).try_into().unwrap();
        let id = TypeId::Class(unsafe { NonZeroU32::new_unchecked(id) });

        match self.types.insert(ty, id) {
            Some(class_id) => Err(TypeErrorKind::RedefinedClass(class_id)),
            None => Ok(id),
        }
    }

    pub fn insert_class(
        &mut self,
        ty: Type<'a>,
        data: ClassTypeData<'a>,
    ) -> Result<TypeId, TypeErrorKind<'a>> {
        let id = self.insert_type(ty)?;

        if let Some(parent) = data.parent {
            match self.classes.get_mut(parent.to_index().unwrap()) {
                Some(parent) => parent.children.insert(id),
                None => return Err(TypeErrorKind::UndefinedClassId(parent)),
            };
        }

        self.classes.push(data);

        Ok(id)
    }

    pub fn get_method(
        &self,
        ty: TypeId,
        method: &'a str,
    ) -> Result<&MethodTypeData, TypeErrorKind<'a>> {
        self.classes
            .get(ty.to_index_or_err()?)
            .ok_or(TypeErrorKind::UndefinedClassId(ty))
            .and_then(|class| {
                class
                    .methods
                    .get(method)
                    .or_else(|| {
                        class
                            .parent
                            .and_then(|parent| self.get_method(parent, method).ok())
                    })
                    .ok_or(TypeErrorKind::UndefinedMethod(ty, method))
            })
    }

    pub fn insert_method(
        &mut self,
        ty: TypeId,
        method: &'a str,
        data: MethodTypeData,
    ) -> Result<(), TypeErrorKind<'a>> {
        self.get_parent(ty)?
            .map(|parent| match self.get_method(parent, method) {
                Ok(parent_method) if parent_method == &data => Ok(()),
                Err(TypeErrorKind::UndefinedMethod(_, _)) => Ok(()),
                Ok(_) => Err(TypeErrorKind::RedefinedMethod(ty, method)),
                Err(err) => Err(err),
            })
            .transpose()?;

        self.classes
            .get_mut(ty.to_index_or_err()?)
            .ok_or(TypeErrorKind::UndefinedClassId(ty))
            .and_then(|class| match class.methods.insert(method, data) {
                Some(_) => Err(TypeErrorKind::RedefinedMethod(ty, method)),
                None => Ok(()),
            })
    }

    pub fn get_attribute(&self, ty: TypeId, attr: &'a str) -> Result<TypeId, TypeErrorKind<'a>> {
        self.classes
            .get(ty.to_index_or_err()?)
            .ok_or(TypeErrorKind::UndefinedClassId(ty))
            .and_then(|class| {
                class
                    .attributes
                    .get(attr)
                    .copied()
                    .or_else(|| {
                        class
                            .parent
                            .and_then(|parent| self.get_attribute(parent, attr).ok())
                    })
                    .ok_or(TypeErrorKind::UndefinedObject(attr))
            })
    }

    pub fn insert_attribute(
        &mut self,
        ty: TypeId,
        attr: &'a str,
        data: TypeId,
    ) -> Result<(), TypeErrorKind<'a>> {
        self.classes
            .get_mut(ty.to_index_or_err()?)
            .ok_or(TypeErrorKind::UndefinedClassId(ty))
            .and_then(|class| match class.attributes.insert(attr, data) {
                Some(_) => Err(TypeErrorKind::RedefinedAttribute(ty, attr)),
                None => Ok(()),
            })
    }

    pub fn is_subtype(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        current_class: TypeId,
    ) -> Result<(), TypeErrorKind<'a>> {
        let mut cur_lhs = match lhs {
            TypeId::SelfType => current_class,
            _ => lhs,
        };
        let rhs = match rhs {
            TypeId::SelfType => current_class,
            _ => rhs,
        };
        if cur_lhs == rhs {
            return Ok(());
        }

        while let Some(class) = match self.classes.get(cur_lhs.to_index_or_err()?) {
            None => return Err(TypeErrorKind::UndefinedClassId(cur_lhs)),
            class => class,
        } {
            if class.parent == Some(rhs) {
                return Ok(());
            }
            match class.parent {
                Some(parent) => cur_lhs = parent,
                None => break,
            }
        }

        Err(TypeErrorKind::NotSubtype(lhs, rhs))
    }

    /// Get the least/lowest common ancestor of two types.
    pub fn join(
        &self,
        lhs: TypeId,
        rhs: TypeId,
        current_class: TypeId,
    ) -> Result<TypeId, TypeErrorKind<'a>> {
        let mut lhs = match lhs {
            TypeId::SelfType => current_class,
            _ => lhs,
        };
        let mut rhs = match rhs {
            TypeId::SelfType => current_class,
            _ => rhs,
        };

        if lhs == rhs {
            return Ok(lhs);
        }

        let mut rhs_ancestors = HashSet::new();

        while let Some(class) = match self.classes.get(rhs.to_index_or_err()?) {
            None => return Err(TypeErrorKind::UndefinedClassId(rhs)),
            class => class,
        } {
            rhs_ancestors.insert(rhs);
            match class.parent {
                Some(parent) => rhs = parent,
                None => break,
            }
        }

        while let Some(class) = match self.classes.get(lhs.to_index_or_err()?) {
            None => return Err(TypeErrorKind::UndefinedClassId(lhs)),
            class => class,
        } {
            if rhs_ancestors.contains(&lhs) {
                return Ok(lhs);
            }
            match class.parent {
                Some(parent) => lhs = parent,
                None => break,
            }
        }

        Ok(OBJECT_ID)
    }

    pub fn join_fold<I>(&self, types: I, current_class: TypeId) -> Result<TypeId, TypeErrorKind<'a>>
    where
        I: IntoIterator<Item = TypeId>,
    {
        let mut types = types.into_iter();

        let first = match types.next() {
            Some(ty) => ty,
            None => return Err(TypeErrorKind::EmptyJoin),
        };

        types.try_fold(first, |acc, ty| self.join(acc, ty, current_class))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_join() {
        let mut env = ClassEnv::with_builtin();

        let a = Type::Class("A");
        let b = Type::Class("B");
        let c = Type::Class("C");
        let d = Type::Class("D");
        let e = Type::Class("E");

        let a_data = unsafe {
            ClassTypeData::new_unchecked(
                Some(OBJECT_ID),
                HashMap::new(),
                HashMap::new(),
                HashSet::new(),
            )
        };
        let b_data = unsafe {
            ClassTypeData::new_unchecked(
                Some(OBJECT_ID),
                HashMap::new(),
                HashMap::new(),
                HashSet::new(),
            )
        };
        let a = env.insert_class(a, a_data).unwrap();
        let b = env.insert_class(b, b_data).unwrap();

        let c_data = unsafe {
            ClassTypeData::new_unchecked(Some(a), HashMap::new(), HashMap::new(), HashSet::new())
        };
        let c = env.insert_class(c, c_data).unwrap();

        let d_data = unsafe {
            ClassTypeData::new_unchecked(Some(b), HashMap::new(), HashMap::new(), HashSet::new())
        };
        let e_data = unsafe {
            ClassTypeData::new_unchecked(Some(c), HashMap::new(), HashMap::new(), HashSet::new())
        };
        let d = env.insert_class(d, d_data).unwrap();
        let e = env.insert_class(e, e_data).unwrap();

        assert_eq!(env.join(a, b, a), Ok(OBJECT_ID));
        assert_eq!(env.join(a, c, a), Ok(a));
        assert_eq!(env.join(a, d, a), Ok(OBJECT_ID));
        assert_eq!(env.join(a, e, a), Ok(a));
        assert_eq!(env.join(b, c, a), Ok(OBJECT_ID));
        assert_eq!(env.join(b, d, a), Ok(b));
        assert_eq!(env.join(b, e, a), Ok(OBJECT_ID));
        assert_eq!(env.join(c, d, a), Ok(OBJECT_ID));
        assert_eq!(env.join(c, e, a), Ok(c));
        assert_eq!(env.join(d, e, a), Ok(OBJECT_ID));

        assert_eq!(env.join(a, TypeId::SelfType, a), Ok(a));

        assert_eq!(env.join_fold([a, c, e], a), Ok(a));
        assert_eq!(env.join_fold([c, d, e], a), Ok(OBJECT_ID));
        assert_eq!(env.join_fold([a, b, c, d, e], a), Ok(OBJECT_ID));
    }
}
