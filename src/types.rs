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
    CannotRedefineSelfType,
    NotSubtype(TypeId, TypeId),
    EmptyJoin,
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
            TypeErrorKind::CannotRedefineSelfType => write!(f, "cannot redefine SELF_TYPE"),
            TypeErrorKind::CannotInheritFromSelf => write!(f, "cannot inherit from SELF_TYPE"),
            TypeErrorKind::CannotInheritFromBool => write!(f, "cannot inherit from Bool"),
            TypeErrorKind::CannotInheritFromInt => write!(f, "cannot inherit from Int"),
            TypeErrorKind::CannotInheritFromString => write!(f, "cannot inherit from String"),
            TypeErrorKind::NotSubtype(lhs, rhs) => {
                write!(f, "{} is not a subtype of {}", lhs, rhs)
            }
            TypeErrorKind::EmptyJoin => write!(f, "empty join"),
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

const IO_CLASS: Type<'static> = Type::Class("IO");

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

/// An unique identifier for types.
///
/// Because it is only possible to construct a `ClassId` when
/// inserting a class into the `ClassEnv`, it is guaranteed that
/// the `ClassId` is unique and valid.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeId {
    SelfType,
    Class(ClassId),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassId(NonZeroU32);

impl TypeId {
    pub const OBJECT: TypeId = TypeId::Class(ClassId(unsafe { NonZeroU32::new_unchecked(1) }));
    pub const INT: TypeId = TypeId::Class(ClassId(unsafe { NonZeroU32::new_unchecked(2) }));
    pub const BOOL: TypeId = TypeId::Class(ClassId(unsafe { NonZeroU32::new_unchecked(3) }));
    pub const STRING: TypeId = TypeId::Class(ClassId(unsafe { NonZeroU32::new_unchecked(4) }));
    pub const IO: TypeId = TypeId::Class(ClassId(unsafe { NonZeroU32::new_unchecked(5) }));

    pub fn id(self) -> Option<NonZeroU32> {
        match self {
            TypeId::Class(id) => Some(id.0),
            _ => None,
        }
    }

    fn to_index(self) -> Option<usize> {
        match self {
            TypeId::Class(id) => Some((id.0.get() - 1) as usize),
            _ => None,
        }
    }

    fn to_index_or_err(self) -> Result<usize, TypeErrorKind<'static>> {
        self.to_index().ok_or(TypeErrorKind::CannotIndexSelfType)
    }

    pub fn is_self_type(self) -> bool {
        matches!(self, TypeId::SelfType)
    }

    pub fn is_object(self) -> bool {
        self == TypeId::OBJECT
    }

    pub fn is_int(self) -> bool {
        self == TypeId::INT
    }

    pub fn is_bool(self) -> bool {
        self == TypeId::BOOL
    }

    pub fn is_string(self) -> bool {
        self == TypeId::STRING
    }

    pub fn is_io(self) -> bool {
        self == TypeId::IO
    }

    pub fn is_inheritable(self) -> bool {
        self == TypeId::OBJECT || self >= TypeId::IO
    }

    pub fn check_inheritance(self) -> Result<(), TypeErrorKind<'static>> {
        match self {
            Self::SelfType => Err(TypeErrorKind::CannotInheritFromSelf),
            TypeId::BOOL => Err(TypeErrorKind::CannotInheritFromBool),
            TypeId::INT => Err(TypeErrorKind::CannotInheritFromInt),
            TypeId::STRING => Err(TypeErrorKind::CannotInheritFromString),
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
            &TypeId::OBJECT => write!(f, "Object"),
            &TypeId::INT => write!(f, "Int"),
            &TypeId::BOOL => write!(f, "Bool"),
            &TypeId::STRING => write!(f, "String"),
            &TypeId::IO => write!(f, "IO"),
            TypeId::Class(id) => write!(f, "Class{}", id.0),
        }
    }
}

impl std::fmt::Debug for ClassId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl std::fmt::Display for ClassId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0.get())
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
            Some(TypeId::BOOL) => Err(TypeErrorKind::CannotInheritFromBool),
            Some(TypeId::INT) => Err(TypeErrorKind::CannotInheritFromInt),
            Some(TypeId::STRING) => Err(TypeErrorKind::CannotInheritFromString),
            _ => Ok(Self {
                parent,
                attributes,
                methods,
                children,
            }),
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

        let object = ClassTypeData::new(
            None,
            HashMap::new(),
            HashMap::from([
                ("abort", MethodTypeData::new(Box::new([]), TypeId::OBJECT)),
                (
                    "type_name",
                    MethodTypeData::new(Box::new([]), TypeId::STRING),
                ),
                ("copy", MethodTypeData::new(Box::new([]), TypeId::SelfType)),
            ]),
            HashSet::from([TypeId::INT, TypeId::BOOL, TypeId::STRING]),
        )
        .unwrap();
        let bool_ = ClassTypeData::new(
            Some(TypeId::OBJECT),
            HashMap::new(),
            HashMap::new(),
            HashSet::new(),
        )
        .unwrap();
        let int = ClassTypeData::new(
            Some(TypeId::OBJECT),
            HashMap::new(),
            HashMap::new(),
            HashSet::new(),
        )
        .unwrap();
        let string = ClassTypeData::new(
            Some(TypeId::OBJECT),
            HashMap::new(),
            HashMap::from([
                ("length", MethodTypeData::new(Box::new([]), TypeId::INT)),
                (
                    "concat",
                    MethodTypeData::new(Box::from([TypeId::STRING]), TypeId::STRING),
                ),
                (
                    "substr",
                    MethodTypeData::new(Box::from([TypeId::INT, TypeId::INT]), TypeId::STRING),
                ),
            ]),
            HashSet::new(),
        )
        .unwrap();
        let io = ClassTypeData::new(
            Some(TypeId::OBJECT),
            HashMap::new(),
            HashMap::from([
                (
                    "out_string",
                    MethodTypeData::new(Box::from([TypeId::STRING]), TypeId::SelfType),
                ),
                (
                    "out_int",
                    MethodTypeData::new(Box::from([TypeId::INT]), TypeId::SelfType),
                ),
                (
                    "in_string",
                    MethodTypeData::new(Box::from([]), TypeId::STRING),
                ),
                ("in_int", MethodTypeData::new(Box::from([]), TypeId::INT)),
            ]),
            HashSet::new(),
        )
        .unwrap();

        classes.push(object);
        classes.push(int);
        classes.push(bool_);
        classes.push(string);
        classes.push(io);

        let types = HashMap::from([
            (Type::Object, TypeId::OBJECT),
            (Type::Int, TypeId::INT),
            (Type::Bool, TypeId::BOOL),
            (Type::String, TypeId::STRING),
            (Type::SelfType, TypeId::SelfType),
            (IO_CLASS, TypeId::IO),
        ]);

        Self { types, classes }
    }

    fn get_parent(&self, ty: TypeId) -> Result<Option<TypeId>, TypeErrorKind<'static>> {
        match ty.to_index() {
            None => Err(TypeErrorKind::CannotIndexSelfType),
            Some(index) => Ok(self.classes.get(index).unwrap().parent),
        }
    }

    pub fn get_type(&self, ty: &Type<'a>) -> Option<TypeId> {
        self.types.get(ty).copied()
    }

    fn insert_type(&mut self, ty: Type<'a>) -> Result<TypeId, TypeErrorKind<'a>> {
        let id: u32 = (self.classes.len() + 1)
            .try_into()
            .expect("too many classes");
        let id = TypeId::Class(ClassId(NonZeroU32::new(id).unwrap()));

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
        if let Type::SelfType = ty {
            return Err(TypeErrorKind::CannotRedefineSelfType);
        }

        let id = self.insert_type(ty)?;

        if let Some(parent) = data.parent {
            self.classes
                .get_mut(parent.to_index().unwrap())
                .unwrap()
                .children
                .insert(id);
        }

        self.classes.push(data);

        Ok(id)
    }

    pub fn get_method(
        &self,
        ty: TypeId,
        method: &'a str,
    ) -> Result<&MethodTypeData, TypeErrorKind<'a>> {
        let mut data = &self.classes[ty.to_index_or_err()?];

        loop {
            if let Some(data) = data.methods.get(method) {
                return Ok(data);
            }
            match data.parent {
                Some(parent) => data = &self.classes[parent.to_index().unwrap()],
                None => break,
            };
        }

        Err(TypeErrorKind::UndefinedMethod(ty, method))
    }

    pub fn insert_method(
        &mut self,
        ty: TypeId,
        method: &'a str,
        data: MethodTypeData,
    ) -> Result<(), TypeErrorKind<'a>> {
        if let Some(parent) = self.get_parent(ty)? {
            match self.get_method(parent, method) {
                Ok(parent_method) if parent_method != &data => {
                    return Err(TypeErrorKind::RedefinedMethod(ty, method))
                }
                Err(err) if !matches!(err, TypeErrorKind::UndefinedMethod(_, _)) => {
                    return Err(err)
                }
                _ => (),
            }
        }

        match self
            .classes
            .get_mut(ty.to_index_or_err()?)
            .unwrap()
            .methods
            .insert(method, data)
        {
            Some(_) => Err(TypeErrorKind::RedefinedMethod(ty, method)),
            None => Ok(()),
        }
    }

    pub fn get_attribute(&self, ty: TypeId, attr: &'a str) -> Result<TypeId, TypeErrorKind<'a>> {
        let mut data = &self.classes[ty.to_index_or_err()?];

        loop {
            if let Some(data) = data.attributes.get(attr) {
                return Ok(*data);
            }
            match data.parent {
                Some(parent) => data = &self.classes[parent.to_index().unwrap()],
                None => break,
            };
        }

        Err(TypeErrorKind::UndefinedObject(attr))
    }

    pub fn insert_attribute(
        &mut self,
        ty: TypeId,
        attr: &'a str,
        data: TypeId,
    ) -> Result<(), TypeErrorKind<'a>> {
        match self
            .classes
            .get_mut(ty.to_index_or_err()?)
            .unwrap()
            .attributes
            .insert(attr, data)
        {
            Some(_) => Err(TypeErrorKind::RedefinedAttribute(ty, attr)),
            None => Ok(()),
        }
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

        loop {
            let class = &self.classes[cur_lhs.to_index().unwrap()];
            if class.parent == Some(rhs) {
                return Ok(());
            }
            match class.parent {
                Some(parent) if parent == rhs => return Ok(()),
                Some(parent) => cur_lhs = parent,
                None => break,
            };
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

        loop {
            let class = &self.classes[rhs.to_index().unwrap()];
            rhs_ancestors.insert(rhs);
            match class.parent {
                Some(parent) => rhs = parent,
                None => break,
            }
        }

        loop {
            let class = &self.classes[lhs.to_index().unwrap()];
            if rhs_ancestors.contains(&lhs) {
                return Ok(lhs);
            }
            match class.parent {
                Some(parent) => lhs = parent,
                None => break,
            }
        }

        Ok(TypeId::OBJECT)
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

        let a_data = ClassTypeData::new(
            Some(TypeId::OBJECT),
            HashMap::new(),
            HashMap::new(),
            HashSet::new(),
        )
        .unwrap();
        let b_data = ClassTypeData::new(
            Some(TypeId::OBJECT),
            HashMap::new(),
            HashMap::new(),
            HashSet::new(),
        )
        .unwrap();
        let a = env.insert_class(a, a_data).unwrap();
        let b = env.insert_class(b, b_data).unwrap();

        let c_data =
            ClassTypeData::new(Some(a), HashMap::new(), HashMap::new(), HashSet::new()).unwrap();
        let c = env.insert_class(c, c_data).unwrap();

        let d_data =
            ClassTypeData::new(Some(b), HashMap::new(), HashMap::new(), HashSet::new()).unwrap();
        let e_data =
            ClassTypeData::new(Some(c), HashMap::new(), HashMap::new(), HashSet::new()).unwrap();
        let d = env.insert_class(d, d_data).unwrap();
        let e = env.insert_class(e, e_data).unwrap();

        assert_eq!(env.join(a, b, a), Ok(TypeId::OBJECT));
        assert_eq!(env.join(a, c, a), Ok(a));
        assert_eq!(env.join(a, d, a), Ok(TypeId::OBJECT));
        assert_eq!(env.join(a, e, a), Ok(a));
        assert_eq!(env.join(b, c, a), Ok(TypeId::OBJECT));
        assert_eq!(env.join(b, d, a), Ok(b));
        assert_eq!(env.join(b, e, a), Ok(TypeId::OBJECT));
        assert_eq!(env.join(c, d, a), Ok(TypeId::OBJECT));
        assert_eq!(env.join(c, e, a), Ok(c));
        assert_eq!(env.join(d, e, a), Ok(TypeId::OBJECT));

        assert_eq!(env.join(a, TypeId::SelfType, a), Ok(a));

        assert_eq!(env.join_fold([a, c, e], a), Ok(a));
        assert_eq!(env.join_fold([c, d, e], a), Ok(TypeId::OBJECT));
        assert_eq!(env.join_fold([a, b, c, d, e], a), Ok(TypeId::OBJECT));
    }
}
