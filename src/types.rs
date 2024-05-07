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

    #[inline]
    pub fn align(size: usize, alignment: usize) -> usize {
        assert!(alignment.is_power_of_two());
        (size + alignment - 1) & !(alignment - 1)
    }

    #[inline]
    fn alignment(self) -> usize {
        match self {
            TypeId::BOOL => 1,
            _ => 8,
        }
    }

    pub fn size_of(self) -> usize {
        match self {
            TypeId::BOOL => 1,
            TypeId::INT => 8,
            _ => 16,
        }
    }

    pub fn align_offset(self, cur: usize) -> usize {
        Self::align(cur, self.alignment())
    }

    pub fn align_size(self, cur: usize) -> usize {
        Self::align(cur, self.alignment()) + self.size_of()
    }

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
pub struct MethodTypeData<'a> {
    name:      &'a str,
    params:    Box<[TypeId]>,
    return_ty: TypeId,
}

impl<'a> MethodTypeData<'a> {
    pub fn new(name: &'a str, params: Box<[TypeId]>, return_ty: TypeId) -> Self {
        Self {
            name,
            params,
            return_ty,
        }
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn params(&self) -> &[TypeId] {
        &self.params
    }

    pub fn return_ty(&self) -> TypeId {
        self.return_ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassTypeData<'a> {
    id:         &'a str,
    parent:     Option<TypeId>,
    attrs:      HashMap<&'a str, TypeId>,
    attrs_size: usize, // this includes the size of the parent class
    methods:    HashMap<&'a str, MethodTypeData<'a>>,
    vtable:     Vec<(&'a str, &'a str)>,
}

impl<'a> ClassTypeData<'a> {
    pub fn new(
        id: &'a str,
        parent: Option<TypeId>,
        attrs: HashMap<&'a str, TypeId>,
        attrs_size: usize,
        methods: HashMap<&'a str, MethodTypeData<'a>>,
        vtable: Vec<(&'a str, &'a str)>,
    ) -> Result<Self, TypeErrorKind<'static>> {
        match parent {
            Some(TypeId::SelfType) => Err(TypeErrorKind::CannotInheritFromSelf),
            Some(TypeId::BOOL) => Err(TypeErrorKind::CannotInheritFromBool),
            Some(TypeId::INT) => Err(TypeErrorKind::CannotInheritFromInt),
            Some(TypeId::STRING) => Err(TypeErrorKind::CannotInheritFromString),
            _ => Ok(Self {
                id,
                parent,
                attrs,
                attrs_size,
                methods,
                vtable,
            }),
        }
    }

    pub fn id(&self) -> &'a str {
        self.id
    }

    pub fn parent(&self) -> Option<TypeId> {
        self.parent
    }

    pub fn attrs(&self) -> &HashMap<&'a str, TypeId> {
        &self.attrs
    }

    pub fn attrs_size(&self) -> usize {
        self.attrs_size
    }

    pub fn methods(&self) -> &HashMap<&'a str, MethodTypeData<'a>> {
        &self.methods
    }

    pub fn vtable(&self) -> &[(&'a str, &'a str)] {
        &self.vtable
    }

    pub fn get_vtable_entry(&self, method: &str) -> Option<&str> {
        self.vtable
            .iter()
            .find_map(|(id, m)| if *m == method { Some(*id) } else { None })
    }

    pub fn get_vtable_offset(&self, method: &str) -> Option<usize> {
        self.vtable.iter().position(|(_, m)| *m == method)
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
    /// Create a new ClassEnv with the built-in classes.
    pub fn new() -> Self {
        let mut classes = vec![];

        macro_rules! builtin_method {
            ($id:literal, [ $($param:expr),* ], $return_ty:expr) => {
                ($id, MethodTypeData::new($id, Box::new([ $($param),* ]), $return_ty))
            };
        }

        macro_rules! builtin_vtable {
            ($id:literal, [ $($method:expr),* ]) => {
                vec![ ($id, "New"), ("Object", "abort"), ($id, "type_name"), ($id, "copy"), $(($id, $method)),* ]
            };
        }

        let object = ClassTypeData::new(
            "Object",
            None,
            HashMap::new(),
            0,
            HashMap::from([
                builtin_method!("abort", [], TypeId::OBJECT),
                builtin_method!("type_name", [], TypeId::STRING),
                builtin_method!("copy", [], TypeId::SelfType),
            ]),
            builtin_vtable!("Object", []),
        )
        .unwrap();
        let bool_ = ClassTypeData::new(
            "Bool",
            Some(TypeId::OBJECT),
            HashMap::new(),
            0,
            HashMap::new(),
            builtin_vtable!("Bool", []),
        )
        .unwrap();
        let int = ClassTypeData::new(
            "Int",
            Some(TypeId::OBJECT),
            HashMap::new(),
            0,
            HashMap::new(),
            builtin_vtable!("Int", []),
        )
        .unwrap();
        let string = ClassTypeData::new(
            "String",
            Some(TypeId::OBJECT),
            HashMap::new(),
            0,
            HashMap::from([
                builtin_method!("length", [], TypeId::INT),
                builtin_method!("concat", [TypeId::STRING], TypeId::STRING),
                builtin_method!("substr", [TypeId::INT, TypeId::INT], TypeId::STRING),
            ]),
            builtin_vtable!("String", ["length", "concat", "substr"]),
        )
        .unwrap();
        let io = ClassTypeData::new(
            "IO",
            Some(TypeId::OBJECT),
            HashMap::new(),
            0,
            HashMap::from([
                builtin_method!("out_string", [TypeId::STRING], TypeId::SelfType),
                builtin_method!("out_int", [TypeId::INT], TypeId::SelfType),
                builtin_method!("in_string", [], TypeId::STRING),
                builtin_method!("in_int", [], TypeId::INT),
            ]),
            vec![
                ("IO", "New"),
                ("Object", "abort"),
                ("IO", "type_name"),
                ("Object", "copy"),
                ("IO", "out_string"),
                ("IO", "out_int"),
                ("IO", "in_string"),
                ("IO", "in_int"),
            ],
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

    pub fn get_class(&self, ty: TypeId) -> Result<&ClassTypeData<'a>, TypeErrorKind<'a>> {
        Ok(self.classes.get(ty.to_index_or_err()?).unwrap())
    }

    pub fn get_class_name(&self, ty: TypeId) -> Result<&'a str, TypeErrorKind<'a>> {
        Ok(self.classes.get(ty.to_index_or_err()?).unwrap().id)
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
        data: MethodTypeData<'a>,
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

        let class_data = &mut self.classes[ty.to_index_or_err()?];

        match class_data.vtable.iter_mut().find(|(_, m)| *m == method) {
            Some((id, _)) => *id = class_data.id,
            None => class_data.vtable.push((class_data.id, method)),
        }

        match class_data.methods.insert(method, data) {
            Some(_) => Err(TypeErrorKind::RedefinedMethod(ty, method)),
            None => Ok(()),
        }
    }

    pub fn get_attribute(&self, ty: TypeId, attr: &'a str) -> Result<TypeId, TypeErrorKind<'a>> {
        let mut data = &self.classes[ty.to_index_or_err()?];

        loop {
            if let Some(data) = data.attrs.get(attr) {
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
        match self.get_attribute(ty, attr) {
            Ok(_) => Err(TypeErrorKind::RedefinedAttribute(ty, attr)),
            Err(_) => {
                let class_data = &mut self.classes[ty.to_index_or_err()?];
                match class_data.attrs.insert(attr, data) {
                    Some(_) => Err(TypeErrorKind::RedefinedAttribute(ty, attr)),
                    None => {
                        class_data.attrs_size = data.align_size(class_data.attrs_size);
                        Ok(())
                    }
                }
            }
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
        let mut env = ClassEnv::new();

        let a = Type::Class("A");
        let b = Type::Class("B");
        let c = Type::Class("C");
        let d = Type::Class("D");
        let e = Type::Class("E");

        let a_data = ClassTypeData::new(
            "A",
            Some(TypeId::OBJECT),
            HashMap::new(),
            0,
            HashMap::new(),
            vec![],
        )
        .unwrap();
        let b_data = ClassTypeData::new(
            "B",
            Some(TypeId::OBJECT),
            HashMap::new(),
            0,
            HashMap::new(),
            vec![],
        )
        .unwrap();
        let a = env.insert_class(a, a_data).unwrap();
        let b = env.insert_class(b, b_data).unwrap();

        let c_data =
            ClassTypeData::new("C", Some(a), HashMap::new(), 0, HashMap::new(), vec![]).unwrap();
        let c = env.insert_class(c, c_data).unwrap();

        let d_data =
            ClassTypeData::new("D", Some(b), HashMap::new(), 0, HashMap::new(), vec![]).unwrap();
        let e_data =
            ClassTypeData::new("E", Some(c), HashMap::new(), 0, HashMap::new(), vec![]).unwrap();
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

    #[test]
    fn test_align_0() {
        assert_eq!(TypeId::OBJECT.size_of(), 16);
        assert_eq!(TypeId::OBJECT.align_offset(0), 0);
        assert_eq!(TypeId::OBJECT.align_size(0), 16);

        assert_eq!(TypeId::INT.size_of(), 8);
        assert_eq!(TypeId::INT.align_offset(0), 0);
        assert_eq!(TypeId::INT.align_size(0), 8);

        assert_eq!(TypeId::BOOL.size_of(), 1);
        assert_eq!(TypeId::BOOL.align_offset(0), 0);
        assert_eq!(TypeId::BOOL.align_size(0), 1);

        assert_eq!(TypeId::STRING.size_of(), 16);
        assert_eq!(TypeId::STRING.align_offset(0), 0);
        assert_eq!(TypeId::STRING.align_size(0), 16);

        assert_eq!(TypeId::IO.size_of(), 16);
        assert_eq!(TypeId::IO.align_offset(0), 0);
        assert_eq!(TypeId::IO.align_size(0), 16);
    }

    #[test]
    fn test_align_8() {
        assert_eq!(TypeId::OBJECT.align_offset(8), 8);
        assert_eq!(TypeId::OBJECT.align_size(8), 24);

        assert_eq!(TypeId::INT.align_offset(8), 8);
        assert_eq!(TypeId::INT.align_size(8), 16);

        assert_eq!(TypeId::BOOL.align_offset(8), 8);
        assert_eq!(TypeId::BOOL.align_size(8), 9);

        assert_eq!(TypeId::STRING.align_offset(8), 8);
        assert_eq!(TypeId::STRING.align_size(8), 24);

        assert_eq!(TypeId::IO.align_offset(8), 8);
        assert_eq!(TypeId::IO.align_size(8), 24);
    }

    #[test]
    fn test_align_16() {
        assert_eq!(TypeId::OBJECT.align_offset(16), 16);
        assert_eq!(TypeId::OBJECT.align_size(16), 32);

        assert_eq!(TypeId::INT.align_offset(16), 16);
        assert_eq!(TypeId::INT.align_size(16), 24);

        assert_eq!(TypeId::BOOL.align_offset(16), 16);
        assert_eq!(TypeId::BOOL.align_size(16), 17);

        assert_eq!(TypeId::STRING.align_offset(16), 16);
        assert_eq!(TypeId::STRING.align_size(16), 32);

        assert_eq!(TypeId::IO.align_offset(16), 16);
        assert_eq!(TypeId::IO.align_size(16), 32);
    }

    #[test]
    fn test_align_3() {
        assert_eq!(TypeId::OBJECT.align_offset(3), 8);
        assert_eq!(TypeId::OBJECT.align_size(3), 24);

        assert_eq!(TypeId::INT.align_offset(3), 8);
        assert_eq!(TypeId::INT.align_size(3), 16);

        assert_eq!(TypeId::BOOL.align_offset(3), 3);
        assert_eq!(TypeId::BOOL.align_size(3), 4);

        assert_eq!(TypeId::STRING.align_offset(3), 8);
        assert_eq!(TypeId::STRING.align_size(3), 24);

        assert_eq!(TypeId::IO.align_offset(3), 8);
        assert_eq!(TypeId::IO.align_size(3), 24);
    }

    #[test]
    fn test_align_13() {
        assert_eq!(TypeId::OBJECT.align_offset(13), 16);
        assert_eq!(TypeId::OBJECT.align_size(13), 32);

        assert_eq!(TypeId::INT.align_offset(13), 16);
        assert_eq!(TypeId::INT.align_size(13), 24);

        assert_eq!(TypeId::BOOL.align_offset(13), 13);
        assert_eq!(TypeId::BOOL.align_size(13), 14);

        assert_eq!(TypeId::STRING.align_offset(13), 16);
        assert_eq!(TypeId::STRING.align_size(13), 32);

        assert_eq!(TypeId::IO.align_offset(13), 16);
        assert_eq!(TypeId::IO.align_size(13), 32);
    }
}
