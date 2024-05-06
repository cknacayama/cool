use std::{collections::HashMap, fs};

use crate::{
    ast::{
        BinOp, TypedAttribute, TypedClass, TypedExpr, TypedExprKind, TypedFormal, TypedMethod, UnOp,
    },
    types::{ClassEnv, ClassTypeData, TypeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Reg {
    Rax,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
}

impl Reg {
    pub fn to_string(&self) -> &'static str {
        match self {
            Reg::Rax => "rax",
            Reg::Rcx => "rcx",
            Reg::Rdx => "rdx",
            Reg::Rsi => "rsi",
            Reg::Rdi => "rdi",
            Reg::R8 => "r8",
            Reg::R9 => "r9",
            Reg::R10 => "r10",
            Reg::R11 => "r11",
        }
    }

    pub fn to_byte_string(&self) -> &'static str {
        match self {
            Reg::Rax => "al",
            Reg::Rcx => "cl",
            Reg::Rdx => "dl",
            Reg::Rsi => "sil",
            Reg::Rdi => "dil",
            Reg::R8 => "r8b",
            Reg::R9 => "r9b",
            Reg::R10 => "r10b",
            Reg::R11 => "r11b",
        }
    }

    pub fn to_index(&self) -> usize {
        match self {
            Reg::Rax => 0,
            Reg::Rcx => 1,
            Reg::Rdx => 2,
            Reg::Rsi => 3,
            Reg::Rdi => 4,
            Reg::R8 => 5,
            Reg::R9 => 6,
            Reg::R10 => 7,
            Reg::R11 => 8,
        }
    }

    pub fn from_index(index: usize) -> Option<Self> {
        match index {
            0 => Some(Reg::Rax),
            1 => Some(Reg::Rcx),
            2 => Some(Reg::Rdx),
            3 => Some(Reg::Rsi),
            4 => Some(Reg::Rdi),
            5 => Some(Reg::R8),
            6 => Some(Reg::R9),
            7 => Some(Reg::R10),
            8 => Some(Reg::R11),
            _ => None,
        }
    }

    pub fn arg_reg(index: usize) -> Option<Self> {
        match index {
            0 => Some(Reg::Rdi),
            1 => Some(Reg::Rsi),
            2 => Some(Reg::Rdx),
            3 => Some(Reg::Rcx),
            4 => Some(Reg::R8),
            5 => Some(Reg::R9),
            _ => None,
        }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug)]
pub struct CodeGenerator<'a> {
    data_section:     String,
    text_section:     String,
    locals:           Vec<HashMap<&'a str, (TypeId, usize)>>,
    class_env:        ClassEnv<'a>,
    cur_class:        TypeId,
    cur_stack_offset: usize,
    cur_str_label:    usize,
    cur_label:        usize,
    regs:             [bool; 8],
}

macro_rules! mov_regs {
    ($r1:expr => $r11:expr, $r2:expr => $r22:expr, $_self:expr) => {
        let r1 = $r1;
        let r2 = $r2;
        let r11 = $r11;
        let r22 = $r22;
        if r1 == r22 && r2 == r11 {
            let r3 = $_self.alloc_reg_filter(&[r11, r22]).unwrap();
            $_self.push_text(&format!(
                r"    mov     {r3}, {r2}
    mov     {r11}, {r1}
    mov     {r22}, {r3}
"
            ));
            $_self.free_reg(r3);
        } else if r1 == r22 {
            $_self.push_text(&format!(
                r"    mov     {r11}, {r1}
    mov     {r22}, {r2}
"
            ));
        } else {
            $_self.push_text(&format!(
                r"    mov     {r22}, {r2}
    mov     {r11}, {r1}
"
            ));
        }
    };
}

impl<'a> CodeGenerator<'a> {
    pub fn new(class_env: ClassEnv<'a>) -> Self {
        Self {
            locals: Vec::new(),
            class_env,
            cur_class: TypeId::SelfType,
            cur_stack_offset: 0,
            cur_str_label: 0,
            cur_label: 0,
            data_section: String::new(),
            text_section: String::new(),
            regs: [true; 8],
        }
    }

    fn alloc_reg(&mut self) -> Option<Reg> {
        for (i, reg) in self.regs.iter_mut().enumerate() {
            if *reg {
                *reg = false;
                return Reg::from_index(i);
            }
        }
        None
    }

    fn alloc_reg_filter(&mut self, filter: &[Reg]) -> Option<Reg> {
        for (i, reg) in self.regs.iter_mut().enumerate() {
            let r = Reg::from_index(i).unwrap();
            if *reg && !filter.contains(&r) {
                *reg = false;
                return Some(r);
            }
        }
        None
    }

    fn alloc_reg_no_ret(&mut self) -> Option<Reg> {
        for (i, reg) in self.regs.iter_mut().enumerate() {
            if i == Reg::Rax.to_index() || i == Reg::Rdx.to_index() {
                continue;
            }
            if *reg {
                *reg = false;
                return Reg::from_index(i);
            }
        }
        None
    }

    fn reserve_reg(&mut self, reg: Reg) -> bool {
        if self.regs[reg.to_index()] {
            self.regs[reg.to_index()] = false;
            true
        } else {
            false
        }
    }

    fn free_reg(&mut self, reg: Reg) {
        self.regs[reg.to_index()] = true;
    }

    fn free_all_regs(&mut self) {
        for reg in self.regs.iter_mut() {
            *reg = true;
        }
    }

    fn begin_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.locals.pop();
    }

    fn insert_local(&mut self, id: &'a str, ty: TypeId, offset: usize) {
        self.locals.last_mut().unwrap().insert(id, (ty, offset));
    }

    fn get_local(&self, id: &'a str) -> Option<(TypeId, usize)> {
        for scope in self.locals.iter().rev() {
            if let Some(local) = scope.get(id) {
                return Some(*local);
            }
        }
        None
    }

    fn push_data(&mut self, data: &str) {
        self.data_section.push_str(data);
    }

    fn push_text(&mut self, text: &str) {
        self.text_section.push_str(text);
    }

    fn push_type_name(&mut self, id: &'a str) {
        let len = id.len();
        self.push_data(&format!(
            r#"
{id}_Typename:
    .string "{id}"
    .align 8
{id}_Typenamelen:
    .quad {len}
"#,
        ));
        self.push_text(&format!(
            r"    .globl  {id}_type_name
{id}_type_name:
    mov     rax, QWORD PTR [rip + {id}_Typenamelen]
    lea     rdx, QWORD PTR [rip + {id}_Typename]
    ret
"
        ));
    }

    fn push_vtable(&mut self) {
        let class = self.class_env.get_class(self.cur_class).unwrap();
        let id = class.id();
        let vtable = class.vtable();
        self.data_section.push_str(&format!("\n{id}_Table:\n"));
        for (class, method) in vtable.iter() {
            self.data_section
                .push_str(&format!("    .quad {class}_{method}\n"));
        }
    }

    pub fn gen_class(&mut self, class: &TypedClass<'a>) {
        self.cur_class = class.type_id;

        self.push_type_name(class.id);
        self.push_vtable();

        let data = self.class_env.get_class(class.type_id).unwrap();
        let parent_data = self.class_env.get_class(class.parent).unwrap();

        self.gen_new(class, data.attrs_size(), parent_data.id());

        for method in class.methods.iter() {
            self.gen_method(method)
        }
    }

    fn gen_method(&mut self, method: &TypedMethod<'a>) {
        let class = self.class_env.get_class(self.cur_class).unwrap();

        let size = method.size();

        self.push_text(&format!(
            r"    .globl  {class_id}_{method_name}
{class_id}_{method_name}:
    push    rbp
    mov     rbp, rsp

    sub     rsp, {size}

    mov     QWORD PTR [rbp - 8], rdi
    mov     QWORD PTR [rbp - 16], rsi
",
            class_id = class.id(),
            method_name = method.id(),
        ));

        self.begin_scope();
        self.cur_stack_offset = 16;

        let mut i = 2;
        for param in method.params() {
            i = self.gen_param(i, param);
        }

        let (_, r1, r2) = self.gen_expr(method.body());

        self.gen_ret(r1, r2, size);
    }

    fn gen_ret(&mut self, r1: Reg, r2: Option<Reg>, size: usize) {
        match (r1, r2) {
            (_, None) => {
                self.push_text(&format!("    mov     rax, {r1}\n"));
            }
            _ => {
                let r2 = r2.unwrap();
                mov_regs!(r1 => Reg::Rax, r2 => Reg::Rdx, self);
            }
        }

        self.push_text(&format!(
            r"    add     rsp, {size}
    pop     rbp
    ret
"
        ));

        self.end_scope();

        self.free_reg(r1);
        if let Some(r2) = r2 {
            self.free_reg(r2);
        }
    }

    fn gen_param(&mut self, i: usize, param: &TypedFormal<'a>) -> usize {
        let offset = param.ty.align_offset(self.cur_stack_offset);
        self.cur_stack_offset = offset + param.ty.size_of();
        self.insert_local(param.id, param.ty, offset);
        match param.ty {
            TypeId::INT => {
                self.push_text(&format!(
                    r"    mov     QWORD PTR [rbp - {offset}], {reg}
",
                    reg = Reg::arg_reg(i).unwrap()
                ));
                i + 1
            }
            TypeId::BOOL => {
                self.push_text(&format!(
                    r"    mov     BYTE PTR [rbp - {offset}], {reg}
",
                    reg = Reg::arg_reg(i).unwrap().to_byte_string()
                ));
                i + 1
            }
            _ => {
                self.push_text(&format!(
                    r"    mov     QWORD PTR [rbp - {offset1}], {r1}
    mov     QWORD PTR [rbp - {offset2}], {r2}
",
                    r1 = Reg::arg_reg(i).unwrap(),
                    r2 = Reg::arg_reg(i + 1).unwrap(),
                    offset1 = offset,
                    offset2 = offset + 8
                ));
                i + 2
            }
        }
    }

    fn gen_new(&mut self, class: &TypedClass<'a>, attrs_size: usize, parent_id: &'a str) {
        self.begin_scope();
        self.text_section.push_str(&format!(
            r"    .globl  {id}_New
{id}_New:
    push    rbp
    mov     rbp, rsp

    sub     rsp, {stack_size}

    cmp     rdi, 0

    jne     .{id}_New_L1

    mov     rdi, {attrs_size}
    call    allocator_alloc
    mov     QWORD PTR [rbp - 8], rax

    jmp     .{id}_New_L2
.{id}_New_L1:
    mov     QWORD PTR [rbp - 8], rdi
.{id}_New_L2:
    mov     rdi, QWORD PTR [rbp - 8]
    call    {parent_id}_New
",
            id = class.id,
            stack_size = class.init_size,
        ));

        self.cur_stack_offset = 8;

        for attr in class.attrs.iter() {
            self.gen_attr(attr);
        }

        self.text_section.push_str(&format!(
            r"    mov     rax, QWORD PTR [rbp - 8]
    lea     rdx, QWORD PTR [rip + {id}_Table]
    add     rsp, {stack_size}
    pop     rbp
    ret
",
            id = class.id,
            stack_size = class.init_size
        ));

        self.end_scope();
    }

    fn gen_attr(&mut self, attr: &TypedAttribute<'a>) {
        match attr.init() {
            Some(ast) => match self.gen_expr(ast) {
                (TypeId::INT, r1, _) if attr.ty().is_int() => {
                    let r2 = self.alloc_reg().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     {r2}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r2} + {offset}], {r1}
",
                        offset = attr.offset(),
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                }
                (TypeId::INT, r1, _) => {
                    let r2 = self.alloc_reg_no_ret().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     rdi, {r1}
    call    Int_To_Object

    mov     {r2}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r2} + {offset1}], rax
    mov     QWORD PTR [{r2} + {offset2}], rdx
",
                        offset1 = attr.offset(),
                        offset2 = attr.offset() + 8,
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                }
                (TypeId::BOOL, r1, _) if attr.ty().is_bool() => {
                    let r2 = self.alloc_reg().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     {r2}, QWORD PTR [rbp - 8]
    mov     BYTE PTR [{r2} + {offset}], {r1}
",
                        offset = attr.offset(),
                        r1 = r1.to_byte_string()
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                }
                (TypeId::BOOL, r1, _) => {
                    let r2 = self.alloc_reg_no_ret().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     rdi, {r1}
    call    Bool_To_Object

    mov     {r2}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r2} + {offset1}], rax
    mov     QWORD PTR [{r2} + {offset2}], rdx
",
                        offset1 = attr.offset(),
                        offset2 = attr.offset() + 8,
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                }
                (TypeId::STRING, r1, Some(r2)) if attr.ty().is_string() => {
                    let r3 = self.alloc_reg().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     {r3}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r3} + {offset1}], {r1}
    mov     QWORD PTR [{r3} + {offset2}], {r2}
",
                        offset1 = attr.offset(),
                        offset2 = attr.offset() + 8,
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                    self.free_reg(r3);
                }
                (TypeId::STRING, r1, Some(r2)) => {
                    let r3 = self.alloc_reg_no_ret().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     rdi, {r1}
    mov     rsi, {r2}
    call    String_To_Object

    mov     {r3}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r3} + {offset1}], rax
    mov     QWORD PTR [{r3} + {offset2}], rdx
",
                        offset1 = attr.offset(),
                        offset2 = attr.offset() + 8,
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                    self.free_reg(r3);
                }
                (_, r1, Some(r2)) => {
                    let r3 = self.alloc_reg().unwrap();
                    self.text_section.push_str(&format!(
                        r"    mov     {r3}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r3} + {offset1}], {r1}
    mov     QWORD PTR [{r3} + {offset2}], {r2}
",
                        offset1 = attr.offset(),
                        offset2 = attr.offset() + 8,
                    ));
                    self.free_reg(r1);
                    self.free_reg(r2);
                    self.free_reg(r3);
                }
                _ => unreachable!(),
            },
            None => self.gen_default_init(attr.ty(), attr.offset()),
        }
    }

    fn gen_default_init(&mut self, ty: TypeId, offset: usize) {
        match ty {
            TypeId::INT => {
                let r = self.alloc_reg().unwrap();
                self.text_section.push_str(&format!(
                    r"    mov     {r}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r} + {offset}], 0
"
                ));
                self.free_reg(r);
            }
            TypeId::BOOL => {
                let r = self.alloc_reg().unwrap();
                self.text_section.push_str(&format!(
                    r"    mov     {r}, QWORD PTR [rbp - 8]
    mov     BYTE PTR [{r} + {offset}], 0
"
                ));
                self.free_reg(r);
            }
            TypeId::STRING => {
                let r = self.alloc_reg_no_ret().unwrap();
                self.text_section.push_str(&format!(
                    r"    call    String_New

    mov     {r}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r} + {offset1}], rax
    mov     QWORD PTR [{r} + {offset2}], rdx
",
                    offset1 = offset,
                    offset2 = offset + 8
                ));
                self.free_reg(r);
            }
            _ => {
                let r = self.alloc_reg().unwrap();
                self.text_section.push_str(&format!(
                    r"    mov     {r}, QWORD PTR [rbp - 8]
    mov     QWORD PTR [{r} + {offset1}], 0
    mov     QWORD PTR [{r} + {offset2}], 0
",
                    offset1 = offset,
                    offset2 = offset + 8
                ));
                self.free_reg(r);
            }
        }
    }

    fn gen_expr(&mut self, expr: &TypedExpr<'a>) -> (TypeId, Reg, Option<Reg>) {
        use TypedExprKind as TEK;
        let type_id = expr.ty;
        match &expr.kind {
            TEK::IntLit(i) => {
                let reg = self.alloc_reg().unwrap();
                self.push_text(&format!("    mov     {}, {}\n", reg.to_string(), i));
                (type_id, reg, None)
            }
            TEK::BoolLit(b) => {
                let reg = self.alloc_reg().unwrap();
                self.push_text(&format!("    mov     {}, {}\n", reg.to_string(), *b as i64));
                (type_id, reg, None)
            }
            TEK::StringLit(s) => {
                let r1 = self.alloc_reg().unwrap();
                let r2 = self.alloc_reg().unwrap();
                self.push_data(&format!(
                    r"_string_{label}:
    .string {s:?}
    .align 8
_string_{label}_len:
    .quad {len}",
                    label = self.cur_str_label,
                    len = s.len(),
                ));
                self.push_text(&format!(
                    r"    mov     {r1}, QWORD PTR [rip + _string_{label}_len] 
    lea     {r2}, QWORD PTR [rip + _string_{label}]
",
                    label = self.cur_str_label,
                ));
                self.cur_str_label += 1;
                (type_id, r1, Some(r2))
            }

            TEK::Unary(op, e) => self.gen_unary(*op, e),
            TEK::Binary(op, lhs, rhs) => self.gen_binary(*op, lhs, rhs),
            TEK::Dispatch(e, method, args) => self.gen_dispatch(e, method, args, type_id),
            TEK::SelfDispatch(method, args) => self.gen_dispatch_self(method, args, type_id),
            TEK::New(ty) => self.gen_new_expr(*ty),
            _ => todo!(),
        }
    }

    fn gen_new_expr(&mut self, ty: TypeId) -> (TypeId, Reg, Option<Reg>) {
        let id = self.class_env.get_class(ty).unwrap().id();
        self.push_text(&format!("    mov     rdi, 0\n    call    {id}_New\n"));
        self.reserve_reg(Reg::Rax);
        match ty {
            TypeId::INT | TypeId::BOOL => (ty, Reg::Rax, None),
            _ => {
                self.reserve_reg(Reg::Rdx);
                (ty, Reg::Rax, Some(Reg::Rdx))
            }
        }
    }

    fn gen_dispatch_self(
        &mut self,
        method: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        let mut i = 2;
        self.push_text(&format!(
            r"    mov     rdi, QWORD PTR [rbp - 8]
    mov     rsi, QWORD PTR [rbp - 16]
"
        ));
        self.reserve_reg(Reg::Rdi);
        self.reserve_reg(Reg::Rsi);
        for arg in args {
            i = self.gen_arg(i, arg);
        }
        let class = self.class_env.get_class(self.cur_class).unwrap();
        let method_offset = class.get_vtable_offset(method).unwrap() * 8;
        let r = self.alloc_reg().unwrap();
        self.push_text(&format!(
            r"    mov     {r}, QWORD PTR [rsi + {method_offset}]
    call    {r}
"
        ));
        self.free_all_regs();
        match ret_ty {
            TypeId::INT | TypeId::BOOL => {
                self.reserve_reg(Reg::Rax);
                (ret_ty, Reg::Rax, None)
            }
            _ => {
                self.reserve_reg(Reg::Rax);
                self.reserve_reg(Reg::Rdx);
                (ret_ty, Reg::Rax, Some(Reg::Rdx))
            }
        }
    }

    fn gen_dispatch(
        &mut self,
        e: &TypedExpr<'a>,
        method: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r1, r2) = self.gen_expr(e);
        let mut i = match e.ty {
            TypeId::INT => return self.gen_dispatch_int(r1, method, args, ret_ty),
            TypeId::BOOL => return self.gen_dispatch_bool(r1, method, args, ret_ty),
            TypeId::STRING => {
                return self.gen_dispatch_string(r1, r2.unwrap(), method, args, ret_ty)
            }
            _ => 2,
        };
        let r2 = r2.unwrap();
        mov_regs!(r1 => Reg::Rdi, r2 => Reg::Rsi, self);
        self.free_reg(r1);
        self.free_reg(r2);
        self.reserve_reg(Reg::Rdi);
        self.reserve_reg(Reg::Rsi);
        for arg in args {
            i = self.gen_arg(i, arg);
        }
        let class = self.class_env.get_class(e.ty).unwrap();
        let method_offset = class.get_vtable_offset(method).unwrap() * 8;
        let r = self.alloc_reg().unwrap();
        self.push_text(&format!(
            r"    mov     {r}, QWORD PTR [rsi + {method_offset}]
    call    {r}
"
        ));
        self.free_all_regs();
        match ret_ty {
            TypeId::INT | TypeId::BOOL => {
                self.reserve_reg(Reg::Rax);
                (ret_ty, Reg::Rax, None)
            }
            _ => {
                self.reserve_reg(Reg::Rax);
                self.reserve_reg(Reg::Rdx);
                (ret_ty, Reg::Rax, Some(Reg::Rdx))
            }
        }
    }

    fn gen_dispatch_string(
        &mut self,
        r1: Reg,
        r2: Reg,
        method: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        mov_regs!(r1 => Reg::Rdi, r2 => Reg::Rsi, self);
        self.free_reg(r1);
        self.free_reg(r2);
        self.reserve_reg(Reg::Rdi);
        self.reserve_reg(Reg::Rsi);
        let method_class = self
            .class_env
            .get_class(TypeId::STRING)
            .unwrap()
            .get_vtable_entry(method)
            .unwrap();
        let call = format!("{}_{}", method_class, method);
        if method_class != "String" {
            self.push_text(&format!(
                r"    call    String_To_Object
    mov     rdi, rax
    mov     rsi, rdx
"
            ));
        }
        let mut i = 2;
        for arg in args {
            i = self.gen_arg(i, arg);
        }
        self.push_text(&call);
        self.free_all_regs();
        match ret_ty {
            TypeId::INT | TypeId::BOOL => {
                self.reserve_reg(Reg::Rax);
                (ret_ty, Reg::Rax, None)
            }
            _ => {
                self.reserve_reg(Reg::Rax);
                self.reserve_reg(Reg::Rdx);
                (ret_ty, Reg::Rax, Some(Reg::Rdx))
            }
        }
    }

    fn gen_dispatch_int(
        &mut self,
        r: Reg,
        method: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        self.push_text(&format!("    mov     rdi, {r}\n"));
        self.free_reg(r);
        self.reserve_reg(Reg::Rdi);
        let method_class = self
            .class_env
            .get_class(TypeId::INT)
            .unwrap()
            .get_vtable_entry(method)
            .unwrap();
        let call = format!("{}_{}", method_class, method);
        let mut i = if method_class != "Int" {
            self.push_text(&format!(
                r"    call    Int_To_Object
    mov     rdi, rax
    mov     rsi, rdx
"
            ));
            self.reserve_reg(Reg::Rsi);
            2
        } else {
            1
        };
        for arg in args {
            i = self.gen_arg(i, arg);
        }
        self.push_text(&call);
        self.free_all_regs();
        match ret_ty {
            TypeId::INT | TypeId::BOOL => {
                self.reserve_reg(Reg::Rax);
                (ret_ty, Reg::Rax, None)
            }
            _ => {
                self.reserve_reg(Reg::Rax);
                self.reserve_reg(Reg::Rdx);
                (ret_ty, Reg::Rax, Some(Reg::Rdx))
            }
        }
    }

    fn gen_dispatch_bool(
        &mut self,
        r: Reg,
        method: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        self.push_text(&format!("    mov     rdi, {r}\n"));
        self.free_reg(r);
        self.reserve_reg(Reg::Rdi);
        let method_class = self
            .class_env
            .get_class(TypeId::BOOL)
            .unwrap()
            .get_vtable_entry(method)
            .unwrap();
        let call = format!("{}_{}", method_class, method);
        let mut i = if method_class != "Int" {
            self.push_text(&format!(
                r"    call    Bool_To_Object
    mov     rdi, rax
    mov     rsi, rdx
"
            ));
            self.reserve_reg(Reg::Rsi);
            2
        } else {
            1
        };
        for arg in args {
            i = self.gen_arg(i, arg);
        }
        self.push_text(&call);
        self.free_all_regs();
        match ret_ty {
            TypeId::INT | TypeId::BOOL => {
                self.reserve_reg(Reg::Rax);
                (ret_ty, Reg::Rax, None)
            }
            _ => {
                self.reserve_reg(Reg::Rax);
                self.reserve_reg(Reg::Rdx);
                (ret_ty, Reg::Rax, Some(Reg::Rdx))
            }
        }
    }

    fn gen_arg(&mut self, i: usize, arg: &TypedExpr<'a>) -> usize {
        if i >= 6 {
            todo!("Implement stack arguments");
        }
        let (ty, r1, r2) = self.gen_expr(arg);
        let arg1 = Reg::arg_reg(i).unwrap();
        match ty {
            TypeId::INT | TypeId::BOOL => {
                self.push_text(&format!("    mov     {arg1}, {r1}\n",));
                self.free_reg(r1);
                self.reserve_reg(arg1);
                i + 1
            }
            _ => {
                let r2 = r2.unwrap();
                let arg2 = Reg::arg_reg(i + 1).unwrap();
                mov_regs!(r1 => arg1, r2 => arg2, self);
                self.free_reg(r1);
                self.free_reg(r2);
                self.reserve_reg(arg1);
                self.reserve_reg(arg2);
                i + 2
            }
        }
    }

    fn gen_unary(&mut self, op: UnOp, e: &TypedExpr<'a>) -> (TypeId, Reg, Option<Reg>) {
        let (ty, r1, r2) = self.gen_expr(e);
        match op {
            UnOp::Complement => {
                self.push_text(&format!("    neg     {r1}\n"));
                (ty, r1, None)
            }
            UnOp::Not => {
                self.push_text(&format!("    not     {r1}\n"));
                (ty, r1, None)
            }
            UnOp::IsVoid => match e.ty {
                TypeId::INT | TypeId::BOOL => {
                    self.push_text(&format!("mov     {r1}, 0\n"));
                    (TypeId::BOOL, r1, None)
                }
                TypeId::STRING => {
                    self.push_text(&format!("mov     {r1}, 0\n"));
                    self.free_reg(r2.unwrap());
                    (TypeId::BOOL, r1, None)
                }
                _ => {
                    self.push_text(&format!(
                        r"    cmp     {r1}, 0
    sete    {}
",
                        r1.to_byte_string()
                    ));
                    self.free_reg(r2.unwrap());
                    (TypeId::BOOL, r1, None)
                }
            },
        }
    }

    fn gen_binary(
        &mut self,
        op: BinOp,
        lhs: &TypedExpr<'a>,
        rhs: &TypedExpr<'a>,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (ty1, r1, r2) = self.gen_expr(lhs);
        let (_, mut r3, r4) = self.gen_expr(rhs);
        match op {
            BinOp::Add => {
                self.push_text(&format!("    add     {r1}, {r3}\n"));
                self.free_reg(r3);
                (TypeId::INT, r1, None)
            }
            BinOp::Sub => {
                self.push_text(&format!("    sub     {r1}, {r3}\n"));
                self.free_reg(r3);
                (TypeId::INT, r1, None)
            }
            BinOp::Mul => {
                self.push_text(&format!("    imul    {r1}, {r3}\n"));
                self.free_reg(r3);
                (TypeId::INT, r1, None)
            }
            BinOp::Div => {
                if matches!(r3, Reg::Rax | Reg::Rdx) {
                    let r5 = self.alloc_reg_no_ret().unwrap();
                    self.push_text(&format!("    mov     {r5}, {r3}\n"));
                    self.free_reg(r3);
                    r3 = r5;
                }
                self.push_text(&format!(
                    r"    mov     rax, {r1}
    cqo
    idiv    {r3}
    mov     {r1}, rax
"
                ));
                self.free_reg(r3);
                (TypeId::INT, r1, None)
            }

            BinOp::Lt => {
                self.push_text(&format!(
                    r"    cmp     {r1}, {r3}
    setl    {r1_to_byte}
",
                    r1_to_byte = r1.to_byte_string()
                ));
                self.free_reg(r3);
                (TypeId::BOOL, r1, None)
            }
            BinOp::Le => {
                self.push_text(&format!(
                    r"    cmp     {r1}, {r3}
    setle   {r1_to_byte}
",
                    r1_to_byte = r1.to_byte_string()
                ));
                self.free_reg(r3);
                (TypeId::BOOL, r1, None)
            }
            BinOp::Eq => self.gen_eq(ty1, r1, r2, r3, r4),
        }
    }

    fn gen_eq(
        &mut self,
        ty: TypeId,
        r1: Reg,
        r2: Option<Reg>,
        r3: Reg,
        r4: Option<Reg>,
    ) -> (TypeId, Reg, Option<Reg>) {
        match ty {
            TypeId::INT | TypeId::BOOL => {
                self.push_text(&format!(
                    r"    cmp     {r1}, {r3}
    sete    {r1_to_byte}
",
                    r1_to_byte = r1.to_byte_string()
                ));
                self.free_reg(r3);
                (TypeId::BOOL, r1, None)
            }
            TypeId::STRING => {
                let r2 = r2.unwrap();
                let r4 = r4.unwrap();
                self.push_text(&format!(
                    r"    cmp     {r1}, {r3}
    jne     .L{label}
    sub     rsp, 24
    mov     QWORD PTR [rsp], {r1}
    mov     QWORD PTR [rsp + 8], {r2}
    mov     QWORD PTR [rsp + 16], {r4}
    mov     rdi, QWORD PTR [rsp + 8]
    mov     rsi, QWORD PTR [rsp + 16]
    mov     rdx, QWORD PTR [rsp]
    call    String_Equal
    add     rsp, 24
    cmp     rax, 0
    sete    {r1_to_byte}
    jmp     .L{label_end}
.L{label}:
    mov     {r1}, 0
.L{label_end}:
",
                    label = self.cur_label,
                    label_end = self.cur_label + 1,
                    r1_to_byte = r1.to_byte_string()
                ));
                self.cur_label += 2;
                self.free_reg(r2);
                self.free_reg(r3);
                self.free_reg(r4);
                (TypeId::BOOL, r1, None)
            }
            _ => {
                let r2 = r2.unwrap();
                let r4 = r4.unwrap();
                self.push_text(&format!(
                    r"    cmp     {r1}, {r3}
    sete    {r1_to_byte}
",
                    r1_to_byte = r1.to_byte_string()
                ));
                self.free_reg(r2);
                self.free_reg(r3);
                self.free_reg(r4);
                (TypeId::BOOL, r1, None)
            }
        }
    }

    pub fn output(&self) -> String {
        let mut output = fs::read_to_string("lib/text_section.s").unwrap();
        output.push_str(&self.text_section);
        output.push_str(&fs::read_to_string("lib/data_section.s").unwrap());
        output.push_str(&self.data_section);
        output.push('\n');
        output
    }

    pub fn take_output(self) -> String {
        self.output()
    }
}
