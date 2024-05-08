use std::{collections::HashMap, fs};

use crate::{
    ast::{
        BinOp, TypedAttribute, TypedCaseArm, TypedClass, TypedExpr, TypedFormal, TypedMethod, UnOp,
    },
    types::{ClassEnv, TypeId},
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

    pub fn to_dword_string(&self) -> &'static str {
        match self {
            Reg::Rax => "eax",
            Reg::Rcx => "ecx",
            Reg::Rdx => "edx",
            Reg::Rsi => "esi",
            Reg::Rdi => "edi",
            Reg::R8 => "r8d",
            Reg::R9 => "r9d",
            Reg::R10 => "r10d",
            Reg::R11 => "r11d",
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
    locals:           Vec<HashMap<&'a str, (TypeId, isize)>>,
    attrs:            HashMap<&'a str, (TypeId, usize)>,
    class_env:        ClassEnv<'a>,
    cur_class:        TypeId,
    cur_stack_offset: usize,
    cur_str_label:    usize,
    cur_label:        usize,
    cur_jmp_table:    usize,
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
            $_self.gen_mov(r22, r2);
            $_self.gen_mov(r11, r1);
        }
    };
}

impl<'a> CodeGenerator<'a> {
    pub fn new(class_env: ClassEnv<'a>) -> Self {
        Self {
            locals: Vec::new(),
            attrs: HashMap::new(),
            class_env,
            cur_class: TypeId::SelfType,
            cur_stack_offset: 0,
            cur_str_label: 0,
            cur_label: 0,
            cur_jmp_table: 0,
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

    fn reserve_or_alloc_reg(&mut self, reg: Reg) -> Reg {
        if !self.regs[reg.to_index()] {
            self.alloc_reg().unwrap()
        } else {
            self.regs[reg.to_index()] = false;
            reg
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

    fn insert_param(&mut self, id: &'a str, ty: TypeId, i: usize) {
        let offset = i * 8 + 16;
        self.locals
            .last_mut()
            .unwrap()
            .insert(id, (ty, offset as isize));
    }

    fn insert_local(&mut self, id: &'a str, ty: TypeId) -> isize {
        let offset = ty.align_offset(self.cur_stack_offset);
        self.cur_stack_offset = offset + ty.size_of();
        let offset = -(offset as isize);
        self.locals.last_mut().unwrap().insert(id, (ty, offset));
        offset
    }

    fn insert_attr(&mut self, id: &'a str, ty: TypeId, offset: usize) {
        self.attrs.insert(id, (ty, offset));
    }

    fn get_local(&self, id: &'a str) -> Option<(TypeId, isize)> {
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

    fn gen_comment(&mut self, comment: &str) {
        self.push_text("    # ");
        self.push_text(comment);
        self.text_section.push('\n');
    }

    fn gen_mov(&mut self, dst: Reg, src: Reg) {
        if dst != src {
            self.push_text(&format!("    mov     {dst}, {src}\n"));
        }
    }

    fn gen_rsp_add(&mut self, size: usize) {
        if size != 0 {
            let size = TypeId::align(size, 8);
            self.push_text(&format!("    add     rsp, {}\n", size));
        }
    }

    fn gen_rsp_sub(&mut self, size: usize) {
        if size != 0 {
            let size = TypeId::align(size, 8);
            self.push_text(&format!("    sub     rsp, {}\n", size));
        }
    }

    fn gen_type_name(&mut self, id: &'a str) {
        let len = id.len();
        self.push_data(&format!(
            r#"
{id}.Typename:
    .string "{id}"
    .align 8
{id}.Typenamelen:
    .quad {len}
"#,
        ));
        self.push_text(&format!(
            r"    
    .globl  {id}.type_name
{id}.type_name:
    mov     rax, QWORD PTR [rip + {id}.Typenamelen]
    lea     rdx, QWORD PTR [rip + {id}.Typename]
    ret
"
        ));
    }

    fn gen_vtable(&mut self) {
        let class = self.class_env.get_class(self.cur_class).unwrap();
        let id = class.id();
        let vtable = class.vtable();
        self.data_section.push_str(&format!("\n{id}.Table:\n"));
        for (class, method) in vtable.iter() {
            self.data_section
                .push_str(&format!("    .quad {class}.{method}\n"));
        }
    }

    pub fn gen_class(&mut self, class: &TypedClass<'a>) {
        self.cur_class = class.type_id;

        self.gen_type_name(class.id);
        self.gen_vtable();

        let data = self.class_env.get_class(class.type_id).unwrap();
        let parent_data = self.class_env.get_class(class.parent).unwrap();

        self.gen_new(class, data.attrs_size(), parent_data.id());

        for method in class.methods.iter() {
            self.gen_method(method)
        }

        self.free_all_regs();
        self.attrs.clear();
    }

    fn gen_method(&mut self, method: &TypedMethod<'a>) {
        let class = self.class_env.get_class(self.cur_class).unwrap();

        let size = method.size();

        self.push_text(&format!(
            r"
    .globl  {class_id}.{method_name}
{class_id}.{method_name}:
    push    rbp
    mov     rbp, rsp
",
            class_id = class.id(),
            method_name = method.id(),
        ));
        self.gen_rsp_sub(size);

        self.begin_scope();
        self.cur_stack_offset = 8;

        self.insert_param("self", self.cur_class, 0);

        let mut i = 2;
        for param in method.params() {
            i = self.gen_param(i, param);
        }

        let (_, r1, r2) = self.gen_expr(method.body());

        self.gen_method_ret(r1, r2, size);
    }

    fn gen_method_ret(&mut self, r1: Reg, r2: Option<Reg>, size: usize) {
        match (r1, r2) {
            (_, None) => {
                self.gen_mov(Reg::Rax, r1);
            }
            _ => {
                let r2 = r2.unwrap();
                mov_regs!(r1 => Reg::Rax, r2 => Reg::Rdx, self);
            }
        }

        self.gen_rsp_add(size);
        self.push_text(
            r"
    pop     rbp
    ret
",
        );

        self.end_scope();

        self.free_reg(r1);
        if let Some(r2) = r2 {
            self.free_reg(r2);
        }
    }

    fn gen_param(&mut self, i: usize, param: &TypedFormal<'a>) -> usize {
        self.insert_param(param.id, param.ty, i);
        match param.ty {
            TypeId::INT | TypeId::BOOL => i + 1,
            _ => i + 2,
        }
    }

    fn gen_new(&mut self, class: &TypedClass<'a>, attrs_size: usize, parent_id: &'a str) {
        self.begin_scope();
        self.text_section.push_str(&format!(
            r"    
    .globl  {id}.New
{id}.New:
    push    rbp
    mov     rbp, rsp
    sub     rsp, {stack_size}
    cmp     QWORD PTR [rbp + 16], 0
    jne     .{id}.New_L1
    push    {attrs_size}
    call    allocator.alloc
    add     rsp, 8
    jmp     .{id}.New_L2
.{id}.New_L1:
    mov     rax, QWORD PTR [rbp + 16]
.{id}.New_L2:
    push    rax
    call    {parent_id}.New
    add     rsp, 8
    mov     QWORD PTR [rbp - 8], rax
",
            id = class.id,
            stack_size = TypeId::align(class.init_size, 8),
        ));

        self.cur_stack_offset = 8;

        for attr in class.attrs.iter() {
            self.gen_attr(attr);
        }

        self.text_section.push_str(&format!(
            r"    mov     rax, QWORD PTR [rbp - 8]
    lea     rdx, QWORD PTR [rip + {id}.Table]
    add     rsp, {stack_size}
    pop     rbp
    ret
",
            id = class.id,
            stack_size = TypeId::align(class.init_size, 8),
        ));

        self.end_scope();
    }

    fn gen_attr(&mut self, attr: &TypedAttribute<'a>) {
        self.insert_attr(attr.id(), attr.ty(), attr.offset());
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
                        r"    push    {r1}
    call    Int.To_Object
    add     rsp, 8

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
                        r"    push    {r1}
    call    Bool.To_Object
    add     rsp, 8

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
                        r"    push    {r2}
    push    {r1}
    call    String.To_Object
    add     rsp, 16

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
            None => self.gen_attr_default(attr.ty(), attr.offset()),
        }
    }

    fn gen_attr_default(&mut self, ty: TypeId, offset: usize) {
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
                    r"    push    0
    call    String.New
    add     rsp, 8
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
        use crate::ast::TypedExprKind as TEK;
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
                    r"
_string_{label}:
    .string {s:?}
    .align 8
_string_{label}_len:
    .quad {len}
",
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
            TEK::SelfId => {
                let r1 = self.alloc_reg().unwrap();
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r1}, QWORD PTR [rbp + 16]
    mov     {r2}, QWORD PTR [rbp + 24]
"
                ));
                (type_id, r1, Some(r2))
            }
            TEK::Id(id) => self.gen_id(id),
            TEK::Unary(op, e) => self.gen_unary(*op, e),
            TEK::Binary(op, lhs, rhs) => self.gen_binary(*op, lhs, rhs),
            TEK::Dispatch(e, method, args) => self.gen_dispatch(e, method, args, type_id),
            TEK::SelfDispatch(method, args) => self.gen_dispatch_self(method, args, type_id),
            TEK::StaticDispatch(e, e_ty, method, args) => {
                self.gen_static_dispatch(e, *e_ty, method, args, type_id)
            }
            TEK::New(ty) => self.gen_new_expr(*ty),
            TEK::Block(exprs) => self.gen_block(exprs),
            TEK::If(cond, then_expr, else_expr) => self.gen_if(type_id, cond, then_expr, else_expr),
            TEK::Assign(id, e) => self.gen_assign(id, e),
            TEK::Let(binds, expr) => self.gen_let(binds, expr),
            TEK::While(cond, body) => self.gen_while(cond, body),
            TEK::Case(e, cases) => self.gen_case(type_id, e, cases),
        }
    }

    fn gen_jmp_table(&mut self, jmp_table: usize, labels: &[usize]) {
        self.push_data(&format!("_jmp_table_{jmp_table}:\n"));
        for label in labels {
            self.push_data(&format!("    .quad .L{label}\n"));
        }
    }

    fn gen_case(
        &mut self,
        ty: TypeId,
        e: &TypedExpr<'a>,
        cases: &[TypedCaseArm<'a>],
    ) -> (TypeId, Reg, Option<Reg>) {
        let (e_ty, r1, r2) = self.gen_expr(e);
        if matches!(e_ty, TypeId::INT | TypeId::BOOL | TypeId::STRING) {
            todo!();
        }
        let r2 = r2.unwrap();

        self.push_text(&format!(
            r"    sub     rsp, 40
    mov     QWORD PTR [rsp + 32], {r1}
    mov     QWORD PTR [rsp + 24], -1
    mov     QWORD PTR [rsp + 16], 0
    mov     QWORD PTR [rsp], {r2}
"
        ));

        let r3 = self.alloc_reg().unwrap();
        for (i, case_ty) in cases.iter().map(|c| c.ty).enumerate() {
            let name = self.class_env.get_class_name(case_ty).unwrap();
            let label1 = self.cur_label;
            let label2 = self.cur_label + 1;
            self.cur_label += 2;
            self.push_text(&format!(
                r"    lea     {r3}, QWORD PTR [rip + {name}.Table]
    mov     QWORD PTR [rsp + 8], {r3}
    call    class.distance
    cmp     rax, -1
    je      .L{label2}
    cmp     QWORD PTR [rsp + 24], -1
    je      .L{label1}
    cmp     rax, QWORD PTR [rsp + 24]
    jge     .L{label2}
.L{label1}:
    mov     QWORD PTR [rsp + 16], {i}
    mov     QWORD PTR [rsp + 24], rax
.L{label2}:
"
            ));
        }

        let jmp_table = self.cur_jmp_table;
        self.cur_jmp_table += 1;
        let label = self.cur_label;
        self.cur_label += 1;
        self.push_text(&format!(
            r"    pop     {r2}
    add     rsp, 8
    pop     {r3}
    pop     {r1}
    cmp     {r1}, -1
    jne     .L{label}
    call    Object.abort
.L{label}:
    lea     {r1}, QWORD PTR [rip + _jmp_table_{jmp_table}]
    mov     {r3}, QWORD PTR [{r1} + {r3} * 8]
    pop     {r1}
    jmp     {r3}
"
        ));
        self.free_reg(r3);

        let last_label = self.cur_label
            + cases.len()
            + cases.iter().map(|c| c.expr.count_labels()).sum::<usize>();
        let labels = cases
            .iter()
            .map(|case| self.gen_case_arm(ty, r1, r2, case, last_label))
            .collect::<Vec<_>>();
        self.push_text(&format!(".L{last_label}:\n"));
        self.cur_label += 1;
        self.gen_jmp_table(jmp_table, &labels);

        match ty {
            TypeId::INT | TypeId::BOOL => {
                self.free_reg(r2);
                (ty, r1, None)
            }
            _ => (ty, r1, Some(r2)),
        }
    }

    fn gen_case_arm(
        &mut self,
        ty: TypeId,
        r1: Reg,
        r2: Reg,
        case: &TypedCaseArm<'a>,
        exit_label: usize,
    ) -> usize {
        let r3 = self.alloc_reg().unwrap();
        let case_name = self.class_env.get_class_name(case.ty).unwrap();
        let label = self.cur_label;
        self.cur_label += 1;
        self.gen_comment(case_name);
        self.push_text(&format!(".L{label}:\n"));
        self.begin_scope();
        let offset = self.insert_local(case.id, case.ty);
        match case.ty {
            TypeId::INT => {
                let (addr, _) = self.gen_stack_access(8, offset);
                self.push_text(&format!(
                    r"    mov     {r3}, QWORD PTR [{r1}]
    mov     {addr}, {r3}
"
                ));
            }
            TypeId::BOOL => {
                let (addr, _) = self.gen_stack_access(1, offset);
                self.push_text(&format!(
                    r"    movzx   {r3_dword}, BYTE PTR [{r1}]
    mov     {addr}, {r3_byte}
",
                    r3_dword = r3.to_dword_string(),
                    r3_byte = r3.to_byte_string()
                ));
            }
            TypeId::STRING => {
                let r4 = self.alloc_reg().unwrap();
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    r"    mov     {r3}, QWORD PTR [{r1}]
    mov     {r4}, QWORD PTR [{r1} + 8]
    mov     {addr1}, {r3}
    mov     {addr2}, {r4}
"
                ));
                self.free_reg(r4);
            }
            _ => {
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    r"    mov     {addr1}, {r1}
    mov     {addr2}, {r2}
"
                ));
            }
        }
        self.free_reg(r3);
        let (e_ty, r3, r4) = self.gen_expr(&case.expr);
        match e_ty {
            TypeId::INT if ty.is_int() => {
                self.gen_mov(r1, r3);
                self.free_reg(r3);
            }
            TypeId::BOOL if ty.is_bool() => {
                self.gen_mov(r1, r3);
                self.free_reg(r3);
            }
            TypeId::INT => {
                self.push_text(&format!(
                    r"    push    {r3}
    call    Int.To_Object
    add     rsp, 8
"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                self.free_reg(r3);
            }
            TypeId::BOOL => {
                self.push_text(&format!(
                    r"    push    {r3}
    call    Bool.To_Object
    add     rsp, 8
"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                self.free_reg(r3);
            }
            TypeId::STRING if !ty.is_string() => {
                let r4 = r4.unwrap();
                self.push_text(&format!(
                    r"    push    {r4}
    push    {r3}
    call    String.To_Object
    add     rsp, 16
"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                self.free_reg(r3);
                self.free_reg(r4);
            }
            _ => {
                let r4 = r4.unwrap();
                self.gen_mov(r1, r3);
                self.gen_mov(r2, r4);
                self.free_reg(r3);
                self.free_reg(r4);
            }
        }
        self.end_scope();
        self.push_text(&format!("    jmp     .L{exit_label}\n"));
        label
    }

    fn gen_while(
        &mut self,
        cond: &TypedExpr<'a>,
        body: &TypedExpr<'a>,
    ) -> (TypeId, Reg, Option<Reg>) {
        let label1 = self.cur_label;
        let label2 = self.cur_label + 1;
        self.cur_label += 2;
        self.push_text(&format!(".L{label1}:\n",));
        let (_, r1, _) = self.gen_expr(cond);
        self.push_text(&format!(
            "    test    {r1}, {r1}\n    je      .L{label2}\n",
            r1 = r1.to_byte_string(),
        ));
        self.free_reg(r1);
        let (_, r1, r2) = self.gen_expr(body);
        let r2 = r2.unwrap_or_else(|| self.alloc_reg().unwrap());
        self.push_text(&format!(
            r"    jmp     .L{label1}
.L{label2}:
    xor     {r1}, {r1}
    xor     {r2}, {r2}
",
        ));
        (TypeId::OBJECT, r1, Some(r2))
    }

    fn gen_let(
        &mut self,
        binds: &[(TypedFormal<'a>, Option<TypedExpr<'a>>)],
        expr: &TypedExpr<'a>,
    ) -> (TypeId, Reg, Option<Reg>) {
        self.begin_scope();
        for (formal, e) in binds {
            self.gen_bind(formal, e.as_ref());
        }
        let (ty, r1, r2) = self.gen_expr(expr);
        self.end_scope();
        (ty, r1, r2)
    }

    fn gen_bind(&mut self, formal: &TypedFormal<'a>, e: Option<&TypedExpr<'a>>) {
        let offset = self.insert_local(formal.id, formal.ty);
        match e {
            Some(e) => {
                let (_, r1, r2) = self.gen_assign(formal.id, e);
                self.free_reg(r1);
                if let Some(r2) = r2 {
                    self.free_reg(r2);
                }
            }
            None => {
                self.gen_local_default(formal, offset);
            }
        }
    }

    fn gen_local_default(&mut self, formal: &TypedFormal<'a>, offset: isize) {
        match formal.ty {
            TypeId::INT => {
                let (addr, _) = self.gen_stack_access(8, offset);
                self.push_text(&format!("    mov     {addr}, 0\n"));
            }
            TypeId::BOOL => {
                let (addr, _) = self.gen_stack_access(1, offset);
                self.push_text(&format!("    mov     {addr}, 0\n"));
            }
            TypeId::STRING => {
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    r"    push    0
    call    String.New
    add     rsp, 8
    mov     {addr1}, rax
    mov     {addr2}, rdx
",
                ));
            }
            _ => {
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!("    mov     {addr1}, 0\n    mov     {addr2}, 0\n"));
            }
        }
    }

    fn gen_stack_access(&mut self, size: usize, offset: isize) -> (String, Option<String>) {
        match (size, offset) {
            (1, ..0) => (format!("BYTE PTR [rbp - {}]", -offset), None),
            (8, ..0) => (format!("QWORD PTR [rbp - {}]", -offset), None),
            (16, ..0) => {
                let offset = -offset;
                (
                    format!("QWORD PTR [rbp - {}]", offset),
                    Some(format!("QWORD PTR [rbp - {}]", offset + 8)),
                )
            }
            (1, _) => (format!("BYTE PTR [rbp + {}]", offset), None),
            (8, _) => (format!("QWORD PTR [rbp + {}]", offset), None),
            (16, _) => (
                format!("QWORD PTR [rbp + {}]", offset),
                Some(format!("QWORD PTR [rbp + {}]", offset + 8)),
            ),
            _ => unreachable!(),
        }
    }

    fn gen_assign(&mut self, id: &'a str, e: &TypedExpr<'a>) -> (TypeId, Reg, Option<Reg>) {
        let Some((ty, offset)) = self.get_local(id) else {
            return self.gen_attr_store(id, e);
        };
        let (ty_expr, r1, r2) = self.gen_expr(e);
        match ty_expr {
            TypeId::INT if ty.is_int() => {
                let (addr, _) = self.gen_stack_access(8, offset);
                self.push_text(&format!("    mov     {addr}, {r1}\n"));
                (TypeId::INT, r1, None)
            }
            TypeId::BOOL if ty.is_bool() => {
                let (addr, _) = self.gen_stack_access(1, offset);
                self.push_text(&format!(
                    "    mov     {addr}, {r1}\n",
                    r1 = r1.to_byte_string()
                ));
                (TypeId::BOOL, r1, None)
            }
            TypeId::INT => {
                self.push_text(&format!(
                    r"    push    {r1}
    call    Int.To_Object
    add     rsp, 8
"
                ));
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    "    mov     {addr1}, rax\n    mov     {addr2}, rdx\n"
                ));
                let r3 = self.alloc_reg().unwrap();
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r3, Reg::Rdx);
                (TypeId::INT, r1, Some(r3))
            }
            TypeId::BOOL => {
                self.push_text(&format!(
                    r"    push    {r1}
    call    Bool.To_Object
    add     rsp, 8
"
                ));
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    "    mov     {addr1}, rax\n    mov     {addr2}, rdx\n"
                ));
                let r3 = self.alloc_reg().unwrap();
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r3, Reg::Rdx);
                (ty, r1, Some(r3))
            }
            TypeId::STRING if !ty.is_string() => {
                let r2 = r2.unwrap();
                self.push_text(&format!(
                    r"    push    {r2}
    push    {r1}
    call    String.To_Object
    add     rsp, 16
"
                ));
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    "    mov     {addr1}, rax\n    mov     {addr2}, rdx\n"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                (ty, r1, Some(r2))
            }
            _ => {
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    "    mov     {addr1}, {r1}\n    mov     {addr2}, {r2}\n",
                    r2 = r2.unwrap()
                ));
                (ty, r1, r2)
            }
        }
    }

    fn gen_if(
        &mut self,
        ty: TypeId,
        cond: &TypedExpr<'a>,
        then_expr: &TypedExpr<'a>,
        else_expr: &TypedExpr<'a>,
    ) -> (TypeId, Reg, Option<Reg>) {
        self.gen_comment("if");
        let (_, r1, _) = self.gen_expr(cond);
        let label1 = self.cur_label;
        let label2 = self.cur_label + 1;
        self.cur_label += 2;
        self.push_text(&format!(
            "    test    {r1}, {r1}\n    je      .L{label1}\n",
            r1 = r1.to_byte_string(),
        ));
        self.free_reg(r1);
        self.gen_comment("then");
        let (_, r1, r2) = self.gen_branch(ty, then_expr);
        self.push_text(&format!("    jmp     .L{label2}\n.L{label1}:\n",));
        self.gen_comment("else");
        let (_, r3, r4) = self.gen_branch(ty, else_expr);
        self.gen_mov(r1, r3);
        self.free_reg(r3);
        if let Some(r2) = r2 {
            let r4 = r4.unwrap();
            self.gen_mov(r2, r4);
            self.free_reg(r4);
        }
        self.push_text(&format!(".L{label2}:\n",));
        (ty, r1, r2)
    }

    fn gen_branch(&mut self, ty: TypeId, expr: &TypedExpr<'a>) -> (TypeId, Reg, Option<Reg>) {
        let (ty_branch, r1, mut r2) = self.gen_expr(expr);
        match ty_branch {
            TypeId::INT if !ty.is_int() => {
                let r3 = self.alloc_reg_no_ret().unwrap();
                self.push_text(&format!(
                    r"    push    {r1}
    call    Int.To_Object
    add     rsp, 8
"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r3, Reg::Rdx);
                r2 = Some(r3);
            }
            TypeId::BOOL if !ty.is_bool() => {
                let r3 = self.alloc_reg_no_ret().unwrap();
                self.push_text(&format!(
                    r"    push    {r1}
    call    Bool.To_Object
    add     rsp, 8
"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r3, Reg::Rdx);
                r2 = Some(r3);
            }
            TypeId::STRING if !ty.is_string() => {
                let r2 = r2.unwrap();
                self.push_text(&format!(
                    r"    push    {r1}
    push    {r2}
    call    String.To_Object
    add     rsp, 16
    mov     {r1}, rax
    mov     {r2}, rdx
"
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
            }
            _ => {}
        }
        (ty_branch, r1, r2)
    }

    fn gen_attr_store(&mut self, id: &'a str, e: &TypedExpr<'a>) -> (TypeId, Reg, Option<Reg>) {
        let (ty, offset) = self.attrs.get(id).copied().unwrap();
        let (ty_expr, r1, r2) = self.gen_expr(e);
        match ty_expr {
            TypeId::INT if ty.is_int() => {
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r2}, QWORD PTR [rbp + 16]
    mov     QWORD PTR [{r2} + {offset}], {r1}
"
                ));
                self.free_reg(r2);
                (TypeId::INT, r1, None)
            }
            TypeId::BOOL if ty.is_bool() => {
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r2}, QWORD PTR [rbp + 16]
    mov     BYTE PTR [{r2} + {offset}], {r1}
",
                    r1 = r1.to_byte_string()
                ));
                self.free_reg(r2);
                (TypeId::BOOL, r1, None)
            }
            TypeId::INT => {
                self.push_text(&format!(
                    r"    push    {r1}
    call    Int.To_Object
    add     rsp, 8
"
                ));
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r2}, QWORD PTR [rbp + 16]
    mov     QWORD PTR [{r2} + {offset}], rax
    mov     QWORD PTR [{r2} + {offset2}], rdx
",
                    offset2 = offset + 8
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                (ty, r1, Some(r2))
            }
            TypeId::BOOL => {
                self.push_text(&format!(
                    r"    push    {r1}
    call    Bool.To_Object
    add     rsp, 8
"
                ));
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r2}, QWORD PTR [rbp + 16]
    mov     QWORD PTR [{r2} + {offset}], rax
    mov     QWORD PTR [{r2} + {offset2}], rdx
",
                    offset2 = offset + 8
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                (ty, r1, Some(r2))
            }
            TypeId::STRING if !ty.is_string() => {
                let r2 = r2.unwrap();
                self.push_text(&format!(
                    r"    push    {r2}
    push    {r1}
    call    String.To_Object
    add     rsp, 16
"
                ));
                self.push_text(&format!(
                    r"    mov     {r2}, QWORD PTR [rbp + 16]
    mov     QWORD PTR [{r2} + {offset}], rax
    mov     QWORD PTR [{r2} + {offset2}], rdx
",
                    offset2 = offset + 8
                ));
                self.gen_mov(r1, Reg::Rax);
                self.gen_mov(r2, Reg::Rdx);
                (ty, r1, Some(r2))
            }
            _ => {
                let r3 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r3}, QWORD PTR [rbp + 16]
    mov     QWORD PTR [{r3} + {offset}], {r1}
    mov     QWORD PTR [{r3} + {offset2}], {r2}
",
                    offset2 = offset + 8,
                    r2 = r2.unwrap()
                ));
                self.free_reg(r3);
                (ty, r1, r2)
            }
        }
    }

    fn gen_attr_load(&mut self, id: &'a str) -> (TypeId, Reg, Option<Reg>) {
        let (ty, offset) = self.attrs.get(id).copied().unwrap();

        match ty {
            TypeId::INT => {
                let r1 = self.alloc_reg().unwrap();
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r1}, QWORD PTR [rbp + 16]
    mov     {r2}, QWORD PTR [{r1} + {offset}]
"
                ));
                self.free_reg(r1);
                (TypeId::INT, r2, None)
            }
            TypeId::BOOL => {
                let r1 = self.alloc_reg().unwrap();
                let r2 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r1}, QWORD PTR [rbp + 16]
    movzx   {r2}, BYTE PTR [{r1} + {offset}]
",
                    r2 = r2.to_dword_string()
                ));
                self.free_reg(r1);
                (TypeId::BOOL, r2, None)
            }
            _ => {
                let r1 = self.alloc_reg().unwrap();
                let r2 = self.alloc_reg().unwrap();
                let r3 = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    r"    mov     {r1}, QWORD PTR [rbp + 16]
    mov     {r2}, QWORD PTR [{r1} + {offset}]
    mov     {r3}, QWORD PTR [{r1} + {offset2}]
",
                    offset2 = offset + 8
                ));
                self.free_reg(r1);
                (ty, r2, Some(r3))
            }
        }
    }

    fn gen_id(&mut self, id: &'a str) -> (TypeId, Reg, Option<Reg>) {
        let Some((ty, offset)) = self.get_local(id) else {
            return self.gen_attr_load(id);
        };
        match ty {
            TypeId::INT => {
                let (addr, _) = self.gen_stack_access(8, offset);
                let r = self.alloc_reg().unwrap();
                self.push_text(&format!("    mov     {r}, {addr}\n"));
                (TypeId::INT, r, None)
            }
            TypeId::BOOL => {
                let (addr, _) = self.gen_stack_access(1, offset);
                let r = self.alloc_reg().unwrap();
                self.push_text(&format!(
                    "    movzx   {r}, {addr}\n",
                    r = r.to_dword_string()
                ));
                (TypeId::BOOL, r, None)
            }
            _ => {
                let r1 = self.alloc_reg().unwrap();
                let r2 = self.alloc_reg().unwrap();
                let (addr1, addr2) = self.gen_stack_access(16, offset);
                let addr2 = addr2.unwrap();
                self.push_text(&format!(
                    "    mov     {r1}, {addr1}\n    mov     {r2}, {addr2}\n",
                ));
                (ty, r1, Some(r2))
            }
        }
    }

    fn gen_block(&mut self, exprs: &[TypedExpr<'a>]) -> (TypeId, Reg, Option<Reg>) {
        for expr in exprs.iter().take(exprs.len() - 1) {
            let (_, r1, r2) = self.gen_expr(expr);
            self.free_reg(r1);
            if let Some(r2) = r2 {
                self.free_reg(r2);
            }
        }
        self.gen_expr(exprs.last().unwrap())
    }

    fn gen_ret_regs(&mut self, ty: TypeId) -> (TypeId, Reg, Option<Reg>) {
        match ty {
            TypeId::INT | TypeId::BOOL => {
                let r = self.reserve_or_alloc_reg(Reg::Rax);
                self.gen_mov(r, Reg::Rax);
                (ty, r, None)
            }
            _ => {
                let r1 = self.reserve_or_alloc_reg(Reg::Rax);
                let r2 = self.reserve_or_alloc_reg(Reg::Rdx);
                mov_regs!(Reg::Rax => r1, Reg::Rdx => r2, self);
                (ty, r1, Some(r2))
            }
        }
    }

    fn gen_new_expr(&mut self, ty: TypeId) -> (TypeId, Reg, Option<Reg>) {
        let id = self.class_env.get_class(ty).unwrap().id();
        self.push_text(&format!(
            "    push    0\n    call    {id}.New\n    add     rsp, 8\n"
        ));
        self.gen_ret_regs(ty)
    }

    fn gen_dispatch_self(
        &mut self,
        method_name: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        let mut size = 16;
        let method = self
            .class_env
            .get_method(self.cur_class, method_name)
            .unwrap();
        let params = method.params().to_owned();
        for (arg, param_ty) in args.iter().zip(params).rev() {
            size = self.gen_arg(param_ty, arg, size);
        }
        let class = self.class_env.get_class(self.cur_class).unwrap();
        let method_offset = class.get_vtable_offset(method_name).unwrap() * 8;
        let r = self.alloc_reg().unwrap();
        self.push_text(&format!(
            r"    push    QWORD PTR [rbp + 24]
    push    QWORD PTR [rbp + 16]
    mov     {r}, QWORD PTR [rbp + 24]
    mov     {r}, QWORD PTR [{r} + {method_offset}]
    call    {r}
    add     rsp, {size}
"
        ));
        self.free_reg(r);
        self.gen_ret_regs(ret_ty)
    }

    fn gen_static_dispatch(
        &mut self,
        e: &TypedExpr<'a>,
        e_ty: TypeId,
        method_name: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        let mut size = 0;
        let method = self.class_env.get_method(e_ty, method_name).unwrap();
        let params = method.params().to_owned();
        for (arg, param_ty) in args.iter().zip(params).rev() {
            size = self.gen_arg(param_ty, arg, size);
        }
        match e.ty {
            TypeId::INT => return self.gen_dispatch_int_static(e, e_ty, method_name, ret_ty, size),
            TypeId::BOOL => {
                return self.gen_dispatch_bool_static(e, e_ty, method_name, ret_ty, size)
            }
            TypeId::STRING => {
                return self.gen_dispatch_string_static(e, e_ty, method_name, ret_ty, size)
            }
            _ => {}
        }
        let (_, r1, r2) = self.gen_expr(e);
        let r2 = r2.unwrap();
        let class_name = self.class_env.get_class_name(e_ty).unwrap();
        size += 16;
        self.push_text(&format!(
            r"    push    {r2}
    push    {r1}
    call    {class_name}.{method_name}
    add     rsp, {size}
",
        ));
        self.free_reg(r1);
        self.free_reg(r2);
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch(
        &mut self,
        e: &TypedExpr<'a>,
        method_name: &'a str,
        args: &[TypedExpr<'a>],
        ret_ty: TypeId,
    ) -> (TypeId, Reg, Option<Reg>) {
        let mut size = 0;
        let method = self.class_env.get_method(e.ty, method_name).unwrap();
        let params = method.params().to_owned();
        for (arg, param_ty) in args.iter().zip(params).rev() {
            size = self.gen_arg(param_ty, arg, size);
        }
        match e.ty {
            TypeId::INT => return self.gen_dispatch_int(e, method_name, ret_ty, size),
            TypeId::BOOL => return self.gen_dispatch_bool(e, method_name, ret_ty, size),
            TypeId::STRING => return self.gen_dispatch_string(e, method_name, ret_ty, size),
            _ => {}
        }
        let class = self.class_env.get_class(e.ty).unwrap();
        let method_offset = class.get_vtable_offset(method_name).unwrap() * 8;
        let (_, r1, r2) = self.gen_expr(e);
        let r2 = r2.unwrap();
        size += 16;
        self.push_text(&format!(
            r"    push    {r2}
    push    {r1}
    mov     {r1}, QWORD PTR [{r2} + {method_offset}]
    call    {r1}
    add     rsp, {size}
"
        ));
        self.free_reg(r1);
        self.free_reg(r2);
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch_string(
        &mut self,
        e: &TypedExpr<'a>,
        method: &'a str,
        ret_ty: TypeId,
        mut size: usize,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r1, r2) = self.gen_expr(e);
        let r2 = r2.unwrap();
        self.push_text(&format!("    push    {r2}\n    push    {r1}\n"));
        self.free_reg(r1);
        self.free_reg(r2);
        let method_class = self
            .class_env
            .get_class(TypeId::STRING)
            .unwrap()
            .get_vtable_entry(method)
            .unwrap();
        let call = format!("    call    {}.{}\n", method_class, method);
        size += 16;
        if method_class != "String" {
            self.push_text(
                r"    call    String.To_Object
    mov     QWORD PTR [rsp + 8], rdx
    mov     QWORD PTR [rsp], rax
",
            );
        }
        self.push_text(&call);
        self.push_text(&format!("    add     rsp, {}\n", size));
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch_int(
        &mut self,
        e: &TypedExpr<'a>,
        method: &'a str,
        ret_ty: TypeId,
        mut size: usize,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r, _) = self.gen_expr(e);
        self.push_text(&format!("    push    {r}\n"));
        self.free_reg(r);
        let method_class = self
            .class_env
            .get_class(TypeId::INT)
            .unwrap()
            .get_vtable_entry(method)
            .unwrap();
        let call = format!("    call {}.{}\n", method_class, method);
        size += 8;
        if method_class != "Int" {
            self.push_text(
                r"    call    Int.To_Object
    mov     QWORD PTR [rsp], rdx
    push    rax
",
            );
            size += 8;
        }
        self.push_text(&call);
        self.push_text(&format!("    add     rsp, {}\n", size));
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch_bool(
        &mut self,
        e: &TypedExpr<'a>,
        method: &'a str,
        ret_ty: TypeId,
        mut size: usize,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r, _) = self.gen_expr(e);
        self.push_text(&format!("    push    {r}\n"));
        self.free_reg(r);
        let method_class = self
            .class_env
            .get_class(TypeId::BOOL)
            .unwrap()
            .get_vtable_entry(method)
            .unwrap();
        let call = format!("    call {}.{}\n", method_class, method);
        size += 8;
        if method_class != "Bool" {
            self.push_text(
                r"    call    Bool.To_Object
    mov     QWORD PTR [rsp], rdx
    push    rax
",
            );
            size += 8;
        }
        self.push_text(&call);
        self.push_text(&format!("    add     rsp, {}\n", size));
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch_string_static(
        &mut self,
        e: &TypedExpr<'a>,
        e_ty: TypeId,
        method_name: &'a str,
        ret_ty: TypeId,
        mut size: usize,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r1, r2) = self.gen_expr(e);
        let r2 = r2.unwrap();
        self.push_text(&format!("    push    {r2}\n    push    {r1}\n"));
        size += 16;
        self.free_reg(r1);
        self.free_reg(r2);
        if e_ty != TypeId::STRING {
            self.push_text(
                r"    call    String.To_Object
    mov     QWORD PTR [rsp + 8], rdx
    mov     QWORD PTR [rsp], rax
",
            );
        }
        let class_name = self.class_env.get_class_name(e_ty).unwrap();
        self.push_text(&format!(
            r"    call    {class_name}.{method_name}
    add     rsp, {size}
",
        ));
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch_int_static(
        &mut self,
        e: &TypedExpr<'a>,
        e_ty: TypeId,
        method_name: &'a str,
        ret_ty: TypeId,
        mut size: usize,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r, _) = self.gen_expr(e);
        self.push_text(&format!("    push    {r}\n"));
        self.free_reg(r);
        size += 8;
        if e_ty != TypeId::INT {
            self.push_text(
                r"    call    Int.To_Object
    mov     QWORD PTR [rsp], rdx
    push    rax
",
            );
            size += 8;
        }
        let class_name = self.class_env.get_class_name(e_ty).unwrap();
        self.push_text(&format!(
            r"    call    {class_name}.{method_name}
    add     rsp, {size}
",
        ));
        self.gen_ret_regs(ret_ty)
    }

    fn gen_dispatch_bool_static(
        &mut self,
        e: &TypedExpr<'a>,
        e_ty: TypeId,
        method_name: &'a str,
        ret_ty: TypeId,
        mut size: usize,
    ) -> (TypeId, Reg, Option<Reg>) {
        let (_, r, _) = self.gen_expr(e);
        self.push_text(&format!("    push    {r}\n"));
        self.free_reg(r);
        size += 8;
        if e_ty != TypeId::BOOL {
            self.push_text(
                r"    call    Bool.To_Object
    mov     QWORD PTR [rsp], rdx
    push    rax
",
            );
            size += 8;
        }
        let class_name = self.class_env.get_class_name(e_ty).unwrap();
        self.push_text(&format!(
            r"    call    {class_name}.{method_name}
    add     rsp, {size}
",
        ));
        self.gen_ret_regs(ret_ty)
    }

    fn gen_arg(&mut self, param_ty: TypeId, arg: &TypedExpr<'a>, size: usize) -> usize {
        let (ty, r1, r2) = self.gen_expr(arg);
        match ty {
            TypeId::INT if param_ty.is_int() => {
                self.push_text(&format!("    push    {r1}\n"));
                self.free_reg(r1);
                size + 8
            }
            TypeId::BOOL if param_ty.is_bool() => {
                self.push_text(&format!("    push    {r1}\n"));
                self.free_reg(r1);
                size + 8
            }
            TypeId::INT => {
                self.push_text(&format!(
                    r"    push    {r1}
    call    Int.To_Object
    mov     QWORD PTR [rsp], rdx
    push    rax
"
                ));
                self.free_reg(r1);
                size + 16
            }
            TypeId::BOOL => {
                self.push_text(&format!(
                    r"    push    {r1}
    call    Bool.To_Object
    mov     QWORD PTR [rsp], rdx
    push    rax
"
                ));
                self.free_reg(r1);
                size + 16
            }
            TypeId::STRING if !param_ty.is_string() => {
                let r2 = r2.unwrap();
                self.push_text(&format!(
                    r"    push    {r2}
    push    {r1}
    call    String.To_Object
    mov     QWORD PTR [rsp + 8], rdx
    mov     QWORD PTR [rsp], rax
"
                ));
                self.free_reg(r1);
                size + 16
            }
            _ => {
                let r2 = r2.unwrap();
                self.push_text(&format!("    push    {r2}\n    push    {r1}\n"));
                self.free_reg(r1);
                self.free_reg(r2);
                size + 16
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
                self.push_text(&format!("    xor     {r1}, 1\n"));
                (ty, r1, None)
            }
            UnOp::IsVoid => match e.ty {
                TypeId::INT | TypeId::BOOL => {
                    self.push_text(&format!("    xor     {r1}, {r1}\n"));
                    (TypeId::BOOL, r1, None)
                }
                TypeId::STRING => {
                    self.push_text(&format!("    xor     {r1}, {r1}\n"));
                    self.free_reg(r2.unwrap());
                    (TypeId::BOOL, r1, None)
                }
                _ => {
                    self.push_text(&format!(
                        r"    test    {r1}, {r1}
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
        if op == BinOp::Eq {
            return self.gen_eq(lhs, rhs);
        }
        let (_, r, _) = self.gen_expr(lhs);
        self.push_text(&format!("    push    {r}\n"));
        self.free_reg(r);
        let (_, mut r, _) = self.gen_expr(rhs);
        match op {
            BinOp::Add => {
                self.push_text(&format!(
                    r"    add     {r}, QWORD PTR [rsp]
    add     rsp, 8
"
                ));
                (TypeId::INT, r, None)
            }
            BinOp::Sub => {
                self.push_text(&format!(
                    r"    sub     QWORD PTR [rsp], {r}
    pop     {r}
"
                ));
                (TypeId::INT, r, None)
            }
            BinOp::Mul => {
                self.push_text(&format!(
                    r"    imul    {r}, QWORD PTR [rsp]
    add     rsp, 8
"
                ));
                (TypeId::INT, r, None)
            }
            BinOp::Div => {
                if matches!(r, Reg::Rax | Reg::Rdx) {
                    let r5 = self.alloc_reg_no_ret().unwrap();
                    self.gen_mov(r5, r);
                    self.free_reg(r);
                    r = r5;
                }
                self.push_text(&format!(
                    r"    pop    rax
    cqo
    idiv    {r}
"
                ));
                self.gen_mov(r, Reg::Rax);
                (TypeId::INT, r, None)
            }

            BinOp::Lt => {
                self.push_text(&format!(
                    r"    cmp     QWORD PTR [rsp], {r}
    setl    {r_to_byte}
    add     rsp, 8
",
                    r_to_byte = r.to_byte_string()
                ));
                (TypeId::BOOL, r, None)
            }
            BinOp::Le => {
                self.push_text(&format!(
                    r"    cmp     QWORD PTR [rsp], {r}
    setle   {r_to_byte}
    add     rsp, 8
",
                    r_to_byte = r.to_byte_string()
                ));
                (TypeId::BOOL, r, None)
            }
            BinOp::Eq => unreachable!(),
        }
    }

    fn gen_eq(&mut self, lhs: &TypedExpr<'a>, rhs: &TypedExpr<'a>) -> (TypeId, Reg, Option<Reg>) {
        let (ty, r1, r2) = self.gen_expr(lhs);
        match ty {
            TypeId::INT | TypeId::BOOL => {
                self.push_text(&format!("    push    {r1}\n"));
                self.free_reg(r1);
                let (_, r3, _) = self.gen_expr(rhs);
                self.push_text(&format!(
                    r"    cmp     QWORD PTR [rsp], {r3}
    sete    {r3_to_byte}
    add     rsp, 8
",
                    r3_to_byte = r3.to_byte_string()
                ));
                (TypeId::BOOL, r3, None)
            }
            TypeId::STRING => {
                let r2 = r2.unwrap();
                self.push_text(&format!(
                    r"    push    {r1}
    push    {r2}
",
                ));
                self.free_reg(r1);
                self.free_reg(r2);
                let (_, r3, r4) = self.gen_expr(rhs);
                let r4 = r4.unwrap();
                let label1 = self.cur_label;
                let label2 = self.cur_label + 1;
                self.cur_label += 2;
                self.push_text(&format!(
                    r"    cmp     QWORD PTR [rsp + 8], {r3}
    jne     .L{label1}
    push    {r4}
    call    memory_compare
    add     rsp, 24
    mov     {r3}, rax
    jmp     .L{label2}
.L{label1}:
    add     rsp, 16
    xor     {r3}, {r3}
.L{label2}:
"
                ));
                self.free_reg(r4);
                (TypeId::BOOL, r3, None)
            }
            _ => {
                let r2 = r2.unwrap();
                self.push_text(&format!("    push    {r1}\n"));
                self.free_reg(r1);
                self.free_reg(r2);
                let (_, r3, r4) = self.gen_expr(rhs);
                let r4 = r4.unwrap();
                self.push_text(&format!(
                    r"    cmp     QWORD PTR [rsp], {r3}
    sete    {r3_to_byte}
    add     rsp, 8
",
                    r3_to_byte = r3.to_byte_string()
                ));
                self.free_reg(r4);
                (TypeId::BOOL, r3, None)
            }
        }
    }

    pub fn output(&self) -> String {
        let mut output = fs::read_to_string("lib/text_section.s").unwrap();
        output.push_str(&self.text_section);
        output.push('\n');
        output.push_str(".data\n");
        output.push_str(&self.data_section);
        output.push('\n');
        output.push_str(&fs::read_to_string("lib/data_section.s").unwrap());
        output.push('\n');
        output
    }

    pub fn take_output(self) -> String {
        self.output()
    }
}
