use std::{any::Any, collections::HashMap};

use crate::{
    ast::{TypedClass, TypedExpr, TypedExprKind},
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
    // R11,
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
            // Reg::R11 => "r11",
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
            // Reg::R11 => 8,
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
            // 8 => Some(Reg::R11),
            _ => None,
        }
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
    regs:             [bool; 8],
}

impl<'a> CodeGenerator<'a> {
    pub fn new(class_env: ClassEnv<'a>) -> Self {
        Self {
            locals: Vec::new(),
            class_env,
            cur_class: TypeId::SelfType,
            cur_stack_offset: 0,
            data_section: String::from(include_str!("../lib/data_section.s")),
            text_section: String::from(include_str!("../lib/text_section.s")),
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

    fn free_reg(&mut self, reg: Reg) {
        self.regs[reg.to_index()] = true;
    }

    fn begin_scope(&mut self) {
        self.locals.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.locals.pop();
    }

    fn push_data(&mut self, data: &str) {
        self.data_section.push_str(data);
    }

    fn push_text(&mut self, text: &str) {
        self.text_section.push_str(text);
    }

    fn push_type_name(&mut self, id: &'a str) {
        self.push_data(&format!(
                "\n{id}_Typename:\n    .string \"{id}\"\n{id}_Typenamelen:\n    .quad .-{id}_Typename-1\n"
        ));
        self.push_text(&format!(
            "\n{id}_type_name:\n    mov     rax, [rip+{id}_Typenamelen]\n    lea     rdx, [rip+{id}_Typename]\n    ret\n"
        ));
    }

    fn push_vtable(&mut self) {
        let class = self.class_env.get_class(self.cur_class).unwrap();
        let id = class.id();
        let vtable = class.vtable();
        self.data_section
            .push_str(&format!("\n{id}_Table:\n    .quad {id}_New\n"));
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
    }

    fn gen_new(&mut self, class: &TypedClass<'a>, attrs_size: usize, parent_id: &'a str) {
        self.begin_scope();
        self.text_section.push_str(&format!(
            r"
{id}_New:
    push    rbp
    mov     rbp, rsp

    sub     rsp, {stack_size}

    cmp     rdi, 0

    jne     .{id}_New_L1

    mov     rdi, {attrs_size}
    call    malloc
    mov     QWORD PTR [rbp-8], rax

    jmp     .{id}_New_L2
.{id}_New_L1:
    mov     QWORD PTR [rbp-8], rdi
.{id}_New_L2:
    mov     rdi, [rbp-8]
    call    {parent_id}_New
",
            id = class.id,
            stack_size = class.init_size,
        ));

        self.cur_stack_offset = 8;

        for attr in class.attrs.iter() {
            match attr.init() {
                Some(ast) => {
                    let r = self.gen_expr(ast);
                    self.text_section.push_str(&format!(
                        r"
    mov     r11, [rbp-8]
    mov     [r11+{offset}], {reg}
",
                        offset = attr.offset(),
                        reg = r.to_string()
                    ));
                    self.free_reg(r);
                }
                None => todo!(),
            }
        }

        self.text_section.push_str(&format!(
            r"
    mov     rax, [rbp-8]
    mov     rdx, [rip+{id}_Table]
    add     rsp, {stack_size}
    pop     rbp
    ret
",
            id = class.id,
            stack_size = class.init_size
        ));

        self.end_scope();
    }

    fn gen_expr(&mut self, expr: &TypedExpr) -> Reg {
        use TypedExprKind as TEK;
        match &expr.kind {
            TEK::IntLit(i) => {
                let reg = self.alloc_reg().unwrap();
                self.push_text(&format!("    mov     {}, {}\n", reg.to_string(), i));
                reg
            }
            _ => todo!(),
        }
    }

    pub fn take_output(&self) -> String {
        format!("{}{}", self.data_section, self.text_section)
    }
}
