use std::rc::Rc;

use crate::{
    fxhash::FxHashMap,
    index_vec::IndexVec,
    ir::{opt::Function, types::Type, GlobalId, Instr, IrId, Value},
};

pub fn llvm_string(string: &str) -> String {
    let mut result = String::new();
    for c in string.chars() {
        match c {
            '\n' => result.push_str(r"\0A"),
            '\t' => result.push_str(r"\09"),
            '\u{0008}' => result.push_str(r"\08"),
            '\u{000C}' => result.push_str(r"\0C"),
            _ => result.push(c as char),
        }
    }
    result
}

pub struct Compiler {
    output:       String,
    declarations: String,
    globals:      IndexVec<GlobalId, Rc<str>>,
    ids:          FxHashMap<IrId, Type>,
}

impl Compiler {
    pub fn new(
        globals: IndexVec<GlobalId, Rc<str>>,
        vtables: Vec<Instr>,
        strings: FxHashMap<GlobalId, Rc<str>>,
    ) -> Self {
        let mut declarations = String::from(
            "target triple = \"x86_64-unknown-linux-gnu\"\n\
            %Object = type { ptr, ptr }\n\
            %String = type { ptr, i64 }\n\
            declare ccc %Object @Object.new(ptr)\n\
            declare ccc %Object @Object.abort(%Object)\n\
            declare ccc %String @Object.type_name(%Object)\n\
            declare ccc %Object @Object.copy(%Object)\n\
            declare ccc i64     @Int.new(ptr)\n\
            declare ccc %String @Int.type_name(i64)\n\
            declare ccc i64     @Int.copy(i64)\n\
            declare ccc %Object @Int.Cast(i64)\n\
            declare ccc i1      @Bool.new(ptr)\n\
            declare ccc %String @Bool.type_name(i1)\n\
            declare ccc i1      @Bool.copy(i1)\n\
            declare ccc %Object @Bool.Cast(i1)\n\
            declare ccc %String @String.new(ptr)\n\
            declare ccc %String @String.type_name(%String)\n\
            declare ccc %String @String.copy(%String)\n\
            declare ccc %Object @String.Cast(%String)\n\
            declare ccc i64     @String.length(%String)\n\
            declare ccc %String @String.concat(%String, %String)\n\
            declare ccc %String @String.substr(%String, i64, i64)\n\
            declare ccc %Object @IO.new(ptr)\n\
            declare ccc %String @IO.type_name(%Object)\n\
            declare ccc %Object @IO.copy(%Object)\n\
            declare ccc %Object @IO.out_string(%Object, %String)\n\
            declare ccc %Object @IO.out_int(%Object, i64)\n\
            declare ccc %String @IO.in_string(%Object)\n\
            declare ccc i64     @IO.in_int(%Object)\n\
            declare ccc %Object @Allocator.new(ptr)\n\
            declare ccc %Object @Allocator.type_name(%Object)\n\
            declare ccc %Object @Allocator.copy(%Object)\n\
            declare ccc ptr     @Allocator.alloc(i64)\n\
            declare ccc %Object @Allocator.free(ptr)\n\
            ",
        );
        let mut ids = FxHashMap::default();

        for (id, fns) in vtables.into_iter().map(|instr| match instr {
            Instr::Vtable(id, fns) => (id, fns),
            _ => unreachable!(),
        }) {
            ids.insert(IrId::Global(id), Type::Ptr);
            declarations.push_str(&format!(
                "@{} = constant [{} x ptr] [\n",
                globals[id],
                fns.len()
            ));
            for (i, fn_id) in fns.iter().enumerate() {
                match fn_id {
                    Some(fn_id) => {
                        ids.insert(IrId::Global(*fn_id), Type::Ptr);
                        declarations.push_str(&format!("    ptr @{}", globals[*fn_id]));
                    }
                    None => declarations.push_str("    ptr null"),
                }
                if i + 1 < fns.len() {
                    declarations.push_str(",\n");
                } else {
                    declarations.push_str("\n]\n");
                }
            }
        }

        for (id, string) in strings {
            ids.insert(IrId::Global(id), Type::Ptr);
            declarations.push_str(&format!(
                "@{}.raw = private unnamed_addr constant [{} x i8] c\"{}\"\n",
                globals[id],
                string.len(),
                llvm_string(&string)
            ));
            declarations.push_str(&format!(
                "@{} = private unnamed_addr constant %String {{ ptr @{}.raw, i64 {} }}\n",
                globals[id],
                globals[id],
                string.len(),
            ));
        }

        Self {
            output: String::new(),
            declarations,
            ids,
            globals,
        }
    }

    pub fn finish(self) -> String {
        format!("{}{}", self.declarations, self.output)
    }

    fn push_str(&mut self, s: &str) {
        self.output.push_str(s)
    }

    fn get_type(&self, id: IrId) -> Type {
        self.ids[&id]
    }

    fn compile_value_with_type(&mut self, ty: Type, val: &Value) {
        self.push_str(&format!("{} {}", ty, val.to_llvm_string()))
    }

    fn compile_value(&mut self, val: &Value) {
        match val {
            Value::Int(i) => {
                self.push_str(&format!("i64 {}", i));
            }
            Value::Bool(b) => {
                self.push_str(&format!("i1 {}", b));
            }
            Value::Id(id) => {
                self.push_str(&format!(
                    "{} {}",
                    self.get_type(*id),
                    id.to_ir_string(&self.globals)
                ));
            }
            Value::Void => {
                self.push_str("%Object { ptr null, ptr null }");
            }
        }
    }

    fn get_id(&self, id: IrId) -> String {
        id.to_ir_string(&self.globals)
    }

    pub fn compile_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::Nop => {}
            Instr::Jmp(block) => {
                self.push_str(&format!("    br label %{}\n", block));
            }
            Instr::JmpCond {
                src,
                on_true,
                on_false,
            } => {
                self.push_str(&format!(
                    "    br i1 {}, label %{}, label %{}\n",
                    src, on_true, on_false
                ));
            }
            Instr::Return(src) => {
                self.push_str("    ret ");
                self.compile_value(src);
                self.push_str("\n}\n");
            }
            Instr::Function { id, ret, params } => {
                self.push_str(&format!("define ccc {} @{}(", ret, self.globals[*id]));
                for (i, (ty, param)) in params.iter().enumerate() {
                    self.ids.insert(*param, *ty);
                    self.push_str(&format!("{} {}", ty, self.get_id(*param)));
                    if i + 1 < params.len() {
                        self.push_str(", ");
                    }
                }
                self.push_str(") {\n");
            }
            Instr::Label(id) => {
                self.push_str(&format!("{}:\n", id));
            }
            Instr::AssignGep { dst, src, offset } => {
                self.ids.insert(*dst, Type::Ptr);
                self.push_str(&format!(
                    "    {} = getelementptr ptr, ptr {}, i64 {}\n",
                    self.get_id(*dst),
                    self.get_id(*src),
                    offset
                ));
            }
            Instr::AssignLoad { dst, ty, src } => {
                self.ids.insert(*dst, *ty);
                self.push_str(&format!(
                    "    {} = load {}, ptr {}\n",
                    self.get_id(*dst),
                    ty,
                    self.get_id(*src)
                ));
            }
            Instr::Store { dst, src, ty } => {
                self.ids.insert(*dst, Type::Ptr);
                self.push_str("    store ");
                self.compile_value_with_type(*ty, src);
                self.push_str(&format!(", ptr {}\n", self.get_id(*dst)));
            }
            Instr::AssignCall(dst, ret, src, args) => {
                self.ids.insert(*dst, *ret);
                self.push_str(&format!(
                    "    {} = call {} {}(",
                    self.get_id(*dst),
                    ret,
                    self.get_id(*src)
                ));
                for (i, (ty, arg)) in args.iter().enumerate() {
                    self.compile_value_with_type(*ty, arg);
                    if i + 1 < args.len() {
                        self.push_str(", ");
                    }
                }
                self.push_str(")\n");
            }
            Instr::AssignInsert {
                dst,
                ty,
                src,
                val,
                idx,
            } => {
                self.ids.insert(*dst, *ty);
                self.push_str(&format!(
                    "    {} = insertvalue {} {}, ",
                    self.get_id(*dst),
                    ty,
                    self.get_id(*src),
                ));
                self.compile_value(val);
                self.push_str(&format!(", {}\n", idx));
            }
            Instr::AssignExtract {
                dst,
                src_ty,
                src,
                ty,
                offset,
            } => {
                self.ids.insert(*dst, *ty);
                self.push_str(&format!(
                    "    {} = extractvalue {} {}, {}\n",
                    self.get_id(*dst),
                    src_ty,
                    self.get_id(*src),
                    offset
                ));
            }

            _ => todo!("{:?}", instr),
        }
    }

    pub fn compile_fn(&mut self, function: &Function) {
        for instr in function.instrs().inner() {
            self.compile_instr(instr);
        }
    }
}
