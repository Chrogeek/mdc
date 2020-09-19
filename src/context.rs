use crate::util::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Write;

macro_rules! assembly {
    ($this: expr, $($instruction: expr),*) => {{
            $(
                writeln!($this.output, "{}", $instruction.to_string()).unwrap();
            )*
        }
    };
}

macro_rules! binary_operator_assembly {
    ($this: expr, $($instruction: expr),*) => {
        assembly!($this,
            "lw t1, 4(sp)", "lw t2, 0(sp)", $($instruction,)* "addi sp, sp, 4", "sw t1, 0(sp)"
        )
    };
}

pub struct Context<T: Write> {
    output: T,

    variables: HashSet<String>,
    current_function: Option<String>,
    label_count: usize,

    lookup: HashMap<String, usize>,
    offset: usize,
}

impl<T: Write> Context<T> {
    pub fn new(output: T) -> Context<T> {
        Context {
            output,
            variables: HashSet::new(),
            current_function: None,
            label_count: 0,
            lookup: HashMap::new(),
            offset: 0,
        }
    }

    pub fn create_variable(&mut self, r#type: &Type, name: &String) {
        if self.variables.insert(name.clone()) {
            self.put_allocate(r#type.measure(), name.clone())
        } else {
            panic!("Redefinition of variable {}", name);
        }
    }

    pub fn access_variable(&mut self, name: &String) {
        if self.variables.contains(name) {
            self.put_locate(name.clone())
        } else {
            panic!("Undefined variable: {}", name);
        }
    }

    pub fn enter_function(&mut self, function_name: &String) {
        self.current_function = Some(function_name.clone());
        self.put_directive(&format!(
            ".globl {0}\n{0}:",
            mangle_function_name(function_name)
        ))
    }

    pub fn exit_function(&mut self) {
        let assembly = self.put_directive(&format!(
            "{0}:",
            get_function_epilogue(&self.current_function.clone().unwrap())
        ));
        self.current_function = None;
        assembly
    }

    pub fn next_label(&mut self) -> String {
        self.label_count += 1;
        "_label_".to_string() + &self.label_count.to_string()
    }

    pub fn put_directive(&mut self, directive: &str) {
        assembly!(self, directive)
    }

    pub fn put_push(&mut self, value: i32) {
        self.offset += 4;
        assembly!(
            self,
            "addi sp, sp, -4",
            &format!("li t1, {}", value),
            "sw t1, 0(sp)"
        )
    }

    pub fn put_pop(&mut self) {
        self.offset -= 4;
        assembly!(self, "addi sp, sp, 4")
    }

    pub fn put_return(&mut self) {
        self.offset -= 4;
        assembly!(
            self,
            "lw a0, 0(sp)",
            "addi sp, sp, 4",
            &format!(
                "j {}",
                get_function_epilogue(&self.current_function.clone().unwrap())
            )
        )
    }

    pub fn put_negate(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "neg t1, t1", "sw t1, 0(sp)")
    }

    pub fn put_not(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "not t1, t1", "sw t1, 0(sp)")
    }

    pub fn put_logical_not(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "seqz t1, t1", "sw t1, 0(sp)")
    }

    pub fn put_add(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "add t1, t1, t2")
    }

    pub fn put_subtract(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "sub t1, t1, t2")
    }

    pub fn put_multiply(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "mul t1, t1, t2")
    }

    pub fn put_divide(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "div t1, t1, t2")
    }

    pub fn put_modulo(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "rem t1, t1, t2")
    }

    pub fn put_equal(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "sub t1, t1, t2", "seqz t1, t1")
    }

    pub fn put_unequal(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "sub t1, t1, t2", "snez t1, t1")
    }

    pub fn put_less(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "slt t1, t1, t2")
    }

    pub fn put_less_equal(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "slt t1, t2, t1", "seqz t1, t1")
    }

    pub fn put_greater(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "slt t1, t2, t1")
    }

    pub fn put_greater_equal(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "slt t1, t1, t2", "seqz t1, t1")
    }

    pub fn put_logical_and(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "snez t1, t1", "snez t2, t2", "and t1, t1, t2")
    }

    pub fn put_logical_or(&mut self) {
        self.offset -= 4;
        binary_operator_assembly!(self, "or t1, t1, t2", "snez t1, t1")
    }

    pub fn put_load(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "lw t1, 0(t1)", "sw t1, 0(sp)")
    }

    pub fn put_store(&mut self) {
        self.offset -= 4;
        assembly!(
            self,
            "lw t1, 4(sp)",
            "lw t2, 0(sp)",
            "addi sp, sp, 4",
            "sw t1, 0(t2)"
        )
    }

    pub fn put_allocate(&mut self, size: usize, name: String) {
        self.offset += size;
        self.lookup.insert(name.clone(), self.offset);
        assembly!(self, &format!("addi sp, sp, -{}", size))
    }

    pub fn put_locate(&mut self, name: String) {
        self.offset += 4;
        assembly!(
            self,
            "addi sp, sp, -4",
            &format!("addi t1, fp, -{}", self.lookup.get(&name).unwrap()),
            "sw t1, 0(sp)"
        )
    }

    pub fn put_set_frame(&mut self, name: String) {
        self.offset = 0;
        self.current_function = Some(name.clone());
        assembly!(
            self,
            "addi sp, sp, -4",
            "sw fp, 0(sp)",   // save fp
            "addi fp, sp, 0"  // start new stack frame
        )
    }

    pub fn put_end_frame(&mut self) {
        self.offset = 0;
        self.current_function = None;
        assembly!(
            self,
            "addi sp, fp, 0", // free local variables
            "lw t1, 0(sp)",
            "addi sp, sp, 4",
            "addi fp, t1, 0", // restore fp
            "jr ra"           // jump return
        )
    }

    pub fn put_jump_zero(&mut self, label: String) {
        self.offset -= 4;
        assembly!(
            self,
            "lw t1, 0(sp)",
            "addi sp, sp, 4",
            &format!("beqz t1, {}", label)
        )
    }

    pub fn put_jump(&mut self, label: String) {
        assembly!(self, &format!("j {}", label))
    }

    pub fn put_label(&mut self, label: String) {
        assembly!(self, &format!("{}:", label))
    }
}
