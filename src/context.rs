use crate::util::*;
use std::collections::HashMap;
use std::io::Write;

macro_rules! assembly {
    ($this: expr, $($instruction: expr),*) => {{
            $(
                writeln!($this.text_output, "{}", $instruction.to_string()).unwrap();
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

pub struct Scope {
    start: i32,                           // staring memory location of this scope
    offset: i32,                          // current memory location of this scope
    variables: HashMap<String, Variable>, // map from variable name to memory location
}

impl Scope {
    pub fn new(offset: i32) -> Scope {
        Scope {
            start: offset,
            offset,
            variables: HashMap::new(),
        }
    }

    pub fn create_variable(&mut self, name: &String, ty: &Type) {
        assert!(!self.variables.contains_key(name));
        self.offset -= ty.measure() as i32;
        self.variables.insert(
            name.clone(),
            Variable {
                ty: ty.clone(),
                offset: self.offset,
            },
        );
    }

    pub fn access_variable(&self, name: &String) -> Option<Variable> {
        Some(self.variables.get(name)?.clone())
    }

    // Creates a variable with specified memory offset, dedicated for arguments.
    // Offset must be outside of current scope frame.
    pub fn create_located(&mut self, name: &String, ty: &Type, offset: i32) {
        assert!(offset >= self.start);
        assert!(!self.variables.contains_key(name));
        self.variables.insert(
            name.clone(),
            Variable {
                ty: ty.clone(),
                offset,
            },
        );
    }
}

pub struct Loop {
    label_break: String,
    label_continue: String,
}

#[derive(PartialEq)]
pub struct Primitive {
    return_type: Type,
    parameters: Vec<Type>,
    called: bool,
    defined: bool,
}

pub struct Context<T: Write, U: Write, V: Write> {
    text_output: T,
    data_output: U,
    bss_output: V,
    current_function: Option<String>,
    label_count: i32,
    offset: i32,
    scope_stack: Vec<Scope>,
    loop_stack: Vec<Loop>,
    global_variables: HashMap<String, Type>,
    function_table: HashMap<String, Primitive>,
}

impl<T: Write, U: Write, V: Write> Context<T, U, V> {
    pub fn new(text_output: T, data_output: U, bss_output: V) -> Context<T, U, V> {
        Context {
            text_output,
            data_output,
            bss_output,
            current_function: None,
            label_count: 0,
            offset: 0,
            scope_stack: Vec::new(),
            loop_stack: Vec::new(),
            global_variables: HashMap::new(),
            function_table: HashMap::new(),
        }
    }

    pub fn write_data(&mut self, asm: &str) {
        writeln!(self.data_output, "{}", asm).unwrap();
    }

    pub fn write_bss(&mut self, asm: &str) {
        writeln!(self.bss_output, "{}", asm).unwrap();
    }

    pub fn enter_scope(&mut self) {
        self.scope_stack.push(Scope::new(self.offset));
    }

    pub fn leave_scope(&mut self) {
        self.offset = self.scope_stack.last().unwrap().start;
        self.scope_stack.pop();
    }

    pub fn enter_loop(&mut self, label_break: String, label_continue: String) {
        self.loop_stack.push(Loop {
            label_break,
            label_continue,
        });
    }

    pub fn leave_loop(&mut self) {
        self.loop_stack.pop();
    }

    pub fn get_loop_break(&self) -> String {
        self.loop_stack.last().unwrap().label_break.clone()
    }

    pub fn get_loop_continue(&self) -> String {
        self.loop_stack.last().unwrap().label_continue.clone()
    }

    pub fn create_global_variable(&mut self, name: &String, ty: &Type) {
        assert!(!self.function_table.contains_key(name));
        assert!(self
            .global_variables
            .insert(name.clone(), ty.clone())
            .is_none());
    }

    pub fn access_global_variable(&mut self, name: &String) -> Type {
        if let Some(variable_type) = self.global_variables.get(name) {
            self.offset -= 4;
            assembly!(
                self,
                "addi sp, sp, -4",
                &format!("la t1, {}", mangle_global_variable(name)),
                "sw t1, 0(sp)"
            );
            variable_type.clone()
        } else {
            panic!();
        }
    }

    // Returns the address of created variable
    pub fn create_variable(&mut self, name: &String, ty: &Type) -> i32 {
        self.scope_stack
            .last_mut()
            .unwrap()
            .create_variable(name, ty);
        self.put_allocate(ty.measure() as i32);
        self.offset
    }

    pub fn access_variable(&mut self, name: &String) -> Type {
        for scope in self.scope_stack.iter().rev() {
            if let Some(variable) = scope.access_variable(name) {
                self.put_locate(variable.offset);
                return variable.ty;
            }
        }
        self.access_global_variable(name)
    }

    // Creates a variable with specified offset, used for arguments
    pub fn create_located(&mut self, name: &String, ty: &Type, offset: i32) {
        self.scope_stack
            .last_mut()
            .unwrap()
            .create_located(name, ty, offset);
    }

    pub fn enter_function(&mut self, function_name: &String) {
        self.current_function = Some(function_name.clone());
        self.put_label(mangle_function_name(function_name));
    }

    pub fn exit_function(&mut self) {
        self.put_label(get_function_epilogue(
            &self.current_function.clone().unwrap(),
        ));
        self.current_function = None;
    }

    pub fn declare_function(&mut self, name: &String, return_type: Type, parameters: Vec<Type>) {
        assert!(!self.global_variables.contains_key(name));
        if !self.function_table.contains_key(name) {
            self.function_table.insert(
                name.clone(),
                Primitive {
                    return_type,
                    parameters,
                    called: false,
                    defined: false,
                },
            );
        } else {
            let function = self.function_table.get(name).unwrap();
            assert!(function.return_type == return_type && function.parameters == parameters);
        }
    }

    pub fn define_function(&mut self, name: &String, return_type: Type, parameters: Vec<Type>) {
        assert!(!self.global_variables.contains_key(name));
        if let Some(primitive) = self.function_table.get_mut(name) {
            if primitive.defined
                || primitive.return_type != return_type
                || primitive.parameters != parameters
            {
                panic!();
            } else {
                primitive.defined = true;
            }
        } else {
            self.function_table.insert(
                name.clone(),
                Primitive {
                    return_type,
                    parameters,
                    called: false,
                    defined: true,
                },
            );
        }
    }

    pub fn mark_function_called(&mut self, name: &String) {
        self.function_table.get_mut(name).unwrap().called = true;
    }

    pub fn get_function_return_type(&self, name: &String) -> Type {
        self.function_table.get(name).unwrap().return_type.clone()
    }

    // Checks if there exists any called but undefined functions.
    // Declared, undefined while never called function primitives are not considered error.
    pub fn check_undefined_symbol(&self) {
        for primitive in self.function_table.values().into_iter() {
            assert!(!primitive.called || primitive.defined);
        }
    }

    pub fn check_arguments(&self, name: &String, arguments: &Vec<Type>) {
        let size = arguments.len();

        assert_eq!(
            size,
            self.function_table.get(name).unwrap().parameters.len()
        );

        for i in 0..size {
            assert!(
                self.function_table.get(name).unwrap().parameters[i] == arguments[size - 1 - i]
            );
        }
    }

    pub fn check_return_type(&self, return_type: Type) {
        assert!(
            return_type == self.get_function_return_type(self.current_function.as_ref().unwrap())
        );
    }

    pub fn next_label(&mut self) -> String {
        self.label_count += 1;
        "_label_".to_string() + &self.label_count.to_string()
    }

    pub fn put_directive(&mut self, directive: &str) {
        assembly!(self, directive);
    }

    pub fn put_push(&mut self, value: i32) {
        self.offset -= 4;
        assembly!(
            self,
            "addi sp, sp, -4",
            &format!("li t1, {}", value),
            "sw t1, 0(sp)"
        );
    }

    pub fn put_pop(&mut self) {
        self.offset += 4;
        assembly!(self, "addi sp, sp, 4");
    }

    pub fn put_return(&mut self) {
        self.offset += 4;
        assembly!(
            self,
            "lw a0, 0(sp)",
            "addi sp, sp, 4",
            &format!(
                "j {}",
                get_function_epilogue(&self.current_function.clone().unwrap())
            )
        );
    }

    pub fn put_returned_value(&mut self) {
        self.offset -= 4;
        assembly!(self, "addi sp, sp, -4", "sw a0, 0(sp)");
    }

    pub fn put_negate(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "neg t1, t1", "sw t1, 0(sp)");
    }

    pub fn put_not(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "not t1, t1", "sw t1, 0(sp)");
    }

    pub fn put_logical_not(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "seqz t1, t1", "sw t1, 0(sp)");
    }

    pub fn put_add(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "add t1, t1, t2");
    }

    pub fn put_add_pointer_left(&mut self) {
        self.offset += 4;
        assembly!(
            self,
            "lw t1, 4(sp)",
            "slli t1, t1, 2",
            "lw t2, 0(sp)",
            "add t1, t1, t2",
            "addi sp, sp, 4",
            "sw t1, 0(sp)"
        );
    }

    pub fn put_add_pointer_right(&mut self) {
        self.offset += 4;
        assembly!(
            self,
            "lw t1, 4(sp)",
            "lw t2, 0(sp)",
            "slli t2, t2, 2",
            "add t1, t1, t2",
            "addi sp, sp, 4",
            "sw t1, 0(sp)"
        );
    }

    pub fn put_subtract(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "sub t1, t1, t2");
    }

    pub fn put_multiply(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "mul t1, t1, t2");
    }

    pub fn put_divide(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "div t1, t1, t2");
    }

    pub fn put_modulo(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "rem t1, t1, t2");
    }

    pub fn put_equal(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "sub t1, t1, t2", "seqz t1, t1");
    }

    pub fn put_unequal(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "sub t1, t1, t2", "snez t1, t1");
    }

    pub fn put_less(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "slt t1, t1, t2");
    }

    pub fn put_less_equal(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "slt t1, t2, t1", "seqz t1, t1");
    }

    pub fn put_greater(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "slt t1, t2, t1");
    }

    pub fn put_greater_equal(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "slt t1, t1, t2", "seqz t1, t1");
    }

    pub fn put_logical_and(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "snez t1, t1", "snez t2, t2", "and t1, t1, t2");
    }

    pub fn put_logical_or(&mut self) {
        self.offset += 4;
        binary_operator_assembly!(self, "or t1, t1, t2", "snez t1, t1");
    }

    pub fn put_load(&mut self) {
        assembly!(self, "lw t1, 0(sp)", "lw t1, 0(t1)", "sw t1, 0(sp)");
    }

    pub fn put_store(&mut self) {
        self.offset += 4;
        assembly!(
            self,
            "lw t1, 4(sp)",
            "lw t2, 0(sp)",
            "addi sp, sp, 4",
            "sw t1, 0(t2)"
        );
    }

    pub fn put_allocate(&mut self, size: i32) {
        self.offset -= size;
        assembly!(self, &format!("addi sp, sp, -{}", size));
    }

    pub fn put_locate(&mut self, position: i32) {
        self.offset -= 4;
        assembly!(
            self,
            "addi sp, sp, -4",
            &format!("addi t1, fp, {}", position),
            "sw t1, 0(sp)"
        );
    }

    pub fn put_set_frame(&mut self, name: String) {
        self.offset = 0;
        self.current_function = Some(name.clone());
        assembly!(
            self,
            "addi sp, sp, -4",
            "sw ra, 0(sp)", // save ra
            "addi sp, sp, -4",
            "sw fp, 0(sp)",   // save fp
            "addi fp, sp, 0"  // start new stack frame
        );
    }

    pub fn put_end_frame(&mut self) {
        self.offset = 0;
        self.current_function = None;
        assembly!(
            self,
            "addi sp, fp, 0", // free local variables
            "lw fp, 0(sp)",   // restore fp
            "addi sp, sp, 4",
            "lw ra, 0(sp)", // restore ra
            "addi sp, sp, 4",
            "jr ra" // jump return
        );
    }

    pub fn put_jump_zero(&mut self, label: String) {
        self.offset += 4;
        assembly!(
            self,
            "lw t1, 0(sp)",
            "addi sp, sp, 4",
            &format!("beqz t1, {}", label)
        );
    }

    pub fn put_jump(&mut self, label: String) {
        assembly!(self, &format!("j {}", label));
    }

    pub fn put_label(&mut self, label: String) {
        assembly!(self, &format!("{}:", label));
    }

    pub fn put_call(&mut self, function_name: &String) {
        assembly!(
            self,
            &format!("call {}", mangle_function_name(&function_name))
        );
    }
}
