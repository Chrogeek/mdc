use crate::context::*;
use crate::util::*;
use std::io::Write;

pub enum ProgramItem {
    Function(Function),
    Declaration(Declaration),
}

pub struct Program {
    pub items: Vec<ProgramItem>,
}

impl Program {
    pub fn emit(self, context: &mut Context<impl Write, impl Write, impl Write>) {
        let mut entry = false;
        context.put_directive(".text");
        context.put_directive(".globl main");
        context.write_data(".data");
        context.write_bss(".bss");
        for item in self.items.into_iter() {
            match item {
                ProgramItem::Function(function) => {
                    if function.name == "main" {
                        entry = true;
                    }
                    function.emit(context);
                }
                ProgramItem::Declaration(declaration) => {
                    context.create_global_variable(&declaration.name, &declaration.ty);
                    let mangled = mangle_global_variable(&declaration.name);
                    match &declaration.default {
                        None => {
                            context.write_bss(&format!(
                                ".comm {}, {}, 4",
                                mangled,
                                declaration.ty.measure()
                            ));
                        }
                        Some(expression) => {
                            context.write_data(".align 4");
                            context.write_data(&format!(".size {}, 4", mangled));
                            context.write_data(&format!("{}:", mangled));
                            if let Expression::IntegerLiteral(value) = expression {
                                context.write_data(&format!(".word {}", value));
                            } else {
                                panic!();
                            }
                        }
                    };
                }
            }
        }
        context.put_label("main".to_string());
        context.put_jump("__main".to_string());
        assert!(entry);
        context.check_undefined_symbol();
    }
}

pub struct Function {
    pub ty: Type,
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub body: Option<Vec<BlockItem>>,
}

impl Function {
    pub fn emit(self, context: &mut Context<impl Write, impl Write, impl Write>) {
        match self.body {
            Some(body) => {
                context.define_function(
                    &self.name,
                    self.ty,
                    self.parameters.iter().map(|t| t.1.clone()).collect(),
                );
                context.enter_function(&self.name);
                context.put_set_frame(self.name);
                context.enter_scope();
                let mut offset = 0;
                for (parameter_name, parameter_type) in self.parameters.into_iter() {
                    context.create_located(&parameter_name, &parameter_type, offset + 8);
                    offset += parameter_type.measure() as i32;
                }
                for item in body.into_iter() {
                    item.emit(context);
                }
                context.put_push(0);
                context.put_return(); // default return value: 0
                context.leave_scope();
                context.exit_function();
                context.put_end_frame();
            }
            None => {
                context.declare_function(
                    &self.name,
                    self.ty,
                    self.parameters.into_iter().map(|tuple| tuple.1).collect(),
                );
            }
        }
    }
}

pub enum Statement {
    Empty,
    Return(Expression),
    Expression(Expression),
    If {
        condition: Expression,
        true_branch: Box<Statement>,
        false_branch: Option<Box<Statement>>,
    },
    Compound(Compound),
    Loop {
        initializer: Option<Box<BlockItem>>,
        condition: Option<Expression>,
        body: Box<Statement>,
        modifier: Option<Expression>,
    },
    Break,
    Continue,
}

impl Statement {
    pub fn emit(self, context: &mut Context<impl Write, impl Write, impl Write>) {
        match self {
            Statement::Empty => {}
            Statement::Return(expression) => {
                let rt = expression.emit(context).0;
                context.check_return_type(rt);
                context.put_return();
            }
            Statement::Expression(expression) => {
                expression.emit(context);
                context.put_pop();
            }
            Statement::If {
                condition,
                true_branch,
                false_branch,
            } => {
                let label_1 = context.next_label();
                let label_2 = context.next_label();
                assert!(condition.emit(context).0.is_primitive());
                context.put_jump_zero(label_1.clone());
                true_branch.emit(context);
                context.put_jump(label_2.clone());
                context.put_label(label_1);
                if let Some(false_part) = false_branch {
                    false_part.emit(context);
                }
                context.put_label(label_2);
            }
            Statement::Compound(compound) => {
                compound.emit(context);
            }
            Statement::Loop {
                initializer,
                condition,
                body,
                modifier,
            } => {
                let label_restart = context.next_label();
                let label_break = context.next_label();
                let label_continue = context.next_label();

                context.enter_loop(label_break.clone(), label_continue.clone());
                context.enter_scope();
                if let Some(item) = initializer {
                    item.emit(context);
                }
                context.put_label(label_restart.clone());
                if let Some(expression) = condition {
                    assert!(expression.emit(context).0.is_primitive());
                    context.put_jump_zero(label_break.clone());
                }
                body.emit(context);
                context.put_label(label_continue);
                if let Some(expression) = modifier {
                    expression.emit(context);
                    context.put_pop();
                }
                context.put_jump(label_restart);
                context.put_label(label_break);
                context.leave_scope();
                context.leave_loop();
            }
            Statement::Break => {
                context.put_jump(context.get_loop_break());
            }
            Statement::Continue => {
                context.put_jump(context.get_loop_continue());
            }
        }
    }
}

pub struct Compound {
    pub items: Vec<BlockItem>,
}

impl Compound {
    pub fn emit(self, context: &mut Context<impl Write, impl Write, impl Write>) {
        context.enter_scope();
        for item in self.items.into_iter() {
            item.emit(context);
        }
        context.leave_scope();
    }
}

pub struct Declaration {
    pub ty: Type,
    pub name: String,
    pub default: Option<Expression>,
}

impl Declaration {
    pub fn emit(self, context: &mut Context<impl Write, impl Write, impl Write>) {
        context.create_variable(&self.name, &self.ty);
        if let Some(expression) = self.default {
            Expression::Assignment(
                Box::new(Expression::Identifier(self.name)),
                Box::new(expression),
            )
            .emit(context);
            context.put_pop();
        }
    }
}

pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

impl BlockItem {
    pub fn emit(self, context: &mut Context<impl Write, impl Write, impl Write>) {
        match self {
            BlockItem::Statement(statement) => statement.emit(context),
            BlockItem::Declaration(declaration) => declaration.emit(context),
        }
    }
}

pub enum Expression {
    IntegerLiteral(i32),
    Identifier(String),
    Negation(Box<Expression>),
    Not(Box<Expression>),
    LogicalNot(Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Modulus(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Unequal(Box<Expression>, Box<Expression>),
    Less(Box<Expression>, Box<Expression>),
    LessEqual(Box<Expression>, Box<Expression>),
    Greater(Box<Expression>, Box<Expression>),
    GreaterEqual(Box<Expression>, Box<Expression>),
    LogicalAnd(Box<Expression>, Box<Expression>),
    LogicalOr(Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    Reference(Box<Expression>),
    Dereference(Box<Expression>),
    Convert(Type, Box<Expression>),
    Index(Box<Expression>, Vec<Expression>),
}

impl Expression {
    pub fn emit(
        self,
        context: &mut Context<impl Write, impl Write, impl Write>,
    ) -> (Type, LeftValue) {
        macro_rules! make_unary_emitter {
            ($rhs: ident, $next: ident) => {{
                assert!($rhs.emit(context).0.is_primitive());
                context.$next();
                (Type::make_primitive(), false)
            }};
        }

        macro_rules! make_binary_operator {
            ($lhs: ident, $rhs: ident, $instruction: ident) => {{
                assert!($lhs.emit(context).0.is_primitive());
                assert!($rhs.emit(context).0.is_primitive());
                context.$instruction();
                (Type::make_primitive(), false)
            }};
        }

        macro_rules! make_binary_pointer_operator {
            ($lhs: ident, $rhs: ident, $instruction: ident) => {{
                let lt = $lhs.emit(context).0;
                let rt = $rhs.emit(context).0;
                assert!(lt == rt && !lt.is_array());
                context.$instruction();
                (Type::make_primitive(), false)
            }};
        }

        match self {
            Expression::IntegerLiteral(value) => {
                context.put_push(value);
                (Type::make_primitive(), false)
            }
            Expression::Identifier(name) => {
                let rt = context.access_variable(&name);
                if !rt.is_array() {
                    context.put_load();
                }
                (rt, true)
            }
            Expression::Negation(rhs) => make_unary_emitter!(rhs, put_negate),
            Expression::Not(rhs) => make_unary_emitter!(rhs, put_not),
            Expression::LogicalNot(rhs) => make_unary_emitter!(rhs, put_logical_not),
            Expression::Addition(lhs, rhs) => {
                let lt = lhs.emit(context).0;
                let rt = rhs.emit(context).0;
                let ret = if lt.is_primitive() && rt.is_primitive() {
                    context.put_add();
                    Type::make_primitive()
                } else if lt.is_primitive() && rt.is_pointer() {
                    context.put_add_pointer_left();
                    rt
                } else if lt.is_pointer() && rt.is_primitive() {
                    context.put_add_pointer_right();
                    lt
                } else {
                    panic!();
                };
                (ret, false)
            }
            Expression::Subtraction(lhs, rhs) => {
                let lt = lhs.emit(context).0;
                let rt = rhs.emit(context).0;
                let ret = if lt.is_primitive() && rt.is_primitive() {
                    context.put_subtract();
                    Type::make_primitive()
                } else if lt.is_pointer() && rt.is_primitive() {
                    context.put_negate();
                    context.put_add_pointer_right();
                    lt
                } else if lt.is_pointer() && rt.is_pointer() {
                    assert!(lt == rt);
                    context.put_subtract();
                    context.put_push(4);
                    context.put_divide();
                    Type::make_primitive()
                } else {
                    panic!();
                };
                (ret, false)
            }
            Expression::Multiplication(lhs, rhs) => make_binary_operator!(lhs, rhs, put_multiply),
            Expression::Division(lhs, rhs) => make_binary_operator!(lhs, rhs, put_divide),
            Expression::Modulus(lhs, rhs) => make_binary_operator!(lhs, rhs, put_modulo),
            Expression::Equal(lhs, rhs) => make_binary_pointer_operator!(lhs, rhs, put_equal),
            Expression::Unequal(lhs, rhs) => make_binary_pointer_operator!(lhs, rhs, put_unequal),
            Expression::Less(lhs, rhs) => make_binary_operator!(lhs, rhs, put_less),
            Expression::LessEqual(lhs, rhs) => make_binary_operator!(lhs, rhs, put_less_equal),
            Expression::Greater(lhs, rhs) => make_binary_operator!(lhs, rhs, put_greater),
            Expression::GreaterEqual(lhs, rhs) => {
                make_binary_operator!(lhs, rhs, put_greater_equal)
            }
            Expression::LogicalAnd(lhs, rhs) => make_binary_operator!(lhs, rhs, put_logical_and),
            Expression::LogicalOr(lhs, rhs) => make_binary_operator!(lhs, rhs, put_logical_or),
            Expression::Assignment(lhs, rhs) => {
                let rt = rhs.emit(context).0;
                let lt = Expression::Reference(lhs).emit(context).0;
                assert!(rt == lt.unwrap_pointer());
                context.put_store();
                (rt, false)
            }
            Expression::Ternary(condition, true_part, false_part) => {
                let label_1 = context.next_label();
                let label_2 = context.next_label();
                assert!(condition.emit(context).0.is_primitive());
                context.put_jump_zero(label_1.clone());
                let lt = true_part.emit(context).0;
                context.put_jump(label_2.clone());
                context.put_label(label_1);
                let rt = false_part.emit(context).0;
                assert!(lt == rt);
                context.put_label(label_2);
                (lt, false)
            }
            Expression::FunctionCall(name, arguments) => {
                let mut types = Vec::new();
                let arg_count = arguments.len();
                for argument in arguments.into_iter().rev() {
                    types.push(argument.emit(context).0);
                }
                context.check_arguments(&name, &types);
                context.put_call(&name);
                context.mark_function_called(&name);
                for _ in 0..arg_count {
                    context.put_pop();
                }
                context.put_returned_value();
                (context.get_function_return_type(&name), false)
            }
            Expression::Reference(rhs) => (
                match *rhs {
                    Expression::Identifier(name) => {
                        let ty = context.access_variable(&name);
                        assert!(!ty.is_array());
                        ty.wrap_pointer()
                    }
                    Expression::Dereference(rrhs) => rrhs.emit(context).0,
                    Expression::Index(base, indices) => {
                        let mut ty = base.emit(context).0;
                        let indices_count = indices.len();
                        let mut i = 0;
                        for index in indices.into_iter() {
                            if ty.is_array() {
                                ty = ty.unwrap_array();
                            } else if ty.is_pointer() {
                                ty = ty.unwrap_pointer();
                            } else {
                                panic!();
                            }
                            assert!(index.emit(context).0.is_primitive());
                            context.put_push(ty.measure() as i32);
                            context.put_multiply();
                            context.put_add();
                            if !ty.is_array() && i + 1 < indices_count {
                                context.put_load();
                            }
                            i += 1;
                        }
                        ty.wrap_pointer()
                    }
                    Expression::Convert(target, rhs) => {
                        assert!(!target.is_array());
                        rhs.emit(context);
                        target.wrap_pointer()
                    }
                    _ => unreachable!(),
                },
                false,
            ),
            Expression::Dereference(rhs) => {
                let rt = rhs.emit(context).0;
                assert!(rt.is_pointer());
                context.put_load();
                (rt.unwrap_pointer(), true)
            }
            Expression::Convert(target, rhs) => {
                assert!(!target.is_array());
                let (_, vt) = rhs.emit(context);
                (target, vt)
            }
            Expression::Index(base, indices) => {
                let mut ty = base.emit(context).0;
                for index in indices.into_iter() {
                    if ty.is_array() {
                        ty = ty.unwrap_array();
                    } else if ty.is_pointer() {
                        ty = ty.unwrap_pointer();
                    } else {
                        panic!();
                    }
                    assert!(index.emit(context).0.is_primitive());
                    context.put_push(ty.measure() as i32);
                    context.put_multiply();
                    context.put_add();
                    if !ty.is_array() {
                        context.put_load();
                    }
                }
                (ty, true)
            }
        }
    }
}
