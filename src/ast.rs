use crate::context::*;
use crate::util::*;
use std::io::Write;

#[derive(Debug)]
pub enum ProgramItem {
    Function(Function),
    Declaration(Declaration),
}

#[derive(Debug)]
pub struct Program {
    pub items: Vec<ProgramItem>,
}

impl Program {
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) {
        let mut entry = false;
        context.put_directive(".text");
        context.put_directive(".globl main");
        context.write_data(".data");
        context.write_bss(".bss");
        for item in self.items.iter() {
            match item {
                ProgramItem::Function(function) => {
                    function.emit(context);
                    if function.name == "main" {
                        entry = true;
                    }
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
                            if let ExpressionKind::IntegerLiteral(value) = expression.kind {
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

#[derive(Debug)]
pub struct Function {
    pub ty: Type,
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub body: Option<Vec<BlockItem>>,
}

impl Function {
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) {
        self.body
            .as_ref()
            .and_then(|body| {
                context.define_function(
                    &self.name,
                    self.ty.clone(),
                    self.parameters
                        .iter()
                        .map(|tuple| tuple.1.clone())
                        .collect(),
                );
                context.enter_function(&self.name);
                context.put_set_frame(self.name.clone());
                context.enter_scope();
                let mut offset = 0;
                for (parameter_name, parameter_type) in self.parameters.iter() {
                    context.create_located(parameter_name, parameter_type, offset + 8);
                    offset += parameter_type.measure() as i32;
                }
                for item in body.iter() {
                    item.emit(context);
                }
                context.put_push(0);
                context.put_return(); // default return value: 0
                context.leave_scope();
                context.exit_function();
                context.put_end_frame();
                Some(())
            })
            .or_else(|| {
                context.declare_function(
                    &self.name,
                    self.ty.clone(),
                    self.parameters
                        .iter()
                        .map(|tuple| tuple.1.clone())
                        .collect(),
                );
                None
            });
    }
}

#[derive(Debug)]
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
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) {
        match self {
            Statement::Empty => {}
            Statement::Return(expression) => {
                let rt = expression.emit(context);
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
                assert!(condition.emit(context).is_primitive());
                context.put_jump_zero(label_1.clone());
                context.enter_scope();
                true_branch.emit(context);
                context.leave_scope();
                context.put_jump(label_2.clone());
                context.put_label(label_1);
                if let Some(false_part) = false_branch {
                    context.enter_scope();
                    false_part.emit(context);
                    context.leave_scope();
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
                    assert!(expression.emit(context).is_primitive());
                    context.put_jump_zero(label_break.clone());
                }
                context.enter_scope();
                body.emit(context);
                context.put_label(label_continue);
                if let Some(expression) = modifier {
                    expression.emit(context);
                    context.put_pop();
                }
                context.leave_scope();
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

#[derive(Debug)]
pub struct Compound {
    pub items: Vec<BlockItem>,
}

impl Compound {
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) {
        context.enter_scope();
        for item in self.items.iter() {
            item.emit(context);
        }
        context.leave_scope();
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub ty: Type,
    pub name: String,
    pub default: Option<Expression>,
}

impl Declaration {
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) {
        context.create_variable(&self.name, &self.ty);
        if let Some(expression) = &self.default {
            Expression {
                kind: ExpressionKind::Assignment(
                    Box::new(Expression {
                        kind: ExpressionKind::Identifier(self.name.clone()),
                        is_lvalue: true,
                    }),
                    Box::new(expression.clone()),
                ),
                is_lvalue: false,
            }
            .emit(context);
            context.put_pop();
        }
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

impl BlockItem {
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) {
        match self {
            BlockItem::Statement(statement) => statement.emit(context),
            BlockItem::Declaration(declaration) => declaration.emit(context),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
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

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub is_lvalue: bool,
}

impl Expression {
    pub fn emit(&self, context: &mut Context<impl Write, impl Write, impl Write>) -> Type {
        macro_rules! make_binary_operator {
            ($lhs: ident, $rhs: ident, $instruction: ident) => {{
                assert!($lhs.emit(context).is_primitive());
                assert!($rhs.emit(context).is_primitive());
                context.$instruction();
                Type::Primitive
            }};
        }

        macro_rules! make_binary_pointer_operator {
            ($lhs: ident, $rhs: ident, $instruction: ident) => {{
                let lt = $lhs.emit(context);
                let rt = $rhs.emit(context);
                assert_eq!(lt, rt);
                assert!(!lt.is_array());
                context.$instruction();
                Type::Primitive
            }};
        }

        match &self.kind {
            ExpressionKind::IntegerLiteral(value) => {
                context.put_push(*value);
                Type::Primitive
            }
            ExpressionKind::Identifier(name) => {
                let rt = context.access_variable(name);
                if rt.is_array() {
                    rt
                } else {
                    context.put_load();
                    rt
                }
            }
            ExpressionKind::Negation(rhs) => {
                assert!(rhs.emit(context).is_primitive());
                context.put_negate();
                Type::Primitive
            }
            ExpressionKind::Not(rhs) => {
                assert!(rhs.emit(context).is_primitive());
                context.put_not();
                Type::Primitive
            }
            ExpressionKind::LogicalNot(rhs) => {
                assert!(rhs.emit(context).is_primitive());
                context.put_logical_not();
                Type::Primitive
            }
            ExpressionKind::Addition(lhs, rhs) => {
                let lt = lhs.emit(context);
                let rt = rhs.emit(context);
                if lt.is_primitive() && rt.is_primitive() {
                    context.put_add();
                    Type::Primitive
                } else if lt.is_primitive() && rt.is_pointer() {
                    context.put_add_pointer_left();
                    rt
                } else if lt.is_pointer() && rt.is_primitive() {
                    context.put_add_pointer_right();
                    lt
                } else {
                    panic!();
                }
            }
            ExpressionKind::Subtraction(lhs, rhs) => {
                let lt = lhs.emit(context);
                let rt = rhs.emit(context);
                if lt.is_primitive() && rt.is_primitive() {
                    context.put_subtract();
                    Type::Primitive
                } else if lt.is_pointer() && rt.is_primitive() {
                    context.put_negate();
                    context.put_add_pointer_right();
                    lt
                } else if lt.is_pointer() && rt.is_pointer() {
                    assert_eq!(lt, rt);
                    context.put_subtract();
                    context.put_push(4);
                    context.put_divide();
                    Type::Primitive
                } else {
                    panic!();
                }
            }
            ExpressionKind::Multiplication(lhs, rhs) => {
                make_binary_operator!(lhs, rhs, put_multiply)
            }
            ExpressionKind::Division(lhs, rhs) => make_binary_operator!(lhs, rhs, put_divide),
            ExpressionKind::Modulus(lhs, rhs) => make_binary_operator!(lhs, rhs, put_modulo),
            ExpressionKind::Equal(lhs, rhs) => make_binary_pointer_operator!(lhs, rhs, put_equal),
            ExpressionKind::Unequal(lhs, rhs) => {
                make_binary_pointer_operator!(lhs, rhs, put_unequal)
            }
            ExpressionKind::Less(lhs, rhs) => make_binary_operator!(lhs, rhs, put_less),
            ExpressionKind::LessEqual(lhs, rhs) => make_binary_operator!(lhs, rhs, put_less_equal),
            ExpressionKind::Greater(lhs, rhs) => make_binary_operator!(lhs, rhs, put_greater),
            ExpressionKind::GreaterEqual(lhs, rhs) => {
                make_binary_operator!(lhs, rhs, put_greater_equal)
            }
            ExpressionKind::LogicalAnd(lhs, rhs) => {
                make_binary_operator!(lhs, rhs, put_logical_and)
            }
            ExpressionKind::LogicalOr(lhs, rhs) => make_binary_operator!(lhs, rhs, put_logical_or),
            ExpressionKind::Assignment(lhs, rhs) => {
                assert!(lhs.is_lvalue);
                let t = rhs.emit(context);
                assert_eq!(
                    t,
                    Expression {
                        kind: ExpressionKind::Reference((*lhs).clone()),
                        is_lvalue: false,
                    }
                    .emit(context)
                    .unwrap_pointer()
                );
                context.put_store();
                t
            }
            ExpressionKind::Ternary(condition, true_part, false_part) => {
                let label_1 = context.next_label();
                let label_2 = context.next_label();
                assert!(condition.emit(context).is_primitive());
                context.put_jump_zero(label_1.clone());
                context.enter_scope();
                let lt = true_part.emit(context);
                context.leave_scope();
                context.put_jump(label_2.clone());
                context.put_label(label_1);
                context.enter_scope();
                let rt = false_part.emit(context);
                assert!(lt == rt);
                context.leave_scope();
                context.put_label(label_2);
                lt
            }
            ExpressionKind::FunctionCall(name, arguments) => {
                let mut types = Vec::new();
                for argument in arguments.iter().rev() {
                    types.push(argument.emit(context));
                }
                context.check_arguments(name, &types);
                context.put_call(name);
                context.mark_function_called(name);
                for _ in arguments.iter() {
                    context.put_pop();
                }
                context.put_returned_value();
                context.get_function_return_type(name)
            }
            ExpressionKind::Reference(rhs) => {
                assert!(rhs.is_lvalue);
                match &rhs.kind {
                    ExpressionKind::Identifier(name) => {
                        let ty = context.access_variable(&name);
                        assert!(!ty.is_array());
                        Type::Pointer(Box::new(ty))
                    }
                    ExpressionKind::Dereference(rrhs) => rrhs.emit(context),
                    ExpressionKind::Index(base, indices) => {
                        let mut ty = base.emit(context);
                        for i in 0..indices.len() {
                            let index = &indices[i];
                            if ty.is_array() {
                                ty = ty.unwrap_array();
                            } else if ty.is_pointer() {
                                ty = ty.unwrap_pointer();
                            } else {
                                panic!();
                            }
                            assert!(index.emit(context).is_primitive());
                            context.put_push(ty.measure() as i32);
                            context.put_multiply();
                            context.put_add();
                            if !ty.is_array() && i + 1 < indices.len() {
                                context.put_load();
                            }
                        }
                        Type::Pointer(Box::new(ty))
                    }
                    ExpressionKind::Convert(target, rhs) => {
                        assert!(!target.is_array());
                        rhs.emit(context);
                        Type::Pointer(Box::new(target.clone()))
                    }
                    _ => unreachable!(),
                }
            }
            ExpressionKind::Dereference(rhs) => {
                let rt = rhs.emit(context);
                assert!(rt.is_pointer());
                context.put_load();
                rt.unwrap_pointer()
            }
            ExpressionKind::Convert(target, rhs) => {
                assert!(!target.is_array());
                rhs.emit(context);
                target.clone()
            }
            ExpressionKind::Index(base, indices) => {
                let mut ty = base.emit(context);
                for index in indices.iter() {
                    if ty.is_array() {
                        ty = ty.unwrap_array();
                    } else if ty.is_pointer() {
                        ty = ty.unwrap_pointer();
                    } else {
                        panic!();
                    }
                    assert!(index.emit(context).is_primitive());
                    context.put_push(ty.measure() as i32);
                    context.put_multiply();
                    context.put_add();
                    if !ty.is_array() {
                        context.put_load();
                    }
                }
                ty
            }
        }
    }
}
