use crate::context::*;
use crate::util::*;
use std::io::Write;

pub trait Ast<T: Write, U: Write, V: Write> {
    fn emit(&self, context: &mut Context<T, U, V>);
}

#[derive(Debug)]
pub enum ProgramItem {
    Function(Function),
    Declaration(Declaration),
}

#[derive(Debug)]
pub struct Program {
    pub items: Vec<ProgramItem>,
}

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for Program {
    fn emit(&self, context: &mut Context<T, U, V>) {
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
                    context.create_global_variable(&declaration.name, &declaration.r#type);
                    let mangled = mangle_global_variable(&declaration.name);
                    match &declaration.default {
                        None => {
                            context.write_bss(&format!(".comm {}, 4, 4", mangled));
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

#[derive(Debug)]
pub struct Function {
    pub r#type: Type,
    pub name: String,
    pub parameters: Vec<(String, Type)>,
    pub body: Option<Vec<BlockItem>>,
}

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for Function {
    fn emit(&self, context: &mut Context<T, U, V>) {
        self.body
            .as_ref()
            .and_then(|body| {
                context.define_function(
                    &self.name,
                    self.r#type.clone(),
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
                    eprintln!("{}, {}", parameter_name, offset + 8);
                    offset += parameter_type.measure();
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
                    self.r#type.clone(),
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

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for Statement {
    fn emit(&self, context: &mut Context<T, U, V>) {
        match self {
            Statement::Empty => {}
            Statement::Return(expression) => {
                expression.emit(context);
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
                if let Some(false_part) = false_branch {
                    let label_1 = context.next_label();
                    let label_2 = context.next_label();
                    condition.emit(context);
                    context.put_jump_zero(label_1.clone());
                    context.enter_scope();
                    true_branch.emit(context);
                    context.leave_scope();
                    context.put_jump(label_2.clone());
                    context.put_label(label_1);
                    context.enter_scope();
                    false_part.emit(context);
                    context.leave_scope();
                    context.put_label(label_2);
                } else {
                    let label = context.next_label();
                    condition.emit(context);
                    context.put_jump_zero(label.clone());
                    true_branch.emit(context);
                    context.put_label(label);
                }
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
                    expression.emit(context);
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

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for Compound {
    fn emit(&self, context: &mut Context<T, U, V>) {
        context.enter_scope();
        for item in self.items.iter() {
            item.emit(context);
        }
        context.leave_scope();
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub r#type: Type,
    pub name: String,
    pub default: Option<Expression>,
}

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for Declaration {
    fn emit(&self, context: &mut Context<T, U, V>) {
        let address = context.create_variable(&self.name, &self.r#type);
        if let Some(expression) = &self.default {
            expression.emit(context);
            context.put_locate(address);
            context.put_store();
            context.put_pop();
        }
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for BlockItem {
    fn emit(&self, context: &mut Context<T, U, V>) {
        match self {
            BlockItem::Statement(statement) => statement.emit(context),
            BlockItem::Declaration(declaration) => declaration.emit(context),
        }
    }
}

#[derive(Debug)]
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
    Assignment(String, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Expression>),
}

impl<T: Write, U: Write, V: Write> Ast<T, U, V> for Expression {
    fn emit(&self, context: &mut Context<T, U, V>) {
        macro_rules! make_binary_operator_visitor {
            ($lhs: ident, $rhs: ident, $instruction: ident) => {{
                $lhs.emit(context);
                $rhs.emit(context);
                context.$instruction();
            }};
        }

        match self {
            Expression::IntegerLiteral(value) => context.put_push(*value),
            Expression::Identifier(name) => {
                context.access_variable(name);
                context.put_load();
            }
            Expression::Negation(rhs) => {
                rhs.emit(context);
                context.put_negate();
            }
            Expression::Not(rhs) => {
                rhs.emit(context);
                context.put_not();
            }
            Expression::LogicalNot(rhs) => {
                rhs.emit(context);
                context.put_logical_not();
            }
            Expression::Addition(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_add),
            Expression::Subtraction(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, put_subtract)
            }
            Expression::Multiplication(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, put_multiply)
            }
            Expression::Division(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_divide),
            Expression::Modulus(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_modulo),
            Expression::Equal(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_equal),
            Expression::Unequal(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_unequal),
            Expression::Less(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_less),
            Expression::LessEqual(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, put_less_equal)
            }
            Expression::Greater(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, put_greater),
            Expression::GreaterEqual(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, put_greater_equal)
            }
            Expression::LogicalAnd(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, put_logical_and)
            }
            Expression::LogicalOr(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, put_logical_or)
            }
            Expression::Assignment(lhs, rhs) => {
                rhs.emit(context);
                context.access_variable(lhs);
                context.put_store();
            }
            Expression::Ternary(condition, true_part, false_part) => {
                let label_1 = context.next_label();
                let label_2 = context.next_label();
                condition.emit(context);
                context.put_jump_zero(label_1.clone());
                context.enter_scope();
                true_part.emit(context);
                context.leave_scope();
                context.put_jump(label_2.clone());
                context.put_label(label_1);
                context.enter_scope();
                false_part.emit(context);
                context.leave_scope();
                context.put_label(label_2);
            }
            Expression::FunctionCall(name, arguments) => {
                context.check_arguments(name, arguments);
                for argument in arguments.iter().rev() {
                    argument.emit(context);
                }
                context.put_call(name);
                context.mark_function_called(name);
                for _ in arguments.iter() {
                    context.put_pop();
                }
                context.put_returned_value();
            }
        }
    }
}
