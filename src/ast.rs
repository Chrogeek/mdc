use crate::context::*;
use crate::util::*;
use std::io::Write;

pub trait Ast<T: Write> {
    fn emit(&self, context: &mut Context<T>);
}

#[derive(Debug)]
pub struct Program {
    pub function: Function,
}

impl<T: Write> Ast<T> for Program {
    fn emit(&self, context: &mut Context<T>) {
        if self.function.name != "main" {
            panic!("No entry point 'main' defined");
        }
        context.put_directive(".text");
        context.put_directive(".globl main");
        context.put_directive("main:");
        self.function.emit(context);
    }
}

#[derive(Debug)]
pub struct Function {
    pub r#type: Type,
    pub name: String,
    pub body: Vec<BlockItem>,
}

impl<T: Write> Ast<T> for Function {
    fn emit(&self, context: &mut Context<T>) {
        context.enter_function(&self.name);
        context.put_set_frame(self.name.clone());
        context.enter_scope();

        for asm in self.body.iter() {
            asm.emit(context);
        }

        if self.name == "main" {
            // default return for 'main' function
            context.put_push(0);
            context.put_return();
        }
        context.leave_scope();
        context.exit_function();
        context.put_end_frame();
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
}

impl<T: Write> Ast<T> for Statement {
    fn emit(&self, context: &mut Context<T>) {
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
        }
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub r#type: Type,
    pub name: String,
    pub default: Option<Expression>,
}

impl<T: Write> Ast<T> for Declaration {
    fn emit(&self, context: &mut Context<T>) {
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

impl<T: Write> Ast<T> for BlockItem {
    fn emit(&self, context: &mut Context<T>) {
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
}

impl<T: Write> Ast<T> for Expression {
    fn emit(&self, context: &mut Context<T>) {
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
        }
    }
}
