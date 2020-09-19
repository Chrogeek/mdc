use crate::context::*;
use crate::util::*;

pub trait Ast {
    fn emit(&self, context: &mut Context);
}

#[derive(Debug)]
pub struct Program {
    pub function: Function,
}

impl Ast for Program {
    fn emit(&self, context: &mut Context) {
        if self.function.name != "main" {
            panic!("No entry point 'main' defined");
        }
        context.ir.push(Instruction::Directive(".text".to_string()));
        context
            .ir
            .push(Instruction::Directive(".globl main".to_string()));
        context.ir.push(Instruction::Directive("main:".to_string()));
        self.function.emit(context);
    }
}

#[derive(Debug)]
pub struct Function {
    pub r#type: Type,
    pub name: String,
    pub body: Vec<BlockItem>,
}

impl Ast for Function {
    fn emit(&self, context: &mut Context) {
        context.enter_function(&self.name);
        context.ir.push(Instruction::SetFrame(self.name.clone()));
        for statement in self.body.iter() {
            statement.emit(context);
        }
        if self.name == "main" {
            // default return for 'main' function
            context.ir.push(Instruction::Push(0));
            context.ir.push(Instruction::Return);
        }
        context.exit_function();
        context.ir.push(Instruction::EndFrame);
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

impl Ast for Statement {
    fn emit(&self, context: &mut Context) {
        match self {
            Statement::Empty => {}
            Statement::Return(expression) => {
                expression.emit(context);
                context.ir.push(Instruction::Return);
            }
            Statement::Expression(expression) => {
                expression.emit(context);
                context.ir.push(Instruction::Pop);
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
                    context.ir.push(Instruction::JumpOnZero(label_1.clone()));
                    true_branch.emit(context);
                    context.ir.push(Instruction::Jump(label_2.clone()));
                    context.ir.push(Instruction::Label(label_1));
                    false_part.emit(context);
                    context.ir.push(Instruction::Label(label_2));
                } else {
                    let label = context.next_label();
                    condition.emit(context);
                    context.ir.push(Instruction::JumpOnZero(label.clone()));
                    true_branch.emit(context);
                    context.ir.push(Instruction::Label(label));
                }
            }
        };
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub r#type: Type,
    pub name: String,
    pub default: Option<Expression>,
}

impl Ast for Declaration {
    fn emit(&self, context: &mut Context) {
        context.create_variable(&self.r#type, &self.name);
        if let Some(expression) = &self.default {
            expression.emit(context);
            context.ir.push(Instruction::Locate(self.name.clone()));
            context.ir.push(Instruction::Store);
            context.ir.push(Instruction::Pop);
        }
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

impl Ast for BlockItem {
    fn emit(&self, context: &mut Context) {
        match self {
            BlockItem::Statement(statement) => {
                statement.emit(context);
            }
            BlockItem::Declaration(declaration) => {
                declaration.emit(context);
            }
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

impl Ast for Expression {
    fn emit(&self, context: &mut Context) {
        macro_rules! make_binary_operator_visitor {
            ($lhs: ident, $rhs: ident, $instruction: ident) => {{
                $lhs.emit(context);
                $rhs.emit(context);
                context.ir.push(Instruction::$instruction);
            }};
        }

        match self {
            Expression::IntegerLiteral(value) => context.ir.push(Instruction::Push(*value)),
            Expression::Identifier(name) => {
                context.access_variable(name);
                context.ir.push(Instruction::Load);
            }
            Expression::Negation(rhs) => {
                rhs.emit(context);
                context.ir.push(Instruction::Negate);
            }
            Expression::Not(rhs) => {
                rhs.emit(context);
                context.ir.push(Instruction::Not);
            }
            Expression::LogicalNot(rhs) => {
                rhs.emit(context);
                context.ir.push(Instruction::LogicalNot);
            }
            Expression::Addition(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Add),
            Expression::Subtraction(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Subtract),
            Expression::Multiplication(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, Multiply)
            }
            Expression::Division(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Divide),
            Expression::Modulus(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Modulo),
            Expression::Equal(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Equal),
            Expression::Unequal(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Unequal),
            Expression::Less(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Less),
            Expression::LessEqual(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, LessEqual),
            Expression::Greater(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, Greater),
            Expression::GreaterEqual(lhs, rhs) => {
                make_binary_operator_visitor!(lhs, rhs, GreaterEqual)
            }
            Expression::LogicalAnd(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, LogicalAnd),
            Expression::LogicalOr(lhs, rhs) => make_binary_operator_visitor!(lhs, rhs, LogicalOr),
            Expression::Assignment(lhs, rhs) => {
                rhs.emit(context);
                context.access_variable(lhs);
                context.ir.push(Instruction::Store);
            }
            Expression::Ternary(condition, true_part, false_part) => {
                let label_1 = context.next_label();
                let label_2 = context.next_label();
                condition.emit(context);
                context.ir.push(Instruction::JumpOnZero(label_1.clone()));
                true_part.emit(context);
                context.ir.push(Instruction::Jump(label_2.clone()));
                context.ir.push(Instruction::Label(label_1));
                false_part.emit(context);
                context.ir.push(Instruction::Label(label_2));
            }
        }
    }
}
