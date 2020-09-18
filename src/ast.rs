use crate::ir::Context;
use crate::ir::Instruction;
use crate::util::Type;

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
        self.function.emit(context);
    }
}

#[derive(Debug)]
pub struct Function {
    pub r#type: Type,
    pub name: String,
    pub body: Vec<Statement>,
}

impl Ast for Function {
    fn emit(&self, context: &mut Context) {
        context.ir.push(Instruction::Directive(format!(
            ".globl {0}\n{0}:",
            self.name
        )));
        for statement in self.body.iter() {
            statement.emit(context);
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Empty,
    Return(Expression),
    Expression(Expression),
    Declaration {
        // To be deleted on step 7
        r#type: Type,
        name: String,
        default: Option<Expression>,
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
            Statement::Declaration {
                r#type,
                name,
                default,
            } => {
                let offset = context.create_variable(r#type, name).offset;
                if let Some(expression) = default {
                    context.ir.push(Instruction::FrameAddress(offset));
                    expression.emit(context);
                    context.ir.push(Instruction::Store);
                    context.ir.push(Instruction::Pop);
                }
            }
        };
    }
}

// To be available on step 7
// #[derive(Debug)]
// pub struct Declaration {
//     r#type: Type,
//     name: String,
//     default: Option<Expression>,
// }

// impl Ast for Declaration {
//     fn emit(&self, context: &mut Context) {
//         unimplemented!()
//     }
// }

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
                let offset = context.access_variable(name).offset;
                context.ir.push(Instruction::FrameAddress(offset));
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
                let offset = context.access_variable(lhs).offset;
                context.ir.push(Instruction::FrameAddress(offset));
                context.ir.push(Instruction::Store);
            }
        }
    }
}
