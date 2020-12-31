use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;

use crate::literal::{ Literal, parse_literal };
use crate::identifier::{ Identifier, parse_identifier };
use crate::expression::{ Expression, parse_expression };
use crate::subseq::{ Subseq, parse_subseq, subseq_gen_type };
use crate::unify::*;

#[derive(Debug)]
pub enum UnaryExpr {
    Variable(Variable),
    Literal(Literal),
    Parentheses(Parentheses),
    Subseq(Box<UnaryExpr>, Subseq),
}

impl GenType for UnaryExpr {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        match *self {
            UnaryExpr::Variable(ref v) => v.gen_type(equs),
            UnaryExpr::Literal(ref l) => l.gen_type(equs),
            UnaryExpr::Parentheses(ref p) => p.gen_type(equs),
            UnaryExpr::Subseq(ref expr, ref s) => subseq_gen_type(expr.as_ref(), s, equs)
        }
    }
}

pub fn parse_unary_expr(s: &str) -> IResult<&str, UnaryExpr> {
    let (s, x) = alt((
            parse_variable,
            parse_literal,
            parse_parentheses,
            ))(s)?;
    let mut now = s;
    let mut prec = x;
    while let Ok((s, sub)) = parse_subseq(now) {
        now = s;
        prec = UnaryExpr::Subseq(Box::new(prec), sub);
    }
    Ok((now, prec))
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Variable {
    pub name: Identifier,
}

impl Variable {
    pub fn from_identifier(id: Identifier) -> Self {
        Variable { name: id }
    }
}

impl GenType for Variable {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        equs.get_type_from_variable(self)
    }
}

pub fn parse_variable(s: &str) -> IResult<&str, UnaryExpr> {
    let(s, name) = parse_identifier(s)?;
    Ok((s, UnaryExpr::Variable(Variable { name })))
}

#[derive(Debug)]
pub struct Parentheses {
    pub expr: Expression,
}

impl GenType for Parentheses {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        self.expr.gen_type(equs)
    }
}

pub fn parse_parentheses(s: &str) -> IResult<&str, UnaryExpr> {
    let(s, (_, _, expr, _, _)) = tuple((char('('), space0, parse_expression, space0, char(')')))(s)?;
    Ok((s, UnaryExpr::Parentheses(Parentheses { expr })))
}

#[test]
fn parse_unary_expr_test() {
    println!("{:?}", parse_unary_expr("func(1, 2, 3)"));
    println!("{:?}", parse_unary_expr("add(1, add(2, 3), 4)"));
    println!("{:?}", parse_unary_expr("generate_func(91)(1333)"));
    println!("{:?}", parse_unary_expr("generate_func(31 * 91, 210)(1333 / 5 * 3)"));
}
#[test]
fn parse_parentheses_expr_test() {
    println!("{:?}", parse_unary_expr("(1 + 2 + 3)"));
}
