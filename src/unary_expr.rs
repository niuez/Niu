use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;

use crate::literal::{ Literal, parse_literal };
use crate::identifier::{ Identifier, parse_identifier };
use crate::expression::{ Expression, parse_expression };
use crate::subseq::{ Subseq, parse_subseq };

#[derive(Debug)]
pub enum UnaryExpr<'a> {
    Variable(Variable<'a>),
    Literal(Literal<'a>),
    Parentheses(Parentheses<'a>),
    Subseq(Box<UnaryExpr<'a>>, Subseq<'a>),
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

#[derive(Debug)]
pub struct Variable<'a> {
    pub name: Identifier<'a>,
}

pub fn parse_variable(s: &str) -> IResult<&str, UnaryExpr> {
    let(s, name) = parse_identifier(s)?;
    Ok((s, UnaryExpr::Variable(Variable { name })))
}

#[derive(Debug)]
pub struct Parentheses<'a> {
    pub expr: Expression<'a>,
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
