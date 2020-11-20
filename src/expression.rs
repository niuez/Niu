//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;

use crate::unary_expr::{ UnaryExpr, parse_unary_expr };

#[derive(Debug)]
pub enum Expression<'a> {
    Term(Term<'a>),
}

pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    parse_term(s)
}

#[derive(Debug)]
pub struct Term<'a> {
    pub unary_exprs: Vec<UnaryExpr<'a>>,
    pub opes: Vec<TermOperator>,
}

pub fn parse_term(s: &str) -> IResult<&str, Expression> {
    let (s, (head, _, tails)) = 
        tuple((parse_unary_expr, space0, many0(tuple((parse_term_operator, space0, parse_unary_expr, space0)))))(s)?;
    let mut unary_exprs = vec![head];
    let mut opes = Vec::new();

    for (ope, _, expr, _) in tails {
        unary_exprs.push(expr);
        opes.push(ope);
    }
    Ok((s, Expression::Term(Term { unary_exprs, opes })))
}

pub fn parse_term_operator(s: &str) -> IResult<&str, TermOperator> {
    let (s, c) = one_of("*/")(s)?;
    let ope = match c {
        '*' => TermOperator::Mul,
        '/' => TermOperator::Div,
        _ => unreachable!()
    };
    Ok((s, ope))
}

#[derive(Debug)]

pub enum TermOperator {
    Mul,
    Div,
}

#[test]
fn parse_term_test() {
    println!("{:?}", parse_term("15 * 13 / 12 * 11"));
    println!("{:?}", parse_term("5 * generate_func(31 * 91, 210)(1333 / 5 * 3) * fib(5)"));
}
