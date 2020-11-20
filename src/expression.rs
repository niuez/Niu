//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use crate::unary_expr::{ UnaryExpr, parse_unary_expr };

#[derive(Debug)]
pub enum Expression<'a> {
    Poly(Poly<'a>),
}

#[derive(Debug)]
pub struct Poly<'a> {
    pub terms: Vec<Term<'a>>,
    pub opes: Vec<PolyOperator>,
}

#[derive(Debug)]
pub enum PolyOperator {
    Add,
    Sub,
}

pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    parse_poly(s)
}

pub fn parse_poly(s: &str) -> IResult<&str, Expression> {
    let (s, (head, _, tails)) = 
        tuple((parse_term, space0, many0(tuple((parse_poly_operator, space0, parse_term, space0)))))(s)?;
    let mut terms = vec![head];
    let mut opes = Vec::new();

    for (ope, _, term, _) in tails {
        terms.push(term);
        opes.push(ope);
    }
    Ok((s, Expression::Poly(Poly { terms, opes })))
}

pub fn parse_poly_operator(s: &str) -> IResult<&str, PolyOperator> {
    let (s, c) = one_of("+-")(s)?;
    let ope = match c {
        '+' => PolyOperator::Add,
        '-' => PolyOperator::Sub,
        _ => unreachable!()
    };
    Ok((s, ope))
}

#[derive(Debug)]
pub struct Term<'a> {
    pub unary_exprs: Vec<UnaryExpr<'a>>,
    pub opes: Vec<TermOperator>,
}

#[derive(Debug)]
pub enum TermOperator {
    Mul,
    Div,
}

pub fn parse_term(s: &str) -> IResult<&str, Term> {
    let (s, (head, _, tails)) = 
        tuple((parse_unary_expr, space0, many0(tuple((parse_term_operator, space0, parse_unary_expr, space0)))))(s)?;
    let mut unary_exprs = vec![head];
    let mut opes = Vec::new();

    for (ope, _, expr, _) in tails {
        unary_exprs.push(expr);
        opes.push(ope);
    }
    Ok((s, Term { unary_exprs, opes }))
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


#[test]
fn parse_term_test() {
    println!("{:?}", parse_term("15 * 13 / 12 * 11"));
    println!("{:?}", parse_term("5 * generate_func(31 * 91, 210)(1333 / 5 * 3) * fib(5)"));
}

#[test]
fn parse_poly_test() {
    println!("{:?}", parse_poly("1 + 2 - 3 + 4 - 5"));
    println!("{:?}", parse_poly("func(1 + 2, 3 - 4)"));
    println!("{:#?}", parse_poly("1 + 2 * 3 - 4 / 5"));
    println!("{:#?}", parse_poly("(1 + 2) * (3 - 4) / 5"));
}
