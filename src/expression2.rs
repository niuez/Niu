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

fn default_parse_expression<'a, P: ParseExpression<'a>>(s: &'a str) -> IResult<&'a str, P>
where
    P::Child: ParseExpression<'a>,
    P::Operator: ParseOperator,
{
    let (s, (head, _, tails)) = 
        tuple((P::Child::parse_expression, space0, many0(tuple((P::Operator::parse_operator, space0, P::Child::parse_expression, space0)))))(s)?;
    let mut terms = vec![head];
    let mut opes = Vec::new();

    for (ope, _, term, _) in tails {
        terms.push(term);
        opes.push(ope);
    }
    Ok((s, P::new_expr(terms, opes)))
}

trait ParseExpression<'a>: Sized {
    type Child: 'a;
    type Operator: 'a;
    fn new_expr(childs: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self;
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self>;
}

trait ParseOperator: Sized {
    fn parse_operator(s: &str) -> IResult<&str, Self>;
}

impl<'a> ParseExpression<'a> for Poly<'a> {
    type Child = Term<'a>;
    type Operator = PolyOperator;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
    }
}

impl ParseOperator for PolyOperator {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = one_of("+-")(s)?;
        let ope = match c {
            '+' => PolyOperator::Add,
            '-' => PolyOperator::Sub,
            _ => unreachable!()
        };
        Ok((s, ope))
    }
}

impl<'a> ParseExpression<'a> for Term<'a> {
    type Child = UnaryExpr<'a>;
    type Operator = TermOperator;
    fn new_expr(unary_exprs: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { unary_exprs, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        let (s, (head, _, tails)) = 
            tuple((parse_unary_expr, space0, many0(tuple((Self::Operator::parse_operator, space0, parse_unary_expr, space0)))))(s)?;
        let mut unary_exprs = vec![head];
        let mut opes = Vec::new();

        for (ope, _, expr, _) in tails {
            unary_exprs.push(expr);
            opes.push(ope);
        }
        Ok((s, Term { unary_exprs, opes }))
    }
}

impl ParseOperator for TermOperator {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = one_of("*/")(s)?;
        let ope = match c {
            '*' => TermOperator::Mul,
            '/' => TermOperator::Div,
            _ => unreachable!()
        };
        Ok((s, ope))
    }
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

pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    let (s, p) = Poly::parse_expression(s)?;
    Ok((s, Expression::Poly(p)))
}


#[test]
fn parse_expression_test() {
    println!("{:?}", parse_expression("1 + 2 - 3 + 4 - 5"));
    println!("{:?}", parse_expression("func(1 + 2, 3 - 4)"));
    println!("{:#?}", parse_expression("1 + 2 * 3 - 4 / 5"));
    println!("{:#?}", parse_expression("(1 + 2) * (3 - 4) / 5"));
}
