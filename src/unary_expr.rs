use nom::branch::*;
use nom::IResult;

use crate::literal::{ LiteralIntegralNumber, parse_literal };
use crate::identifier::{ Identifier, parse_identifier };
use crate::expression::Expression;
use crate::subseq::{ Subseq, parse_subseq };

#[derive(Debug)]
pub enum UnaryExpr<'a> {
    Variable(Variable<'a>),
    Literal(LiteralIntegralNumber<'a>),
    Subseq(Box<UnaryExpr<'a>>, Subseq<'a>),
}

pub fn parse_unary_expr(s: &str) -> IResult<&str, Expression> {
    let (s, x) = alt((
            parse_variable,
            parse_literal,
            ))(s)?;
    let mut now = s;
    let mut prec = x;
    while let Ok((s, sub)) = parse_subseq(now) {
        now = s;
        prec = UnaryExpr::Subseq(Box::new(prec), sub);
    }
    Ok((now, Expression::UnaryExpr(prec)))
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub name: Identifier<'a>,
}

pub fn parse_variable(s: &str) -> IResult<&str, UnaryExpr> {
    let(s, name) = parse_identifier(s)?;
    Ok((s, UnaryExpr::Variable(Variable { name })))
}

#[test]
fn parse_unary_expr_test() {
    println!("{:?}", parse_unary_expr("func(1, 2, 3)"));
    println!("{:?}", parse_unary_expr("add(1, add(2, 3), 4)"));
    println!("{:?}", parse_unary_expr("generate_func(91)(1333)"));
}
