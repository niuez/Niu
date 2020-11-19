//use nom::branch::*;
use nom::IResult;

use crate::unary_expr::{ UnaryExpr, parse_unary_expr };

#[derive(Debug)]
pub enum Expression<'a> {
    UnaryExpr(UnaryExpr<'a>),
}

pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    let (s, x) = parse_unary_expr(s)?;
    Ok((s, x))
}
