use nom::branch::*;
use nom::IResult;

use crate::literal::{ LiteralIntegralNumber, parse_literal };
use crate::function_apply::{ FunctionApply, parse_function_apply };

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(LiteralIntegralNumber<'a>),
    FunctionApply(FunctionApply<'a>)
}

pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    let (s, x) = alt((parse_literal, parse_function_apply))(s)?;
    Ok((s, x))
}
