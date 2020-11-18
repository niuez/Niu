use nom::branch::*;
use nom::IResult;

use crate::literal::{ LiteralIntegralNumber, parse_literal };
use crate::function_apply::{ FunctionApply, parse_function_apply };
use crate::variable::{ Variable, parse_variable };

#[derive(Debug)]
pub enum Expression<'a> {
    FunctionApply(FunctionApply<'a>),
    Variable(Variable<'a>),
    Literal(LiteralIntegralNumber<'a>),
}

pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    let (s, x) = alt((
            parse_function_apply,
            parse_variable,
            parse_literal,
            ))(s)?;
    Ok((s, x))
}
