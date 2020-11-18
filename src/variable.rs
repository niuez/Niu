use nom::IResult;

use crate::expression::Expression;
use crate::identifier::{ Identifier, parse_identifier };

#[derive(Debug)]
pub struct Variable<'a> {
    pub name: Identifier<'a>,
}

pub fn parse_variable(s: &str) -> IResult<&str, Expression> {
    let(s, name) = parse_identifier(s)?;
    Ok((s, Expression::Variable(Variable { name })))
}
