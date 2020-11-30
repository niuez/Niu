//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::bytes::complete::*;

use crate::identifier::{ Identifier, parse_identifier };
use crate::expression::{ Expression, parse_expression };

#[derive(Debug)]
pub struct LetDeclaration<'a> {
    pub id: Identifier<'a>,
    pub value: Expression<'a>,
}

pub fn parse_let_declaration(s: &str) -> IResult<&str, LetDeclaration> {
    let (s, (_let, _, id, _, _e, _, value)) = tuple((tag("let"), space1, parse_identifier, space0, tag("="), space0, parse_expression))(s)?;
    Ok((s, (LetDeclaration { id, value })))
}

#[test]
fn parse_decl_test() {
    println!("{:?}", parse_let_declaration("let x = 1 + 2"));
}
