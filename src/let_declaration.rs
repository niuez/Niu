//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::bytes::complete::*;
use nom::combinator::*;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_id::{ TypeId, parse_type_id };
use crate::expression::{ Expression, parse_expression };

#[derive(Debug)]
pub struct LetDeclaration<'a> {
    pub id: Identifier<'a>,
    pub type_info: Option<TypeId<'a>>,
    pub value: Expression<'a>,
}

pub fn parse_let_declaration(s: &str) -> IResult<&str, LetDeclaration> {
    let (s, (_let, _, id, _, tyinfo, _, _e, _, value)) = tuple((tag("let"), space1, parse_identifier, space0, opt(tuple((char(':'), space0, parse_type_id))), space0, tag("="), space0, parse_expression))(s)?;
    Ok((s, (LetDeclaration { id, type_info: tyinfo.map(|(_, _, type_info)| type_info ), value })))
}

#[test]
fn parse_decl_test() {
    println!("{:?}", parse_let_declaration("let x = 1 + 2"));
    println!("{:?}", parse_let_declaration("let x: i64 = 1 + 2"));
}
