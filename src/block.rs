//use nom::branch::*;
use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;

use crate::statement::{ Statement, parse_statement };
use crate::expression::{ Expression, parse_expression };

#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub return_exp: Option<Expression<'a>>,
}

pub fn parse_block(s: &str) -> IResult<&str, Block> {
    let (s, (vec, _, return_exp, _)) = tuple((many0(tuple((space0, parse_statement, space0, tag(";")))), space0, opt(parse_expression), space0))(s)?;
    let mut statements = Vec::new();
    for (_, st, _, _) in vec {
        statements.push(st);
    }
    Ok((s, Block { statements, return_exp }))
}

#[test]
fn parse_block_test() {
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z);"));
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z)"));
}
