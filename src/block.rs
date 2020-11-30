//use nom::branch::*;
use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 

use crate::statement::{ Statement, parse_statement };

#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

pub fn parse_block(s: &str) -> IResult<&str, Block> {
    let (s, (vec, _)) = tuple((many0(tuple((space0, parse_statement, space0, tag(";")))), space0))(s)?;
    let mut statements = Vec::new();
    for (_, st, _, _) in vec {
        statements.push(st);
    }
    Ok((s, Block { statements }))
}

#[test]
fn parse_block_test() {
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z);"));
}
