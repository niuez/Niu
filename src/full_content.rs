use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;

use crate::func_definition::{ FuncDefinition, parse_func_definition };

#[derive(Debug)]
pub struct FullContent {
    pub funcs: Vec<FuncDefinition>,
}

pub fn parse_full_content(s: &str) -> IResult<&str, FullContent> {
    let (s, (_, funcs, _)) = tuple((space0, many0(tuple((parse_func_definition, space0))), space0))(s)?;
    Ok((s, FullContent { funcs: funcs.into_iter().map(|(f, _)| f ).collect() }))
}

#[test]
fn parse_full_content_test() {
    println!("{:?}", parse_full_content("fn func(x: i64) -> i64 { let y = x * x; y + x } fn add(x: i64) -> i64 { x + x }"))
}
