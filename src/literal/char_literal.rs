use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;


use nom::sequence::*;
use nom::IResult;




use crate::trans::*;

use super::*;

#[derive(Debug)]
pub struct Char {
    pub ch: String,
}

impl Transpile for Char {
    fn transpile(&self, _ta: &TypeAnnotation) -> String {
        format!("'{}'", self.ch.clone())
    }
}

pub fn parse_char(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Literal> {
    let (s, (_, c, _)) = tuple((char('\''), alt((parse_normal_char, parse_escape_7bit_char, parse_escape_sequence)), char('\'')))(s)?;
    Ok((s, Literal::Char(c)))
}

fn parse_normal_char(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Char> {
    let (s, c) = satisfy(|c| c != '\\')(s)?;
    Ok((s, Char { ch: c.to_string() }))
}

fn parse_escape_7bit_char(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Char> {
    let (s, (_, b1, b2)) = tuple((tag("\\x"), one_of("01234567"), one_of("0123456789ABCDEFabcdef")))(s)?;
    Ok((s, Char { ch: vec!['\\', 'x', b1, b2].into_iter().collect() }))
}

fn parse_escape_sequence(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Char> {
    let (s, (_, b1)) = tuple((tag("\\"), one_of("nrt\\\'\"0")))(s)?;
    Ok((s, Char { ch: vec!['\\', b1].into_iter().collect() }))
}
