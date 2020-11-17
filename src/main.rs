extern crate nom;

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

#[derive(Debug)]
pub enum LiteralIntegralNumber<'a> {
    U64(LiteralU64<'a>),
    I64(LiteralI64<'a>),
}

#[derive(Debug)]
pub struct LiteralU64<'a> {
    pub number: Vec<&'a str>,
}

#[derive(Debug)]
pub struct LiteralI64<'a> {
    pub number: Vec<&'a str>,
}

pub fn literal_integral_number(s: &str) -> IResult<&str, LiteralIntegralNumber> {
    let (s, x) = alt((literal_i64, literal_u64))(s)?;
    Ok((s, x))
}

pub fn literal_u64(s: &str) -> IResult<&str, LiteralIntegralNumber> {
    let (s, (number, _)) = 
         tuple((unsigned_number, opt(tag("u64"))))(s)?;
    Ok((s, 
        LiteralIntegralNumber::U64(LiteralU64 { number })
        ))
}

pub fn literal_i64(s: &str) -> IResult<&str, LiteralIntegralNumber> {
    let (s, (number, _)) = 
         tuple((unsigned_number, tag("i64")))(s)?;
    Ok((s, 
        LiteralIntegralNumber::I64(LiteralI64 { number })
        ))
}

pub fn unsigned_number(s: &str) -> IResult<&str, Vec<&str>> {
    let (s, x) = digit1(s)?;
    fold_many0(alt((tag("_"), digit1)), vec![x], |mut acc: Vec<_>, item| {
        acc.push(item);
        acc
    })(s)
}

fn main() {
    println!("{:?}", literal_integral_number("659"));
    println!("{:?}", literal_integral_number("6_5_9"));
    println!("{:?}", literal_integral_number("659i64"));
    println!("{:?}", literal_integral_number("6_5_9i64"));
}
