use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };

#[derive(Debug)]
pub enum Subseq<'a> {
    Call(Call<'a>),
}

#[derive(Debug)]
pub struct Call<'a> {
    pub args: Vec<Expression<'a>>,
}

pub fn parse_subseq(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, x)) = tuple((space0, parse_call))(s)?;
    Ok((s, x))
}

pub fn parse_call(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, _, op, _)) = tuple((
        char('('), space0, opt(tuple((
                    parse_expression, space0,
                    many0(tuple((char(','), space0, parse_expression, space0))), opt(char(',')), space0))),
                    char(')')))(s)?;
    let args = match op {
        Some((arg0, _, many, _, _)) => {
            let mut args = vec![arg0];
            for (_, _, arg, _) in many {
                args.push(arg);
            }
            args
        }
        None => Vec::new(),
    };
    Ok((s, (Subseq::Call(Call{ args }))))
}

#[test]
fn parse_call_test() {
    println!("{:?}", parse_call("()"));
    println!("{:?}", parse_call("(1, 2, 3)"));
}
