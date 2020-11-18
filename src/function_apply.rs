use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };

#[derive(Debug)]
pub struct FunctionApply<'a> {
    pub name: &'a str,
    pub args: Vec<Expression<'a>>,
}

pub fn parse_function_apply(s: &str) -> IResult<&str, Expression> {
    let (s, (name, _, _, _, op, _)) = tuple((
            alpha1, space0, char('('), space0, opt(tuple((
                parse_expression, space0, many0(tuple((char(','), space0, parse_expression, space0))),
                opt(char(',')),
                space0,
                ))), char(')')))(s)?;
    let args = match op {
        Some((arg0, _, many, _, _)) => {
            let mut args = vec![arg0];
            for (_, _, arg, _) in many {
                args.push(arg);
            }
            args
        }
        None => {
            Vec::new()
        }
    };
    Ok((s, Expression::FunctionApply(FunctionApply { name, args })))
}

#[test]
fn parse_function_apply_test() {
    println!("{:?}", parse_function_apply("func()"));
    println!("{:?}", parse_function_apply("func(1, 2)"));
    println!("{:?}", parse_function_apply("func(1, 2, 3,)"));
    println!("{:?}", parse_function_apply("func(   1,2,3,    )"));
    println!("{:?}", parse_function_apply("add(func(1), func(2), func(3))"));
}
