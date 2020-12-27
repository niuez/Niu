use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;


use crate::unary_expr::UnaryExpr;
use crate::identifier::Identifier;
use crate::unify::*;

#[derive(Debug)]
pub enum Literal {
    U64(LiteralU64),
    I64(LiteralI64),
    Boolean(Boolean),
}

impl GenType for Literal {
    fn gen_type(&self, _: &mut TypeEquations) -> TResult {
        match *self {
            Literal::U64(_) => Ok(Type::Type(Identifier::from_str("u64"))),
            Literal::I64(_) => Ok(Type::Type(Identifier::from_str("i64"))),
            Literal::Boolean(_) => Ok(Type::Type(Identifier::from_str("bool"))),
        }
    }
}

pub fn parse_literal(s: &str) -> IResult<&str, UnaryExpr> {
    let (s, x) = alt((literal_i64, literal_u64, literal_boolean))(s)?;
    Ok((s, UnaryExpr::Literal(x)))
}

#[derive(Debug)]
pub struct LiteralU64 {
    pub number: String,
}

#[derive(Debug)]
pub struct LiteralI64 {
    pub number: String,
}

#[derive(Debug)]
pub enum Boolean {
    True,
    False,
}

pub fn literal_u64(s: &str) -> IResult<&str, Literal> {
    let (s, (number, _)) = 
         tuple((unsigned_number, opt(tag("u64"))))(s)?;
    Ok((s, 
        Literal::U64(LiteralU64 { number: number.join("") })
        ))
}

pub fn literal_i64(s: &str) -> IResult<&str, Literal> {
    let (s, (number, _)) = 
         tuple((unsigned_number, tag("i64")))(s)?;
    Ok((s, 
        Literal::I64(LiteralI64 { number: number.join("") })
        ))
}

pub fn unsigned_number(s: &str) -> IResult<&str, Vec<&str>> {
    let (s, x) = digit1(s)?;
    fold_many0(alt((tag("_"), digit1)), vec![x], |mut acc: Vec<_>, item| {
        acc.push(item);
        acc
    })(s)
}

pub fn literal_boolean(s: &str) -> IResult<&str, Literal> {
    let (s, x) = alt((tag("true"), tag("false")))(s)?;
    match x {
        "true" => Ok((s, Literal::Boolean(Boolean::True))),
        "false" => Ok((s, Literal::Boolean(Boolean::False))),
        _ => unreachable!(),
    }
}
    

#[test]
fn parse_literal_u64_test() {
    println!("{:?}", parse_literal("659"));
    println!("{:?}", parse_literal("6_5_9"));
}

#[test]
fn parse_literal_i64_test() {
    println!("{:?}", parse_literal("659i64"));
    println!("{:?}", parse_literal("6_5_9i64"));
}

#[test]
fn parse_literal_boolean_test() {
    println!("{:?}", parse_literal("true"));
    println!("{:?}", parse_literal("false"));
}
