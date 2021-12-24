pub mod char_literal;
pub use char_literal::*;

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;


use crate::unary_expr::UnaryExpr;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::content_str::*;

#[derive(Debug)]
pub enum Literal {
    U64(LiteralU64),
    I64(LiteralI64),
    F64(LiteralF64),
    Char(Char),
    Boolean(Boolean),
}

impl GenType for Literal {
    fn gen_type(&self, _: &mut TypeEquations, _: &TraitsInfo) -> TResult {
        match *self {
            Literal::U64(_) => Ok(Type::from_str("u64")),
            Literal::I64(_) => Ok(Type::from_str("i64")),
            Literal::F64(_) => Ok(Type::from_str("f64")),
            Literal::Char(_) => Ok(Type::from_str("char")),
            Literal::Boolean(_) => Ok(Type::from_str("bool")),
        }
    }
}
impl Transpile for Literal {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Literal::U64(ref u) => u.transpile(ta),
            Literal::I64(ref i) => i.transpile(ta),
            Literal::F64(ref f) => f.transpile(ta),
            Literal::Char(ref c) => c.transpile(ta),
            Literal::Boolean(ref b) => b.transpile(ta),
        }
    }
}

impl MutCheck for Literal {
    fn mut_check(&self, _ta: &TypeAnnotation, _vars: &mut VariablesInfo) -> Result<MutResult, String> {
        Ok(MutResult::NotMut)
    }
}

impl MoveCheck for Literal {
    fn move_check(&self, _mc: &mut VariablesMoveChecker, _ta: &TypeAnnotation) -> Result<MoveResult, String> {
        Ok(MoveResult::Right)
    }
}

pub fn parse_literal(s: ContentStr<'_>) -> IResult<ContentStr<'_>, UnaryExpr> {
    let (s, x) = alt((parse_char, literal_f64, literal_i64, literal_u64, literal_boolean))(s)?;
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
pub struct LiteralF64 {
    pub number: String,
}

#[derive(Debug)]
pub enum Boolean {
    True,
    False,
}

impl Transpile for LiteralU64 {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        format!("{}ull", self.number)
    }
}

impl Transpile for LiteralI64 {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        format!("{}ll", self.number)
    }
}

impl Transpile for LiteralF64 {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        format!("{}", self.number)
    }
}

impl Transpile for Boolean {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        match *self {
            Boolean::True => "true",
            Boolean::False => "false",
        }.to_string()
    }
}

pub fn literal_u64(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Literal> {
    let (s, (number, _)) = 
         tuple((unsigned_number, opt(tag("u64"))))(s)?;
    Ok((s, 
        Literal::U64(LiteralU64 { number: number.into_iter().map(|s| s.s).collect::<Vec<_>>().join("") })
        ))
}

pub fn literal_i64(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Literal> {
    let (s, (number, _)) = 
         tuple((unsigned_number, tag("i64")))(s)?;
    Ok((s, 
        Literal::I64(LiteralI64 { number: number.into_iter().map(|s| s.s).collect::<Vec<_>>().join("") })
    ))
}

pub fn unsigned_number(s: ContentStr) -> IResult<ContentStr, Vec<ContentStr>> {
    separated_list1(tag("_"), digit1)(s)
}

pub fn literal_boolean(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Literal> {
    let (s, x) = alt((tag("true"), tag("false")))(s)?;
    match x.s {
        "true" => Ok((s, Literal::Boolean(Boolean::True))),
        "false" => Ok((s, Literal::Boolean(Boolean::False))),
        _ => unreachable!(),
    }
}

pub fn literal_f64(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Literal> {
    let (s, number) = nom::number::complete::recognize_float(s)?;
    match number.s.find('.') {
        Some(_) => Ok((s, Literal::F64(LiteralF64 { number: number.s.to_string() }))),
        None => Err(nom::Err::Error(nom::error::Error { input: s, code: nom::error::ErrorKind::Float }))
    }
}
    

#[test]
fn parse_literal_u64_test() {
    log::debug!("{:?}", parse_literal(ContentStr { s: "659", name: 0 }).ok());
    log::debug!("{:?}", parse_literal(ContentStr { s: "6_5_9", name: 0 }).ok());
}

#[test]
fn parse_literal_i64_test() {
    log::debug!("{:?}", parse_literal(ContentStr { s: "659i64", name: 0 }).ok());
    log::debug!("{:?}", parse_literal(ContentStr { s: "6_5_9i64", name: 0 }).ok());
}

#[test]
fn parse_literal_boolean_test() {
    log::debug!("{:?}", parse_literal(ContentStr { s: "true", name: 0 }).ok());
    log::debug!("{:?}", parse_literal(ContentStr { s: "false", name: 0 }).ok());
}

#[test]
fn parse_literal_f64_test() {
    log::debug!("{:?}", parse_literal(ContentStr { s: "12.3", name: 0 }).ok());
    log::debug!("{:?}", parse_literal(ContentStr { s: "91.", name: 0 }).ok());
    log::debug!("{:?}", parse_literal(ContentStr { s: "123.e-3", name: 0 }).ok());
    log::debug!("{:?}", parse_literal(ContentStr { s: "123.", name: 0 }).ok());
}
