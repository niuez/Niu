use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::sequence::*;
use nom::branch::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::expression::*;
use crate::unify::*;
use crate::unary_expr::Variable;
use crate::trans::*;
use crate::type_spec::*;
use crate::traits::*;

#[derive(Debug, Clone)]
pub enum CppInlineElem {
    Type(TypeSpec),
    Arg(Identifier),
}

#[derive(Debug, Clone)]
pub struct CppInline {
    inlines: Vec<ParseInline>,
}

#[derive(Debug, Clone)]
enum ParseInline {
    Elem(CppInlineElem),
    End,
    Any(char),
}

fn parse_inline_elem_type(s: &str) -> IResult<&str, ParseInline> {
    let (s, (_, _, spec, _, _)) = tuple((tag("$ty("), space0, parse_type_spec, space0, tag(")")))(s)?;
    Ok((s, ParseInline::Elem(CppInlineElem::Type(spec))))
}

fn parse_inline_elem_arg(s: &str) -> IResult<&str, ParseInline> {
    let (s, (_, _, id, _, _)) = tuple((tag("$arg("), space0, parse_identifier, space0, tag(")")))(s)?;
    Ok((s, ParseInline::Elem(CppInlineElem::Arg(id))))
}

fn parse_inline_end(s: &str) -> IResult<&str, ParseInline> {
    let (s, _) = tag("}$$")(s)?;
    Ok((s, ParseInline::End))
}

fn parse_inline_any(s: &str) -> IResult<&str, ParseInline> {
    let (s, c) = anychar(s)?;
    Ok((s, ParseInline::Any(c)))
}

pub fn parse_cpp_inline(s: &str) -> IResult<&str, CppInline> {
    let (mut s, _) = tag("$${")(s)?;
    let mut inlines = Vec::new();
    loop {
        let (ss, inline) = alt((parse_inline_elem_type, parse_inline_elem_arg, parse_inline_end, parse_inline_any))(s)?;
        s = ss;
        match inline {
            ParseInline::End => {
                break
            }
            inline => {
                inlines.push(inline);
            }
        }
    }
    Ok((s, CppInline { inlines } ))
}

#[test]
fn parse_inline_test() {
    println!("{:?}", parse_cpp_inline("$${$ty(Vec<T>)($arg(vec)).push_back($arg(elem))}$$"));
}
