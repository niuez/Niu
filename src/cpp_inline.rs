use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::sequence::*;
use nom::branch::*;
use nom::IResult;

use crate::type_id::*;
use crate::identifier::{ Identifier, parse_identifier, Tag };
use crate::unify::*;
use crate::trans::*;
use crate::type_spec::*;

#[derive(Debug, Clone)]
pub struct CppInline {
    inlines: Vec<CppInlineElem>,
}

#[derive(Debug, Clone)]
pub enum CppInlineElem {
    Type(TypeId),
    Arg(Identifier),
    End,
    Any(char),
}

impl CppInline {
    pub fn generate_cpp_inline_info(&self, equs: &mut TypeEquations, trs: &TraitsInfo, gen_mp: &GenericsTypeMap) -> Result<CppInlineInfo, String> {
        let tag = Tag::new();
        let mut cnt = 0;
        let elems = self.inlines.iter().map(|inline| match inline {
            CppInlineElem::Type(tyid) => {
                let ty = TypeSpec::from_id(tyid).generics_to_type(gen_mp, equs, trs)?;
                let alpha = tag.generate_type_variable("CppInlineInfoType", cnt, equs);
                equs.add_equation(alpha, ty);
                cnt += 1;
                Ok(CppInlineInfoElem::Type(cnt - 1))
            }
            CppInlineElem::Arg(id) => Ok(CppInlineInfoElem::Arg(id.clone())),
            CppInlineElem::Any(c) => Ok(CppInlineInfoElem::Any(*c)),
            _ => unreachable!("End???"),
        }).collect::<Result<Vec<_>, String>>()?;
        Ok(CppInlineInfo { elems, tag })
    }

    pub fn transpile_implement(&self, ta: &TypeAnnotation) -> String {
        self.inlines.iter().map(|inline| match inline {
            CppInlineElem::Arg(id) => {
                id.into_string()
            }
            CppInlineElem::Type(tyid) => {
                tyid.transpile(ta)
            }
            CppInlineElem::Any(c) => {
                c.to_string()
            }
            _ => unreachable!("cant use inline {:?}", inline),
        }).collect::<Vec<_>>().join("")
    }

    pub fn transpile(&self, _ta: &TypeAnnotation, gen_mp: &HashMap<TypeId, String>) -> String {
        self.inlines.iter().map(|inline| match inline {
            CppInlineElem::Type(tyid) => {
                gen_mp.get(tyid).unwrap().to_string()
            }
            CppInlineElem::Any(c) => {
                c.to_string()
            }
            _ => unreachable!("cant use inline {:?}", inline),
        }).collect::<Vec<_>>().join("")
    }
}

fn parse_inline_elem_type(s: &str) -> IResult<&str, CppInlineElem> {
    let (s, (_, _, id, _, _)) = tuple((tag("$ty("), space0, parse_type_id, space0, tag(")")))(s)?;
    Ok((s, CppInlineElem::Type(id)))
}

fn parse_inline_elem_arg(s: &str) -> IResult<&str, CppInlineElem> {
    let (s, (_, _, id, _, _)) = tuple((tag("$arg("), space0, parse_identifier, space0, tag(")")))(s)?;
    Ok((s, CppInlineElem::Arg(id)))
}

fn parse_inline_end(s: &str) -> IResult<&str, CppInlineElem> {
    let (s, _) = tag("}$$")(s)?;
    Ok((s, CppInlineElem::End))
}

fn parse_inline_any(s: &str) -> IResult<&str, CppInlineElem> {
    let (s, c) = anychar(s)?;
    Ok((s, CppInlineElem::Any(c)))
}

pub fn parse_cpp_inline(s: &str) -> IResult<&str, CppInline> {
    let (mut s, _) = tag("$${")(s)?;
    let mut inlines = Vec::new();
    loop {
        let (ss, inline) = alt((parse_inline_elem_type, parse_inline_elem_arg, parse_inline_end, parse_inline_any))(s)?;
        s = ss;
        match inline {
            CppInlineElem::End => {
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
    println!("{:?}", parse_cpp_inline("$${ $arg(self).push_back($arg(t)) }$$"));
}
