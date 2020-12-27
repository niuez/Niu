use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;

use crate::func_definition::{ FuncDefinition, parse_func_definition };
use crate::unify::*;

#[derive(Debug)]
pub struct FullContent {
    pub funcs: Vec<FuncDefinition>,
}

impl GenType for FullContent {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        for f in self.funcs.iter() {
            f.gen_type(equs)?;
        }
        Ok(Type::End)
    }
}

pub fn parse_full_content(s: &str) -> IResult<&str, FullContent> {
    let (s, (_, funcs, _)) = tuple((space0, many0(tuple((parse_func_definition, space0))), space0))(s)?;
    Ok((s, FullContent { funcs: funcs.into_iter().map(|(f, _)| f ).collect() }))
}

#[test]
fn parse_full_content_test() {
    println!("{:?}", parse_full_content("fn func(x: i64) -> i64 { let y = x * x; y + x } fn add(x: i64) -> i64 { x + x }"))
}

#[test]
fn gentype_full_test() {
    let (_, t) = parse_full_content("fn two(z: i64) -> i64 { 2i64 } fn func(x: i64) -> i64 { let y = x; two(x) }").unwrap();
    println!("{:?}", t);
    let mut equs = TypeEquations::new();
    t.gen_type(&mut equs).unwrap();
    println!("{:#?}", equs);
}
