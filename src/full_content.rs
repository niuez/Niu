use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;


use crate::func_definition::{ FuncDefinition, parse_func_definition };
use crate::unify::*;
use crate::trans::*;

#[derive(Debug)]
pub struct FullContent {
    pub funcs: Vec<FuncDefinition>,
}

impl FullContent {
    pub fn type_check(&mut self) -> Result<TypeAnnotation, String> {
        let mut equs = TypeEquations::new();
        let mut ta = TypeAnnotation::new();
        for f in self.funcs.iter() {
            equs.regist_func_info(f);
            ta.regist_func_info(f);
            f.gen_type(&mut equs)?;
            for TypeSubst { tv, t } in equs.unify()? {
                ta.insert(tv, t);
            }
            equs.clear_equations();
            println!("{:?}", equs);
        }
        Ok(ta)
    }
}

impl GenType for FullContent { fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        for f in self.funcs.iter() {
            f.gen_type(equs)?;
        }
        Ok(Type::End)
    }
}

impl Transpile for FullContent {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        let mut res = "#include <bits/stdc++.h>\n\n".to_string();
        for f in self.funcs.iter() {
            let s = f.transpile(ta);
            res.push_str(&s);
        }
        res
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
    let (_, mut t) = parse_full_content("fn two(z: i64) -> i64 { 2i64 } fn func(x: i64) -> i64 { let y = x; two(x) }").unwrap();
    println!("{:?}", t);
    let ta = t.type_check().unwrap();
    println!("{:#?}", ta);
}

#[test]
fn gentype_full_test2() {
    let prog = "fn generics_func<T>(x: T) -> T { x } fn echo(x: i64) -> i64 { let y = generics_func(x); let z = generics_func(false); y }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn gentype_full_test3() {
    let prog = "fn plus<T>(x: T, y: T) -> T { x + y } fn equ(a: i64, b: i64, c: i64, d: i64) -> bool { plus(a, b) == plus(c, d) }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn gentype_full_test4() {
    let prog = "fn plus<T>(x: T, y: T) -> T { x + y } fn equ(a: i64, b: i64, c: i64, d: i64) -> bool { plus(a, b) == plus(c, d) }";

    let (_, mut t) = parse_full_content(prog).unwrap();
    //println!("{:?}", s);
    //println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn gentype_full_test5() {
    let prog = "fn equ(a: i64, b: i64, c: i64, d: i64) -> u64 { let result = if a == b { 1u64 } else if c == d { 2u64 } else { 3u64 }; result }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}


#[test]
fn gentype_full_test6() {
    let prog = "fn equ(a: i64) -> u64 { let res = if true { true } else { 1u64 }; 2u64 }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}

