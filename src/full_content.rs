use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;
use nom::branch::*;


use crate::func_definition::{ FuncDefinition, parse_func_definition };
use crate::traits::*;
use crate::unify::*;
use crate::trans::*;
use crate::structs::*;

#[derive(Debug)]
pub struct FullContent {
    pub structs: Vec<StructDefinition>,
    pub traits: Vec<TraitDefinition>,
    pub impls: Vec<ImplDefinition>,
    pub self_impls: Vec<ImplSelfDefinition>,
    pub funcs: Vec<FuncDefinition>,
}

impl FullContent {
    fn regist_traits(&mut self, trs: &mut TraitsInfo) -> Result<(), String> {
        for tr in self.traits.iter() {
            trs.regist_trait(tr)?;
        }
        Ok(())
    }
    fn regist_impls(&mut self, trs: &mut TraitsInfo) -> Result<(), String> {
        for im in self.impls.iter() {
            trs.regist_impl_candidate(im)?;
        }
        Ok(())
    }
    fn regist_self_impls(&mut self, trs: &mut TraitsInfo) -> Result<(), String> {
        for im in self.self_impls.iter() {
            trs.regist_self_impl(im)?;
        }
        Ok(())
    }

    pub fn type_check(&mut self) -> Result<TypeAnnotation, String> {
        let mut equs = TypeEquations::new();
        let mut ta = TypeAnnotation::new();
        let mut trs = TraitsInfo::new();

        for st in self.structs.iter() {
            trs.regist_structs_info(&st)?;
            ta.regist_structs_info(st);
        }

        self.regist_traits(&mut trs)?;
        self.regist_impls(&mut trs)?;
        self.regist_self_impls(&mut trs)?;

        for im in self.impls.iter() {
            im.unify_require_methods(&mut equs, &mut trs)?;
        }

        for f in self.funcs.iter() {
            equs.regist_func_info(f);
            ta.regist_func_info(f);
            f.unify_definition(&mut equs, &mut trs)?;
        }

        for TypeSubst { tv, t } in equs.take_substs() {
            ta.insert(tv, t);

        }
        Ok(ta)
    }
}

impl Transpile for FullContent {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = "#include <bits/stdc++.h>\n\n".to_string();
        for t in self.structs.iter() {
            let s = t.transpile(ta);
            res.push_str(&s);
        }
        for t in self.traits.iter() {
            let s = t.transpile(ta);
            res.push_str(&s);
        }
        for i in self.impls.iter() {
            let s = i.transpile(ta);
            res.push_str(&s);
        }
        for f in self.funcs.iter() {
            let s = f.transpile(ta);
            res.push_str(&s);
        }
        res
    }
}

#[derive(Debug)]
enum ContentElement {
    Struct(StructDefinition),
    Func(FuncDefinition),
    Trait(TraitDefinition),
    ImplTrait(ImplDefinition),
    ImplSelf(ImplSelfDefinition),
}

fn parse_element_struct(s: &str) -> IResult<&str, ContentElement> {
    let (s, f) = parse_struct_definition(s)?;
    Ok((s, ContentElement::Struct(f)))
}

fn parse_element_func(s: &str) -> IResult<&str, ContentElement> {
    let (s, f) = parse_func_definition(s)?;
    Ok((s, ContentElement::Func(f)))
}

fn parse_element_trait(s: &str) -> IResult<&str, ContentElement> {
    let (s, t) = parse_trait_definition(s)?;
    Ok((s, ContentElement::Trait(t)))
}

fn parse_element_impl_trait(s: &str) -> IResult<&str, ContentElement> {
    let (s, it) = parse_impl_definition(s)?;
    Ok((s, ContentElement::ImplTrait(it)))
}

fn parse_element_self_impl_trait(s: &str) -> IResult<&str, ContentElement> {
    let (s, it) = parse_impl_self_definition(s)?;
    Ok((s, ContentElement::ImplSelf(it)))
}

fn parse_content_element(s: &str) -> IResult<&str, ContentElement> {
    alt((parse_element_struct, parse_element_func, parse_element_trait, parse_element_impl_trait, parse_element_self_impl_trait))(s)
}

pub fn parse_full_content(s: &str) -> IResult<&str, FullContent> {
    let (s, (_, elems, _)) = tuple((space0, many0(tuple((parse_content_element, space0))), space0))(s)?;
    
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    let mut traits = Vec::new();
    let mut impls = Vec::new();
    let mut self_impls = Vec::new();
    for (e, _) in elems {
        match e {
            ContentElement::Struct(s) => structs.push(s),
            ContentElement::Func(f) => funcs.push(f),
            ContentElement::Trait(t) => traits.push(t),
            ContentElement::ImplTrait(it) => impls.push(it),
            ContentElement::ImplSelf(it) => self_impls.push(it),
        }
    }
    Ok((s, FullContent { structs, funcs, traits, impls, self_impls, }))
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


#[test]
fn parse_content_element_test() {
    println!("{:?}", parse_full_content("trait MyTrait { type Output; } impl MyTrait for i64 { type Output = u64; } fn equ(a: i64) -> i64 { a }"));
}


#[test]
fn unify_test_for_selection_candidate() {
    let prog = "trait MyTrait { type Output; } impl MyTrait for u64 { type Output = u64; } fn equ<T: MyTrait>(t: T) -> T { t } fn apply<A: MyTrait>(a: A) -> A { equ(a) }";
    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn unify_test_for_impl() {
    let prog = "trait MyTrait { type Output; fn out(a: Self) -> Self#MyTrait::Output; } impl MyTrait for i64 { type Output = u64; fn out(a: i64) -> u64 { 1u64 }} fn apply() -> u64 { i64#MyTrait.out(1i64) }";
    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}


#[test]
fn unify_test_for_param_candidate() {
    let prog = "trait MyTrait { type Output; fn out(a: Self) -> Self#MyTrait::Output; } impl MyTrait for i64 { type Output = u64; fn out(a: i64) -> u64 { 1u64 }} fn apply<T: MyTrait>(t: T) -> T#MyTrait::Output { T#MyTrait.out(t) }";
    let (s, mut t) = parse_full_content(prog).unwrap();
    println!("{:?}", s);
    println!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    println!("{:#?}", ta);

    println!("```cpp\n{}```\n", t.transpile(&mut ta));
}
