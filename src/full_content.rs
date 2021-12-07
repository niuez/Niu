use std::collections::{ HashSet, HashMap };
use std::path::*;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;
use nom::branch::*;


use crate::func_definition::{ FuncDefinition, parse_func_definition };
use crate::traits::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::structs::*;
use crate::unit_test::*;
use crate::error::*;

#[derive(Debug)]
pub struct FullContent {
    pub programs: HashMap<String, String>,
    pub structs: Vec<(StructDefinition, String)>,
    pub traits: Vec<(TraitDefinition, String)>,
    pub impls: Vec<(ImplDefinition, String)>,
    pub funcs: Vec<(FuncDefinition, String)>,
    pub includes: Vec<(String, String)>,
    unit_tests: Vec<UnitTestFunc>,
}

fn name_errmap<T, O, E, F>(elem: &(T, String), mut f: F) -> Result<O, (E, &str)> 
where
    F: FnMut(&T) -> Result<O, E> {
    f(&elem.0).map_err(|e| (e, elem.1.as_str()))
}

impl FullContent {
    fn regist_traits(&self, trs: &mut TraitsInfo) -> Result<(), (Error, &str)> {
        for t in self.traits.iter() {
            name_errmap(t, |tr| trs.regist_trait(tr))?;
        }
        Ok(())
    }
    fn regist_impls(&self, equs: &mut TypeEquations, trs: &mut TraitsInfo) -> Result<(), (Error, &str)> {
        for (im, _) in self.impls.iter() {
            trs.preregist_impl_candidate(im);
        }
        for i in self.impls.iter() {
            name_errmap(i, |im| trs.regist_impl_candidate(equs, im))?;
        }
        Ok(())
    }
    fn regist_self_impls(&self, trs: &mut TraitsInfo) -> Result<(), (Error, &str)> {
        for st in self.structs.iter() {
            name_errmap(st, |st| trs.regist_self_impl(st.get_impl_self_def()))?;
        }
        Ok(())
    }

    fn inner_type_check(&self) -> Result<TypeAnnotation, (Error, &str)> {
        let mut equs = TypeEquations::new();
        let mut ta = TypeAnnotation::new();
        let mut trs = TraitsInfo::new();

        for st in self.structs.iter() {
            name_errmap(st, |st| {
                ta.regist_structs_info(st.get_member_def());
                trs.regist_structs_info(st.get_member_def())
            })?;
        }

        self.regist_traits(&mut trs)?;
        self.regist_impls(&mut equs, &mut trs)?;
        self.regist_self_impls(&mut trs)?;


        for f in self.funcs.iter() {
            equs.regist_func_info(&f.0);
            ta.regist_func_info(&f.0);
            name_errmap(f, |f| f.unify_definition(&mut equs, &mut trs, &ErrorHint::None))?;
        }

        for st in self.structs.iter() {
            name_errmap(st, |st| st.unify_require_methods(&mut equs, &mut trs))?;
        }

        for im in self.impls.iter() {
            name_errmap(im, |im| im.unify_require_methods(&mut equs, &mut trs))?;
        }


        for TypeSubst { tv, t } in equs.take_substs() {
            ta.insert(tv, t);

        }
        ta.regist_copyable(equs.copyable);
        Ok(ta)
    }
    pub fn type_check(&self) -> Result<TypeAnnotation, String> {
        let res = self.inner_type_check();
        res.map_err(|(err, name)| {
                let data = ErrorData { statement: self.programs.get(name).unwrap() };
                format!("unify error\n{}", err.what(&data))
            }
        )
    }

    pub fn mut_check(&self, ta: &TypeAnnotation) -> Result<(), String> {
        let mut vars = VariablesInfo::new();
        for (st, _name) in self.structs.iter() {
            st.mut_check(ta, &mut vars)?;
        }

        for (im, _name) in self.impls.iter() {
            im.mut_check(ta, &mut vars)?;
        }

        for (f, _name) in self.funcs.iter() {
            f.mut_check(ta, &mut vars)?;
        }
        Ok(())
    }
    pub fn move_check(&self, ta: &TypeAnnotation) -> Result<VariablesMoveChecker, String> {
        let mut mc = VariablesMoveChecker::new();
        for (st, _name) in self.structs.iter() {
            st.move_check(&mut mc, ta)?;
        }

        for (im, _name) in self.impls.iter() {
            im.move_check(&mut mc, ta)?;
        }
        for (f, _name) in self.funcs.iter() {
            f.move_check(&mut mc, ta)?;
        }
        Ok(mc)
    }
    pub fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        let mut res = String::new();
        for (include, _name) in self.includes.iter() {
            res.push_str(&format!("#include <{}>\n", include));
        }
        res.push_str("\n");
        let mut operators = HashMap::new();
        let opes_str = ["Index", "IndexMut", "Clone"];
        for ope in opes_str {
            operators.insert(ope.to_string(), HashSet::new());
        }
        for (t, _name) in self.impls.iter() {
            let tr_id = t.get_trait_id().id.into_string();
            if let Some(set) = operators.get_mut(&tr_id) {
                if let Some(id) = t.get_impl_ty_id() {
                    set.insert(id);
                }
            }
        }
        // structs definition
        for (t, _name) in self.structs.iter() {
            ta.self_type = Some(t.transpile_self_type());
            let s = t.transpile_definition(ta);
            res.push_str(&s);
            ta.self_type = None;
        }
        // traits definition
        for (t, _name) in self.traits.iter() {
            let s = t.transpile(ta);
            res.push_str(&s);
        }
        // impls definition
        for (i, _name) in self.impls.iter() {
            ta.self_type = Some(i.impl_ty.transpile(ta));
            let s = i.transpile(ta);
            res.push_str(&s);
            ta.self_type = None;
        }
        // functions definition
        for (f, _name) in self.funcs.iter() {
            let s = f.transpile_definition_only(ta, "", false);
            res.push_str(&s);
            res.push_str(";\n");
        }
        // structs implementation
        for (t, _name) in self.structs.iter().rev() {
            ta.self_type = Some(t.transpile_self_type());
            let st_id = t.get_id();
            let opes = operators.iter()
                .filter_map(|(k, set)| if set.contains(&st_id) { Some(k.clone()) } else { None })
                .collect::<Vec<_>>();
            let s = t.transpile(ta, opes);
            res.push_str(&s);
            ta.self_type = None;
        }
        // functions of impls implementation
        for (i, _name) in self.impls.iter() {
            ta.self_type = Some(i.impl_ty.transpile(ta));
            let s = i.transpile_functions(ta);
            res.push_str(&s);
            ta.self_type = None;
        }
        for (f, _name) in self.funcs.iter() {
            let s = f.transpile(ta, false);
            res.push_str(&s);
        }
        res
    }

    pub fn transpile_tests(&self, ta: &TypeAnnotation) -> Vec<UnitTestTranspiled> {
        self.unit_tests.iter().map(|unit_test| unit_test.transpile(ta)).collect()
    }
}


#[derive(Debug)]
enum ContentElement {
    Struct(StructDefinition),
    Func(FuncDefinition),
    Trait(TraitDefinition),
    ImplTrait(ImplDefinition),
    Import(String),
    Include(String),
    UnitTest(UnitTestFunc),
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

fn parse_element_import(s: &str) -> IResult<&str, ContentElement> {
    let (s, (_, _, _, _, path, _, _)) = tuple((multispace0, tag("import"), multispace0, char('"'), is_not("\""), char('"'), multispace0))(s)?;
    Ok((s, ContentElement::Import(path.to_string())))
}

fn parse_element_include(s: &str) -> IResult<&str, ContentElement> {
    let (s, (_, _, _, _, path, _, _)) = tuple((multispace0, tag("#include"), multispace0, char('<'), is_not(">"), char('>'), multispace0))(s)?;
    Ok((s, ContentElement::Include(path.to_string())))
}

fn parse_element_unit_test(s: &str) -> IResult<&str, ContentElement> {
    let (s, (_, unit_test, _)) = tuple((multispace0, parse_unit_test_func, multispace0))(s)?;
    Ok((s, ContentElement::UnitTest(unit_test)))
}


fn parse_content_element(s: &str) -> IResult<&str, ContentElement> {
    alt((parse_element_include, parse_element_import, parse_element_struct, parse_element_func, parse_element_trait, parse_element_impl_trait, parse_element_unit_test))(s)
}

pub fn parse_full_content<'a>(s: &'a str, name: &str) -> IResult<&'a str, (Vec<(String, String)>, FullContent)> {
    let (ss, (_, elems, _)) = tuple((multispace0, many0(tuple((parse_content_element, multispace0))), multispace0))(s)?;
    
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    let mut traits = Vec::new();
    let mut impls = Vec::new();
    let mut imports = Vec::new();
    let mut includes = Vec::new();
    let mut unit_tests = Vec::new();
    for (e, _) in elems {
        match e {
            ContentElement::Struct(s) => structs.push((s, name.to_string())),
            ContentElement::Func(f) => funcs.push((f, name.to_string())),
            ContentElement::Trait(t) => traits.push((t, name.to_string())),
            ContentElement::ImplTrait(it) => impls.push((it, name.to_string())),
            ContentElement::Import(path) => imports.push((path, name.to_string())),
            ContentElement::Include(path) => includes.push((path, name.to_string())),
            ContentElement::UnitTest(unit_test) => unit_tests.push(unit_test),
        }
    }
    let programs = std::iter::once((name.to_string(), s.to_string())).collect();
    Ok((ss, (imports, FullContent { programs, structs, funcs, traits, impls, includes, unit_tests, })))
}


pub fn parse_full_content_from_file(filename: &str, import_path: &[PathBuf]) -> Result<FullContent, String> {
    let mut programs = HashMap::new();
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    let mut traits = Vec::new();
    let mut impls = Vec::new();
    let mut includes = Vec::new();
    let mut unit_tests = Vec::new();

    let mut que = Vec::new();
    let mut read = HashSet::new();
    {
        let path = Path::new(filename).canonicalize().map_err(|e| format!("{:?}", e))?.to_path_buf();
        if path.is_file() {
            que.push((path.clone(), true));
            read.insert(path);
        }
        else {
            Err(format!("path {:?} is not file", path))?;
        }
    }
    
    while let Some((path, first_file)) = que.pop() {
        let program = std::fs::read_to_string(path.as_path()).map_err(|_| format!("cant open {}", filename))?;
        let (s, (imports, mut full)) = crate::full_content::parse_full_content(&program, path.to_str().unwrap()).map_err(|e| format!("{:?}", e))?;
        if s != "" {
            Err(format!("path {:?} parse error, remaining -> {}", path, s))?;
        }
        dbg!(import_path);
        for (import, _) in imports.into_iter() {
            let mut ok = false;
            for mut import_dir in import_path.into_iter().cloned().chain(std::iter::once(path.parent().unwrap().to_path_buf())) {
                import_dir.push(&import);
                if let Ok(path) = import_dir.as_path().canonicalize().map(|p| p.to_path_buf()) {
                    log::debug!("path {:?}", path);
                    if read.insert(path.clone()) {
                        if path.is_file() {
                            que.push((path, false));
                            ok = true;
                            break;
                        }
                    }
                    else {
                        log::debug!("already exist");
                        ok = true;
                        break;
                    }
                }
            }
            if !ok {
                Err(format!("cant find {}", import))?;
            }
        }
        programs.extend(full.programs);
        structs.append(&mut full.structs);
        funcs.append(&mut full.funcs);
        traits.append(&mut full.traits);
        impls.append(&mut full.impls);
        includes.append(&mut full.includes);
        if first_file {
            unit_tests.append(&mut full.unit_tests);
        }
    }
    includes.push(("type_traits".to_string(), "".to_string()));
    includes.push(("tuple".to_string(), "".to_string()));
    includes.sort();
    includes.dedup();
    Ok(FullContent { programs, structs, funcs, traits, impls, includes, unit_tests })
}
/*
#[test]
fn parse_full_content_test() {
    log::debug!("{:?}", parse_full_content("fn func(x: i64) -> i64 { let y = x * x; y + x } fn add(x: i64) -> i64 { x + x }"))
}

#[test]
fn gentype_full_test() {
    let (_, mut t) = parse_full_content("fn two(z: i64) -> i64 { 2i64 } fn func(x: i64) -> i64 { let y = x; two(x) }").unwrap();
    log::debug!("{:?}", t);
    let ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);
}

#[test]
fn gentype_full_test2() {
    let prog = "fn generics_func<T>(x: T) -> T { x } fn echo(x: i64) -> i64 { let y = generics_func(x); let z = generics_func(false); y }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn gentype_full_test3() {
    let prog = "fn plus<T>(x: T, y: T) -> T { x + y } fn equ(a: i64, b: i64, c: i64, d: i64) -> bool { plus(a, b) == plus(c, d) }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn gentype_full_test4() {
    let prog = "fn plus<T>(x: T, y: T) -> T { x + y } fn equ(a: i64, b: i64, c: i64, d: i64) -> bool { plus(a, b) == plus(c, d) }";

    let (_, mut t) = parse_full_content(prog).unwrap();
    //log::debug!("{:?}", s);
    //log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn gentype_full_test5() {
    let prog = "fn equ(a: i64, b: i64, c: i64, d: i64) -> u64 { let result = if a == b { 1u64 } else if c == d { 2u64 } else { 3u64 }; result }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}


#[test]
fn gentype_full_test6() {
    let prog = "fn equ(a: i64) -> u64 { let res = if true { true } else { 1u64 }; 2u64 }";

    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}


#[test]
fn parse_content_element_test() {
    log::debug!("{:?}", parse_full_content("trait MyTrait { type Output; } impl MyTrait for i64 { type Output = u64; } fn equ(a: i64) -> i64 { a }"));
}


#[test]
fn unify_test_for_selection_candidate() {
    let prog = "trait MyTrait { type Output; } impl MyTrait for u64 { type Output = u64; } fn equ<T: MyTrait>(t: T) -> T { t } fn apply<A: MyTrait>(a: A) -> A { equ(a) }";
    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}

#[test]
fn unify_test_for_impl() {
    let prog = "trait MyTrait { type Output; fn out(a: Self) -> Self#MyTrait::Output; } impl MyTrait for i64 { type Output = u64; fn out(a: i64) -> u64 { 1u64 }} fn apply() -> u64 { i64#MyTrait.out(1i64) }";
    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}


#[test]
fn unify_test_for_param_candidate() {
    let prog = "trait MyTrait { type Output; fn out(a: Self) -> Self#MyTrait::Output; } impl MyTrait for i64 { type Output = u64; fn out(a: i64) -> u64 { 1u64 }} fn apply<T: MyTrait>(t: T) -> T#MyTrait::Output { T#MyTrait.out(t) }";
    let (s, mut t) = parse_full_content(prog).unwrap();
    log::debug!("{:?}", s);
    log::debug!("{:?}", t);
    let mut ta = t.type_check().unwrap();
    log::debug!("{:#?}", ta);

    log::debug!("```cpp\n{}```\n", t.transpile(&mut ta));
}
*/
