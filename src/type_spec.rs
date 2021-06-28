use std::collections::HashMap;

use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::combinator::*;
use nom::multi::*;
use nom::branch::*;
use nom::bytes::complete::*;

use crate::type_id::*;
use crate::traits::*;

use crate::unify::*;
use crate::trans::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeSign {
    pub id: TypeId,
    pub gens: Vec<TypeSpec>,
}

#[derive(Debug, Clone)]
pub struct GenericsTypeMap<'a> {
    gens_mp: HashMap<TypeId, Type>,
    nxt: Option<&'a GenericsTypeMap<'a>>,
}

impl<'a> GenericsTypeMap<'a> {
    pub fn empty() -> Self {
        GenericsTypeMap { gens_mp: HashMap::new(), nxt: None }
    }
    pub fn next(&'a self, gens_mp: HashMap<TypeId, Type>) -> Self {
        GenericsTypeMap { gens_mp, nxt: Some(self) }
    }
    pub fn get(&'a self, id: &TypeId) -> Option<&'a Type> {
        match self.gens_mp.get(id) {
            Some(ty) => Some(ty),
            None => {
                match self.nxt {
                    Some(ref nxt) => nxt.get(id),
                    None => None,
                }
            }
        }
    }
}


impl TypeSign {
    pub fn generics_to_type(&self, mp: &GenericsTypeMap, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match mp.get(&self.id).cloned() {
            Some(t) => {
                if self.gens.len() == 0 { Ok(t) }
                else { Err(format!("generics type cant have generics argument")) }
            }
            _ => {
                if self.id == TypeId::from_str("Self") {
                    if self.gens.len() == 0 {
                        equs.get_self_type()
                    }
                    else {
                        Err(format!("Self cant have generics arg"))
                    }
                }
                else  {
                    let gens = self.gens.iter().map(|gen| gen.generics_to_type(mp, equs, trs)).collect::<Result<_, _>>()?;
                    trs.check_typeid_with_generics(
                        equs,
                        self.id.clone(),
                        gens,
                        trs)
                }
            }
        }
    }
    pub fn generate_type_no_auto_generics(&self, equs: &TypeEquations, trs: &TraitsInfo) -> TResult {
        if self.id == TypeId::from_str("Self") {
            if self.gens.len() == 0 {
                equs.get_self_type()
            }
            else {
                Err(format!("Self cant have generics arg"))
            }
        }
        else  {
            trs.check_typeid_no_auto_generics(
                self.id.clone(),
                self.gens.iter().map(|gen| gen.generate_type_no_auto_generics(equs, trs)).collect::<Result<_, _>>()?,
                trs
                )
        }
    }

    pub fn get_type_id(&self) -> TypeId {
        self.id.clone()
    }
}

fn parse_generics_annotation(s: &str) -> IResult<&str, Vec<TypeSpec>> {
    let (s, op) = opt(tuple((char('<'), space0, parse_type_spec, space0, many0(tuple((char(','), space0, parse_type_spec, space0))), opt(tuple((space0, char(',')))), space0, char('>'))))(s)?;
    let v = match op {
        None => Vec::new(),
        Some((_, _, ty, _, m0, _, _, _)) => {
            let mut v = vec![ty];
            for (_, _, ty, _) in m0 {
                v.push(ty);
            }
            v
        }
    };
    Ok((s, v))
}


pub fn parse_type_sign(s: &str) -> IResult<&str, TypeSign> {
    let (s, (id, _, gens)) = tuple((parse_type_id, space0, parse_generics_annotation))(s)?;
    Ok((s, TypeSign { id, gens }))
}


/* impl GenType for TypeSign {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        if self.id == TypeId::from_str("Self") {
            if self.gens.len() == 0 {
                equs.get_self_type()
            }
            else {
                Err(format!("Self cant have generics arg"))
            }
        }
        else {
            Ok(Type::Generics(self.id.clone(), self.gens.iter().map(|gen| gen.gen_type(equs, trs)).collect::<Result<_, _>>()?))
        }
    }
} */

impl Transpile for TypeSign {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        if let Some((ids, cppinline)) = ta.is_inline_struct(&self.id) {
            let mp = ids.iter().cloned().zip(self.gens.iter().map(|g| g.transpile(ta))).collect::<HashMap<_, _>>();
            cppinline.transpile(ta, &mp)
        }
        else {
            let gens_trans = if self.gens.len() > 0 {
                format!("<{}>", self.gens.iter().map(|gen| gen.transpile(ta)).collect::<Vec<_>>().join(", "))
            }
            else {
                format!("")
            };
            /* let ty = if self.id == TypeId::from_str("Self") {
               ta.annotation(self.id.id.get_tag_number(), 0).transpile(ta)
               } else {

               }; */
            format!("{}{}", self.id.transpile(ta), gens_trans)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeSpec {
    TypeSign(TypeSign),
    Pointer(Box<TypeSpec>),
    Associated(Box<TypeSpec>, AssociatedType),
}

impl TypeSpec {
    pub fn generics_to_type(&self, mp: &GenericsTypeMap, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            TypeSpec::TypeSign(ref sign) => {
                sign.generics_to_type(mp, equs, trs)
            }
            TypeSpec::Pointer(ref spec) => {
                unimplemented!();
            }
            TypeSpec::Associated(ref spec, ref asso) => {
                Ok(Type::AssociatedType(Box::new(spec.as_ref().generics_to_type(mp, equs, trs)?), asso.clone()))
            }
        }
    }

    pub fn generate_type_no_auto_generics(&self, equs: &TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            TypeSpec::TypeSign(ref sign) => {
                sign.generate_type_no_auto_generics(equs, trs)
            }
            TypeSpec::Pointer(ref spec) => {
                unimplemented!();
            }
            TypeSpec::Associated(ref spec, ref asso) => {
                Ok(Type::AssociatedType(Box::new(spec.as_ref().generate_type_no_auto_generics(equs, trs)?), asso.clone()))
            }
        }
    }

    pub fn associated_type_depth(&self) -> usize {
        match self {
            TypeSpec::TypeSign(_) => 0,
            TypeSpec::Pointer(spec) => {
                spec.associated_type_depth()
            }
            TypeSpec::Associated(spec, _) => 1 + spec.associated_type_depth(),
        }
    }

    pub fn get_type_id(&self) -> Result<TypeId, String> {
        match self {
            TypeSpec::TypeSign(sign) => Ok(sign.get_type_id()),
            TypeSpec::Pointer(spec) => {
                unimplemented!();
            }
            TypeSpec::Associated(_, _) => Err(format!("cant get typeid from {:?}", self)),
        }
    }

    pub fn from_str(s: &str) -> Self {
        TypeSpec::TypeSign(TypeSign { id: TypeId::from_str(s), gens: Vec::new() })
    }
    pub fn from_id(id: &TypeId) -> Self {
        TypeSpec::TypeSign(TypeSign { id: id.clone(), gens: Vec::new() })
    }
}

fn parse_type_spec_subseq(s: &str, prev: TypeSpec) -> IResult<&str, TypeSpec> {
    if let Ok((ss, (_, _, _, asso_ty))) = tuple((space0, char('#'), space0, parse_associated_type))(s) {
        parse_type_spec_subseq(ss, TypeSpec::Associated(Box::new(prev), asso_ty))
    }
    else {
        Ok((s, prev))
    }
}

fn parse_type_spec_pointer(s: &str) -> IResult<&str, TypeSpec> {
    let (s, (_, _, spec)) = tuple((tag("*"), space0, parse_type_spec))(s)?;
    Ok((s, TypeSpec::Pointer(Box::new(spec))))
}

fn parse_type_spec_paren(s: &str) -> IResult<&str, TypeSpec> {
    let (s, (_, _, spec, _)) = tuple((tag("("), space0, parse_type_spec, tag(")")))(s)?;
    Ok((s, spec))
}

fn parse_type_spec_sign(s: &str) -> IResult<&str, TypeSpec> {
    let (s, sign) = parse_type_sign(s)?;
    let prev = TypeSpec::TypeSign(sign);
    parse_type_spec_subseq(s, prev)
}

pub fn parse_type_spec(s: &str) -> IResult<&str, TypeSpec> {
    alt((parse_type_spec_pointer, parse_type_spec_paren, parse_type_spec_sign))(s)
}

/* 
impl GenType for TypeSpec {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            TypeSpec::TypeSign(ref sign) => sign.gen_type(equs, trs),
            TypeSpec::Associated(ref specs, ref asso) => {
                let specs_type = specs.gen_type(equs, trs)?;
                Ok(Type::AssociatedType(Box::new(specs_type), asso.clone()))
            }
        }
    }
} */

impl Transpile for TypeSpec {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            TypeSpec::TypeSign(ref sign) => sign.transpile(ta),
            TypeSpec::Pointer(ref spec) => {
                format!("{}*", spec.transpile(ta))
            }
            TypeSpec::Associated(ref spec, AssociatedType { ref trait_id, ref type_id } ) => {
                format!("typename {}<{}>::{}", trait_id.transpile(ta), spec.transpile(ta), type_id.transpile(ta))
            }
        }
                
    }
}

#[test]
fn parse_type_spec_test() {
    println!("{:?}", parse_type_spec("i64"));
    println!("{:?}", parse_type_spec("i64#MyTrait::Output"));
    println!("{:?}", parse_type_spec("Pair<Pair<i64, u64>, bool>"));
    println!("{:?}", parse_type_spec("T#MyTrait::Output#MyTrait::Output"));
    println!("{:?}", parse_type_spec("(i64)"));
    println!("{:?}", parse_type_spec("*i64"));
    println!("{:?}", parse_type_spec("*(*i64)"));
    println!("{:?}", parse_type_spec("*(T#MyTrait::Output)"));
    // println!("{:?}", parse_type_spec("Pair<Pair<i64, u64>, bool>").unwrap().1.gen_type(&mut equs));
}
    
