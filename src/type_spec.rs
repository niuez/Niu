use std::collections::HashMap;

use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::combinator::*;
use nom::multi::*;

use crate::type_id::*;
use crate::traits::*;

use crate::unify::*;
use crate::trans::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeSign {
    pub id: TypeId,
    pub gens: Vec<TypeSpec>,
}

impl TypeSign {
    pub fn generics_to_type(&self, mp: Option<&HashMap<TypeId, Type>>, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match mp.map(|mp| mp.get(&self.id).cloned()) {
            Some(Some(t)) => {
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
                    trs.check_typeid_with_generics(
                        self.id.clone(),
                        self.gens.iter().map(|gen| gen.generics_to_type(mp, equs, trs)).collect::<Result<_, _>>()?,
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
        let gens_trans = if self.gens.len() > 0 {
            format!("<{}>", self.gens.iter().map(|gen| gen.transpile(ta)).collect::<Vec<_>>().join(", "))
        }
        else {
            format!("")
        };
        format!("{}{}", self.id.transpile(ta), gens_trans)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeSpec {
    TypeSign(TypeSign),
    Associated(Box<TypeSpec>, AssociatedType),
}

impl TypeSpec {
    pub fn generics_to_type(&self, mp: Option<&HashMap<TypeId, Type>>, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            TypeSpec::TypeSign(ref sign) => {
                sign.generics_to_type(mp, equs, trs)
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
            TypeSpec::Associated(ref spec, ref asso) => {
                Ok(Type::AssociatedType(Box::new(spec.as_ref().generate_type_no_auto_generics(equs, trs)?), asso.clone()))
            }
        }
    }

    pub fn associated_type_depth(&self) -> usize {
        match self {
            TypeSpec::TypeSign(_) => 0,
            TypeSpec::Associated(spec, _) => 1 + spec.associated_type_depth(),
        }
    }

    pub fn from_str(s: &str) -> Self {
        TypeSpec::TypeSign(TypeSign { id: TypeId::from_str(s), gens: Vec::new() })
    }
    pub fn from_id(id: &TypeId) -> Self {
        TypeSpec::TypeSign(TypeSign { id: id.clone(), gens: Vec::new() })
    }
}

pub fn parse_type_spec(s: &str) -> IResult<&str, TypeSpec> {
    let (s, type_id) = parse_type_sign(s)?;
    let mut now = s;
    let mut prev = TypeSpec::TypeSign(type_id);
    while let Ok((s, (_, _, _, asso_ty))) = tuple((space0, char('#'), space0, parse_associated_type))(now) {
        now = s;
        prev = TypeSpec::Associated(Box::new(prev), asso_ty);
    }
    Ok((now, prev))
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
            TypeSpec::Associated(ref spec, AssociatedType { ref trait_id, ref type_id } ) => {
                format!("typename {}<{}>::{}", trait_id.transpile(ta), spec.transpile(ta), type_id.transpile(ta))
            }
        }
                
    }
}

#[test]
fn parse_type_spec_test() {
    let mut equs = TypeEquations::new();
    println!("{:?}", parse_type_spec("i64"));
    println!("{:?}", parse_type_spec("i64#MyTrait::Output"));
    println!("{:?}", parse_type_spec("Pair<Pair<i64, u64>, bool>"));
    println!("{:?}", parse_type_spec("T#MyTrait::Output#MyTrait::Output"));
    // println!("{:?}", parse_type_spec("Pair<Pair<i64, u64>, bool>").unwrap().1.gen_type(&mut equs));
}
    
