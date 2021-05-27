use std::collections::HashMap;

use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;

use crate::type_id::*;
use crate::traits::*;

use crate::unify::*;
use crate::trans::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeSign {
    pub id: TypeId,
    pub gens: Vec<TypeSpec>,
}

impl TypeSign {
    pub fn generics_to_type(&self, mp: &HashMap<TypeId, Type>, equs: &mut TypeEquations) -> TResult {
        match mp.get(&self.id).cloned() {
            Some(t) => {
                if self.gens.len() == 0 { Ok(t) }
                else { Err(format!("generics type cant have generics argument")) }
            }
            None => {
                Ok(Type::Generics(
                        self.id.clone(),
                        self.gens.iter().map(|gen| gen.generics_to_type(mp, equs)).collect::<Result<_, _>>()?
                        ))
            }
        }
    }
    pub fn check_typeid(self, trs: &TraitsInfo) -> TResult {
        if self.gens.len() == 0 {
            self.check_typeid(trs)
        }
        else {
            unreachable!("do not check generics type");
        }
    }
}

pub fn parse_type_sign(s: &str) -> IResult<&str, TypeSign> {
    let (s, id) = parse_type_id(s)?;
    let gens = Vec::new();
    Ok((s, TypeSign { id, gens }))
}


impl GenType for TypeSign {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        Ok(Type::Generics(self.id.clone(), self.gens.iter().map(|gen| gen.gen_type(equs)).collect::<Result<_, _>>()?))
    }
}

impl Transpile for TypeSign {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        let gens_trans = self.gens.iter().map(|gen| gen.transpile(ta)).collect::<Vec<_>>().join(", ");
        format!("{}<{}>", self.id.transpile(ta), gens_trans)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    TypeSign(TypeSign),
    Associated(Box<TypeSpec>, AssociatedType),
}

impl TypeSpec {
    pub fn generics_to_type(&self, mp: &HashMap<TypeId, Type>, equs: &mut TypeEquations) -> TResult {
        match *self {
            TypeSpec::TypeSign(ref sign) => {
                sign.generics_to_type(mp, equs)
            }
            TypeSpec::Associated(ref spec, ref asso) => {
                Ok(Type::AssociatedType(Box::new(spec.as_ref().generics_to_type(mp, equs)?), asso.clone()))
            }
        }
    }

    pub fn check_typeid(self, trs: &TraitsInfo) -> TResult {
        match self {
            TypeSpec::TypeSign(sign) =>
                sign.check_typeid(trs),
            TypeSpec::Associated(spec, asso) =>
                unreachable!("do not check_typeid TypeSpec::Associated"),
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

impl GenType for TypeSpec {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        match *self {
            TypeSpec::TypeSign(ref sign) => sign.gen_type(equs),
            TypeSpec::Associated(ref specs, ref asso) => {
                let specs_type = specs.gen_type(equs)?;
                Ok(Type::AssociatedType(Box::new(specs_type), asso.clone()))
            }
        }
    }
}

impl Transpile for TypeSpec {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
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
    println!("{:?}", parse_type_spec("i64"));
    println!("{:?}", parse_type_spec("i64#MyTrait::Output"));
}
    
