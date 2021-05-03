use std::collections::HashMap;

use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;

use crate::type_id::*;
use crate::traits::*;

use crate::unify::*;
use crate::trans::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeSpec {
    TypeId(TypeId),
    Associated(Box<TypeSpec>, AssociatedType),
}

impl TypeSpec {
    pub fn generics_to_type(&self, mp: &HashMap<TypeId, Type>, equs: &mut TypeEquations) -> TResult {
        match *self {
            TypeSpec::TypeId(ref id) => {
                match mp.get(id).cloned() {
                    Some(t) => Ok(t),
                    None => self.gen_type(equs),
                }
            }
            TypeSpec::Associated(ref spec, ref asso) => {
                Ok(Type::AssociatedType(Box::new(spec.as_ref().generics_to_type(mp, equs)?), asso.clone()))
            }
        }
    }
}

pub fn parse_type_spec(s: &str) -> IResult<&str, TypeSpec> {
    let (s, type_id) = parse_type_id(s)?;
    let mut now = s;
    let mut prev = TypeSpec::TypeId(type_id);
    while let Ok((s, (_, _, _, asso_ty))) = tuple((space0, char('#'), space0, parse_associated_type))(now) {
        now = s;
        prev = TypeSpec::Associated(Box::new(prev), asso_ty);
    }
    Ok((now, prev))
}

impl GenType for TypeSpec {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        match *self {
            TypeSpec::TypeId(ref id) => id.gen_type(equs),
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
            TypeSpec::TypeId(ref id) => id.transpile(ta),
            TypeSpec::Associated(ref spec, AssociatedType { ref trait_id, ref type_id } ) => {
                format!("{}<{}>::{}", trait_id.transpile(ta), type_id.transpile(ta), spec.transpile(ta))
            }
        }
                
    }
}

#[test]
fn parse_type_spec_test() {
    println!("{:?}", parse_type_spec("i64"));
    println!("{:?}", parse_type_spec("i64#MyTrait::Output"));
}
    
