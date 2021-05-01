use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;

use crate::type_id::*;
use crate::traits::*;

use crate::unify::*;

#[derive(Debug, Clone)]
pub enum TypeSpec {
    TypeId(TypeId),
    Associated(Box<TypeSpec>, AssociatedType),
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

#[test]
fn parse_type_spec_test() {
    println!("{:?}", parse_type_spec("i64"));
    println!("{:?}", parse_type_spec("i64#MyTrait::Output"));
}
    
