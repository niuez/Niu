use nom::IResult;
use nom::branch::*;
use nom::character::complete::*;
use nom::sequence::*;

use crate::type_id::*;
use crate::traits::*;

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

#[test]
fn parse_type_spec_test() {
    println!("{:?}", parse_type_spec("i64"));
    println!("{:?}", parse_type_spec("i64#MyTrait::Output"));
}
    
