//use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
//use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_id::{ TypeId, parse_type_id };
//use crate::unify::*;
//use crate::unary_expr::Variable;
//use crate::trans::*;


#[derive(Debug, Clone)]
pub struct TraitDefinition {
    pub trait_id: Identifier,
    pub types: Vec<Identifier>,
}

impl TraitDefinition {
    pub fn get_trait_id_pair(&self) -> (Identifier, TraitDefinition) {
        (self.trait_id.clone(), self.clone())
    }
}

pub fn parse_trait_definition(s: &str) -> IResult<&str, TraitDefinition> {
    let (s, (_, _, trait_id, _, _, _, many_types, _, _)) = 
        tuple((tag("trait"), space1, parse_identifier,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_identifier, space0, char(';'), space0))),
            space0, char('}')))(s)?;
    let types = many_types.into_iter().map(|(_, _, id, _, _, _)| id).collect();
    Ok((s, TraitDefinition { trait_id, types }))
}

#[derive(Debug, Clone)]
pub struct ImplTrait {
    pub trait_id: Identifier,
    pub impl_ty: TypeId,
    pub types: Vec<(Identifier, TypeId)>,
}

pub fn parse_impl_trait(s: &str) -> IResult<&str, ImplTrait> {
    let (s, (_, _, trait_id, _, _, _, impl_ty, _, _, _, many_types, _, _)) = 
        tuple((tag("impl"), space1, parse_identifier,
            space1, tag("for"), space1, parse_type_id,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_identifier, space0, char('='), space0, parse_type_id, space0, char(';'), space0))),
            space0, char('}')))(s)?;
    let types = many_types.into_iter().map(|(_, _, id, _, _, _, ty, _, _, _)| (id, ty)).collect();
    Ok((s, ImplTrait { trait_id, impl_ty, types }))
}


#[test]
fn parse_trait_definition_test() {
    println!("{:?}", parse_trait_definition("trait MyTrait { type Output; type Input; }"));
}
