use nom::IResult;
use nom::sequence::*;

use crate::identifier::*;
use crate::traits::*;

#[derive(Debug, Clone)]
pub struct AssociatedTypeIdentifier {
    pub id: Identifier,
}

pub fn parse_associated_type_identifier(s: &str) -> IResult<&str, AssociatedTypeIdentifier> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, AssociatedTypeIdentifier { id }))
}


#[derive(Debug, Clone)]
pub struct AssociatedType {
    pub trait_id: TraitId,
    pub type_id: AssociatedTypeIdentifier,
}

pub fn parse_associated_type(s: &str) -> IResult<&str, AssociatedType> {
    let (s, (trait_id, _, _, _, type_id)) = tuple((parse_trait_id, space0, tag("::"), space0, parse_associated_type_identifier))(s)?;
    Ok((s, AssociatedType { trait_id, type_id }))
}
