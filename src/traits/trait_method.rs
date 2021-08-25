use nom::IResult;
use nom::sequence::*;

use crate::identifier::*;
use crate::traits::*;

use crate::trans::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitMethodIdentifier {
    pub id: Identifier,
}

impl Transpile for TraitMethodIdentifier {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        self.id.into_string()
    }
}


pub fn parse_trait_method_identifier(s: &str) -> IResult<&str, TraitMethodIdentifier> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TraitMethodIdentifier { id }))
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitMethod {
    pub trait_id: TraitId,
    pub method_id: TraitMethodIdentifier,
}

pub fn parse_trait_method(s: &str) -> IResult<&str, TraitMethod> {
    let (s, (trait_id, _, _, _, method_id)) = tuple((parse_trait_id, multispace0, tag("."), multispace0, parse_trait_method_identifier))(s)?;
    Ok((s, TraitMethod { trait_id, method_id }))
}
