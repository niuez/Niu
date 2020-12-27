use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };

#[derive(Debug)]
pub struct TypeId {
    pub id: Identifier,
}

pub fn parse_type_id(s: &str) -> IResult<&str, TypeId> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TypeId { id }))
}
