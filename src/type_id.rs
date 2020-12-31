use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::unify::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeId {
    pub id: Identifier,
}

impl GenType for TypeId {
    fn gen_type(&self, _: &mut TypeEquations) -> TResult {
        Ok(Type::Type(self.id.clone()))
    }
}

pub fn parse_type_id(s: &str) -> IResult<&str, TypeId> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TypeId { id }))
}
