use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::unify::*;
use crate::trans::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeId {
    pub id: Identifier,
}

impl GenType for TypeId {
    fn gen_type(&self, _: &mut TypeEquations) -> TResult {
        Ok(Type::Type(self.id.clone()))
    }
}

impl Transpile for TypeId {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        self.id.into_string()
    }
}

pub fn parse_type_id(s: &str) -> IResult<&str, TypeId> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TypeId { id }))
}
