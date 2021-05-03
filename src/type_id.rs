use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::unify::*;
use crate::trans::*;
use crate::type_spec::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeId {
    pub id: Identifier,
}

impl TypeId {
    pub fn from_str(s: &str) -> Self {
        TypeId { id: Identifier::from_str(s) }
    }
}

impl GenType for TypeId {
    fn gen_type(&self, _: &mut TypeEquations) -> TResult {
        Ok(Type::Type(TypeSpec::TypeId(self.clone())))
    }
}

impl Transpile for TypeId {
    fn transpile(&self, _: &mut TypeAnnotation) -> String {
        match self.id.into_string().as_str() {
            "i64" => "std::int_fast64_t",
            "u64" => "std::uint_fast64_t",
            "bool" => "bool",
            s => s,
        }.to_string()
    }
}

pub fn parse_type_id(s: &str) -> IResult<&str, TypeId> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TypeId { id }))
}
