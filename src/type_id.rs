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
    pub fn check_typeid(self, _trs: &TraitsInfo) -> TResult {
        unreachable!("TypeId::check_typeid");
        // trs.check_typeid_exist(&self)
    }
}

impl GenType for TypeId {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        unreachable!("TypeID gen_type {:?}", self);
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
