use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::unify::*;
use crate::trans::*;

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
    fn gen_type(&self, _equs: &mut TypeEquations, _trs: &TraitsInfo) -> TResult {
        unreachable!("TypeID gen_type {:?}", self);
    }
}

impl Transpile for TypeId {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        match self.id.into_string().as_str() {
            "i64" => "long long",
            "u64" => "unsigned long long",
            "f64" => "double",
            "bool" => "bool",
            "void" => "void",
            s => s,
        }.to_string()
    }
}

pub fn parse_type_id(s: &str) -> IResult<&str, TypeId> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TypeId { id }))
}
