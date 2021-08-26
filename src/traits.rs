use std::collections::HashMap;

pub mod associated_type;
pub use associated_type::*;

pub mod candidate;
pub use candidate::*;

pub mod trait_method;
pub use trait_method::*;


use nom::bytes::complete::*;
use nom::character::complete::*;
//use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
//use crate::unary_expr::Variable;
use crate::unify::where_section::*;
use crate::trans::*;
use crate::func_definition::*;

pub const BINARY_OPERATOR_TRAITS : [(&'static str, (&'static str, &'static str)); 10] = [
            ("BitOr", ("bit_or", "|")), ("BitXor", ("bit_xor", "^")), ("BitAnd", ("bit_and", "&")),
            ("Shl", ("shl", "<<")), ("Shr", ("shr", ">>")), ("Add", ("add", "+")),
            ("Sub", ("sub", "-")), ("Mul", ("mul", "*")), ("Div", ("div", "/")), ("Rem", ("rem", "%"))
        ];

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TraitId {
    pub id: Identifier,
}

impl TraitId {
    pub fn from_str(s: &str) -> TraitId {
        TraitId { id: Identifier::from_str(s) }
    }
}

impl Transpile for TraitId {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        self.id.into_string()
    }
}

pub fn parse_trait_id(s: &str) -> IResult<&str, TraitId> {
    let (s, id) = parse_identifier(s)?;
    Ok((s, TraitId { id }))
}

#[derive(Debug, Clone)]
pub struct TraitDefinition {
    pub trait_id: TraitId,
    pub where_sec: WhereSection,
    pub asso_ids: Vec<AssociatedTypeIdentifier>,
    pub required_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}

#[derive(Debug, Clone)]
pub struct TraitDefinitionInfo {
    pub trait_id: TraitId,
    pub where_sec: WhereSection,
    pub asso_ids: Vec<AssociatedTypeIdentifier>,
    pub required_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}

impl TraitDefinition {
    pub fn get_trait_id_pair(&self) -> (TraitId, TraitDefinitionInfo) {
        (self.trait_id.clone(), TraitDefinitionInfo {
            trait_id: self.trait_id.clone(),
            where_sec: self.where_sec.clone(),
            asso_ids: self.asso_ids.clone(),
            required_methods: self.required_methods.clone(),
        })
    }
}

impl Transpile for TraitDefinition {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        format!("template<class Self, class = void> struct {}: std::false_type {{ }};\n", self.trait_id.transpile(ta))
    }
}

pub fn parse_trait_definition(s: &str) -> IResult<&str, TraitDefinition> {
    let (s, (_, _, trait_id, _, where_sec, _, _, _, many_types, many_methods, _, _)) = 
        tuple((tag("trait"), space1, parse_trait_id,
            multispace0, parse_where_section, multispace0, char('{'), multispace0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, multispace0, char(';'), multispace0))),
            many0(tuple((parse_func_definition_info, multispace0, char(';'), multispace0))),
            multispace0, char('}')))(s)?;
    let asso_ids = many_types.into_iter().map(|(_, _, id, _, _, _)| id).collect();
    let required_methods = many_methods.into_iter().map(|(info, _, _, _)| (TraitMethodIdentifier { id: info.func_id.clone() }, info)).collect();
    Ok((s, TraitDefinition { trait_id, where_sec, asso_ids, required_methods }))
}



#[test]
fn parse_trait_definition_test() {
    log::debug!("{:?}", parse_trait_definition("trait MyTrait { type Output; type Input; }"));
}
