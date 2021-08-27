use std::collections::HashMap;

pub mod associated_type;
pub use associated_type::*;

pub mod candidate;
pub use candidate::*;

pub mod trait_method;
pub use trait_method::*;


use nom::bytes::complete::*;
use nom::character::complete::*;
//use nom::branch::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier, Tag };
//use crate::unary_expr::Variable;
use crate::unify::where_section::*;
use crate::unify::*;
use crate::trans::*;
use crate::func_definition::*;
use crate::type_spec::*;
use crate::type_id::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TraitId {
    pub id: Identifier,
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
pub struct TraitSpec {
    pub trait_id: TraitId,
    pub generics: Vec<TypeSpec>,
}

impl TraitSpec {
    pub fn get_tag(&self) -> Tag {
        self.trait_id.id.tag.clone()
    }
    pub fn generate_trait_generics(&self, equs: &mut TypeEquations, trs: &TraitsInfo, gen_mp: &GenericsTypeMap) -> Result<TraitGenerics, String> {
        trs.check_trait(self)?;
        let generics = self.generics.iter().map(|g| g.generics_to_type(gen_mp, equs, trs)).collect::<Result<Vec<_>, String>>()?;
        Ok(TraitGenerics { trait_id: self.trait_id.clone(), generics })
    }
}

fn parse_generics_args(s: &str) -> IResult<&str, Vec<TypeId>> {
    let (s, op) = opt(tuple((multispace0, char('<'), multispace0, separated_list0(tuple((multispace0, char(','), multispace0)), parse_type_id), multispace0, char('>'))))(s)?;
    Ok((s, op.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new())))
}

pub fn parse_trait_spec(s: &str) -> IResult<&str, TraitSpec> {
    let (s, (trait_id, opts)) =
        tuple((parse_trait_id,
               opt(tuple((multispace0, char('<'), multispace0, separated_list0(tuple((multispace0, char(','), multispace0)), parse_type_spec), multispace0, char('>'))))))(s)?;
    let generics = opts.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new());
    Ok((s, TraitSpec { trait_id, generics }))
}

#[derive(Debug, Clone)]
pub struct TraitDefinition {
    pub trait_id: TraitId,
    pub generics: Vec<TypeId>,
    pub where_sec: WhereSection,
    pub asso_ids: Vec<AssociatedTypeIdentifier>,
    pub required_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}

#[derive(Debug, Clone)]
pub struct TraitDefinitionInfo {
    pub trait_id: TraitId,
    pub generics: Vec<TypeId>,
    pub where_sec: WhereSection,
    pub asso_ids: Vec<AssociatedTypeIdentifier>,
    pub required_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}

impl TraitDefinition {
    pub fn get_trait_id_pair(&self) -> (TraitId, TraitDefinitionInfo) {
        (self.trait_id.clone(), TraitDefinitionInfo {
            trait_id: self.trait_id.clone(),
            generics: self.generics.clone(),
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
    let (s, (_, _, trait_id, _, generics, _, where_sec, _, _, _, many_types, many_methods, _, _)) = 
        tuple((tag("trait"), space1, parse_trait_id,
            multispace0, parse_generics_args,
            multispace0, parse_where_section, multispace0, char('{'), multispace0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, multispace0, char(';'), multispace0))),
            many0(tuple((parse_func_definition_info, multispace0, char(';'), multispace0))),
            multispace0, char('}')))(s)?;
    let asso_ids = many_types.into_iter().map(|(_, _, id, _, _, _)| id).collect();
    let required_methods = many_methods.into_iter().map(|(info, _, _, _)| (TraitMethodIdentifier { id: info.func_id.clone() }, info)).collect();
    Ok((s, TraitDefinition { trait_id, generics, where_sec, asso_ids, required_methods }))
}



#[test]
fn parse_trait_definition_test() {
    log::debug!("{:?}", parse_trait_definition("trait MyTrait { type Output; type Input; }"));
}
