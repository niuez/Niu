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
use crate::error::*;

pub const BINARY_OPERATOR_TRAITS : [(&'static str, (&'static str, &'static str)); 10] = [
            ("BitOr", ("operator|", "|")), ("BitXor", ("operator^", "^")), ("BitAnd", ("operator&", "&")),
            ("Shl", ("operator<<", "<<")), ("Shr", ("operator>>", ">>")), ("Add", ("operator+", "+")),
            ("Sub", ("operator-", "-")), ("Mul", ("operator*", "*")), ("Div", ("operator/", "/")), ("Rem", ("operator%", "%"))
        ];
pub const UNARY_OPERATOR_TRAITS : [(&'static str, (&'static str, &'static str)); 2] = [
            ("Neg", ("operator-", "-")), ("Not", ("operator!", "!")),
        ];
pub enum ResultFindOperator {
    Binary((&'static str, &'static str)),
    Unary((&'static str, &'static str)),
    Eq,
    Ord,
    Clone,
    Copy,
}

/* pub fn find_binary_operator(tr: &str) -> Option<(&'static str, &'static str)> {
    BINARY_OPERATOR_TRAITS.iter().find_map(|(tr_id, val)|
        if *tr_id == tr { Some(*val) }
        else { None }
    )
} */

pub fn find_operator(tr: &str) -> Option<ResultFindOperator> {
    BINARY_OPERATOR_TRAITS.iter().map(|(tr, val)| (tr, ResultFindOperator::Binary(*val)))
        .chain(
            UNARY_OPERATOR_TRAITS.iter().map(|(tr, val)| (tr, ResultFindOperator::Unary(*val)))
        )
        .chain(std::iter::once(
            (&"Eq", ResultFindOperator::Eq)
        ))
        .chain(std::iter::once(
            (&"Ord", ResultFindOperator::Ord)
        ))
        .chain(std::iter::once(
            (&"Clone", ResultFindOperator::Clone)
        ))
        .chain(std::iter::once(
            (&"Copy", ResultFindOperator::Copy)
        ))
        .find_map(|(tr_id, val)|
                  if *tr_id == tr { Some(val) }
                  else { None }
        )
}

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TraitSpec {
    pub trait_id: TraitId,
    pub generics: Vec<TypeSpec>,
}

impl TraitSpec {
    pub fn get_tag(&self) -> Tag {
        self.trait_id.id.tag.clone()
    }
    pub fn generate_trait_generics(&self, equs: &mut TypeEquations, trs: &TraitsInfo, gen_mp: &GenericsTypeMap) -> Result<TraitGenerics, Box<dyn NiuError>> {
        trs.check_trait(self)?;
        let generics = self.generics.iter().map(|g| g.generics_to_type(gen_mp, equs, trs)).collect::<Result<Vec<_>, _>>()?;
        Ok(TraitGenerics { trait_id: self.trait_id.clone(), generics })
    }
    pub fn generate_trait_generics_with_no_map(&self, equs: &TypeEquations, trs: &TraitsInfo) -> Result<TraitGenerics, Box<dyn NiuError>> {
        trs.check_trait(self)?;
        let generics = self.generics.iter().map(|g| g.generate_type_no_auto_generics(equs, trs)).collect::<Result<Vec<_>, _>>()?;
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
        match find_operator(self.trait_id.id.into_string().as_str()) {
            None | Some(ResultFindOperator::Clone) => {
                let generics = self.generics.iter().map(|g| format!(", class {}", g.transpile(ta))).collect::<Vec<_>>().join("");
                format!("template<class Self{}, class = void> struct {}: std::false_type {{ }};\n", generics, self.trait_id.transpile(ta))
            }
            Some(_) => {
                format!("")
            }
        }
    }
}

pub fn parse_trait_definition(s: &str) -> IResult<&str, TraitDefinition> {
    let (s, (_, _, trait_id, _, generics, _, where_sec, _, _, _, many_types, many_methods, _, _)) = 
        tuple((tag("trait"), multispace1, parse_trait_id,
            multispace0, parse_generics_args,
            multispace0, parse_where_section, multispace0, char('{'), multispace0,
            many0(tuple((tag("type"), multispace1, parse_associated_type_identifier, multispace0, char(';'), multispace0))),
            many0(tuple((parse_func_definition_info, multispace0, char(';'), multispace0))),
            multispace0, char('}')))(s)?;
    let asso_ids = many_types.into_iter().map(|(_, _, id, _, _, _)| id).collect();
    //let required_methods = many_methods.into_iter().map(|(info, _, _, _)| (TraitMethodIdentifier { id: info.func_id.clone() }, info)).collect();
    let required_methods = match find_operator(trait_id.id.into_string().as_str()) {
        None | Some(ResultFindOperator::Clone) | Some(ResultFindOperator::Copy) => {
            many_methods.into_iter().map(|(func, _, _, _)| (TraitMethodIdentifier { id: func.func_id.clone() }, func)).collect()
        }
        Some(ResultFindOperator::Unary((_, ope))) | Some(ResultFindOperator::Binary((_, ope))) => {
            many_methods.into_iter().map(|(mut func, _, _, _)| {
                func.func_id = Identifier::from_str(format!("operator{}", ope).as_str());
                (TraitMethodIdentifier { id: func.func_id.clone() }, func)
            }).collect()
        }
        Some(ResultFindOperator::Eq) => {
            many_methods.into_iter().map(|(mut func, _, _, _)| {
                func.func_id = Identifier::from_str("operator==");
                (TraitMethodIdentifier { id: func.func_id.clone() }, func)
            }).collect()
        }
        Some(ResultFindOperator::Ord) => {
            many_methods.into_iter().map(|(mut func, _, _, _)| {
                func.func_id = Identifier::from_str("operator<");
                (TraitMethodIdentifier { id: func.func_id.clone() }, func)
            }).collect()
        }
    };
    Ok((s, TraitDefinition { trait_id, generics, where_sec, asso_ids, required_methods }))
}



#[test]
fn parse_trait_definition_test() {
    log::debug!("{:?}", parse_trait_definition("trait MyTrait { type Output; type Input; }").ok());
}
