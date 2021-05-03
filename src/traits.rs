pub mod associated_type;
pub use associated_type::*;

use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
//use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_spec::*;
use crate::unify::*;
//use crate::unary_expr::Variable;
use crate::trans::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TraitId {
    pub id: Identifier,
}

impl Transpile for TraitId {
    fn transpile(&self, _: &mut TypeAnnotation) -> String {
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
    pub asso_ids: Vec<AssociatedTypeIdentifier>,
}

impl TraitDefinition {
    pub fn get_trait_id_pair(&self) -> (TraitId, TraitDefinition) {
        (self.trait_id.clone(), self.clone())
    }
}

pub fn parse_trait_definition(s: &str) -> IResult<&str, TraitDefinition> {
    let (s, (_, _, trait_id, _, _, _, many_types, _, _)) = 
        tuple((tag("trait"), space1, parse_trait_id,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, space0, char(';'), space0))),
            space0, char('}')))(s)?;
    let asso_ids = many_types.into_iter().map(|(_, _, id, _, _, _)| id).collect();
    Ok((s, TraitDefinition { trait_id, asso_ids }))
}

#[derive(Debug, Clone)]
pub enum SelectionCandidate {
    ImplCandidate(ImplCandidate),
    ParamCandidate(ParamCandidate),
}

impl SelectionCandidate {
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<(Vec<TypeSubst>, &Self)> {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
        }
    }
    pub fn get_associated_from_id(&self, equs: &mut TypeEquations, asso_id: &AssociatedTypeIdentifier, subst: &Vec<TypeSubst>) -> Type {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.get_associated_from_id(equs, asso_id, subst)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.get_associated_from_id(equs, asso_id, subst)
            }
        }
    }

}

#[derive(Debug, Clone)]
pub struct ImplCandidate {
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
}


impl ImplCandidate {
    pub fn get_impl_trait_pair(&self) -> (TraitId, SelectionCandidate) {
        (self.trait_id.clone(), SelectionCandidate::ImplCandidate(self.clone()))
    }
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<Vec<TypeSubst>> {
        let mut equs = TypeEquations::new();
        let impl_ty = self.impl_ty.gen_type(&mut equs).unwrap();
        equs.add_equation(ty.clone(), impl_ty);
        equs.unify(trs).ok()
    }

    pub fn get_associated_from_id(&self, equs: &mut TypeEquations, asso_id: &AssociatedTypeIdentifier, _subst: &Vec<TypeSubst>) -> Type {
        self.asso_defs.get(asso_id).unwrap().gen_type(equs).unwrap()
    }
}

pub fn parse_impl_candidate(s: &str) -> IResult<&str, ImplCandidate> {
    let (s, (_, _, trait_id, _, _, _, impl_ty, _, _, _, many_types, _, _)) = 
        tuple((tag("impl"), space1, parse_trait_id,
            space1, tag("for"), space1, parse_type_spec,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, space0, char('='), space0, parse_type_spec, space0, char(';'), space0))),
            space0, char('}')))(s)?;
    let asso_defs = many_types.into_iter().map(|(_, _, id, _, _, _, ty, _, _, _)| (id, ty)).collect();
    Ok((s, ImplCandidate { trait_id, impl_ty, asso_defs }))
}

#[derive(Debug, Clone)]
pub struct ParamCandidate {
    pub trait_id: TraitId,
    pub impl_ty: Type,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, Type>,
}

impl ParamCandidate {
    pub fn new(trait_id: TraitId, impl_ty: Type, asso_defs: HashMap<AssociatedTypeIdentifier, Type>) -> SelectionCandidate {
        SelectionCandidate::ParamCandidate(ParamCandidate {
            trait_id, impl_ty, asso_defs,
        })
    }
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<Vec<TypeSubst>> {
        if self.impl_ty == *ty {
            Some(Vec::new())
        }
        else {
            None
        }
    }
    pub fn get_associated_from_id(&self, _equs: &mut TypeEquations, asso_id: &AssociatedTypeIdentifier, _subst: &Vec<TypeSubst>) -> Type {
        self.asso_defs.get(asso_id).unwrap().clone()
    }
}


#[test]
fn parse_trait_definition_test() {
    println!("{:?}", parse_trait_definition("trait MyTrait { type Output; type Input; }"));
}
