use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
//use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::type_spec::*;
use crate::unify::*;
//use crate::unary_expr::Variable;
use crate::trans::*;
use crate::traits::*;

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
pub struct ImplDefinition {
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
}

impl ImplDefinition {
    pub fn get_impl_trait_pair(&self) -> (TraitId, SelectionCandidate) {
        (self.trait_id.clone(), SelectionCandidate::ImplCandidate(ImplCandidate {
            trait_id: self.trait_id.clone(),
            impl_ty: self.impl_ty.clone(),
            asso_defs: self.asso_defs.clone(),
        }))
    }
}

#[derive(Debug, Clone)]
pub struct ImplCandidate {
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
}

impl Transpile for ImplDefinition {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        let impl_def = format!("template<> class {}<{}>", self.trait_id.transpile(ta), self.impl_ty.transpile(ta));
        let asso_defs = self.asso_defs.iter().map(|(id, spec)| {
            format!("using {} = {};\n", id.transpile(ta), spec.transpile(ta))
        }).collect::<Vec<_>>().join(" ");
        format!("{} {{\n static constexpr bool value = true;\n{}}};\n", impl_def, asso_defs)
    }
}

impl ImplCandidate {
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


pub fn parse_impl_definition(s: &str) -> IResult<&str, ImplDefinition> {
    let (s, (_, _, trait_id, _, _, _, impl_ty, _, _, _, many_types, _, _)) = 
        tuple((tag("impl"), space1, parse_trait_id,
            space1, tag("for"), space1, parse_type_spec,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, space0, char('='), space0, parse_type_spec, space0, char(';'), space0))),
            space0, char('}')))(s)?;
    let asso_defs = many_types.into_iter().map(|(_, _, id, _, _, _, ty, _, _, _)| (id, ty)).collect();
    Ok((s, ImplDefinition { trait_id, impl_ty, asso_defs }))
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
        println!("{:?}, {:?}", self, ty);
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
