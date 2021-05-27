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
use crate::func_definition::*;

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

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &Vec<TypeSubst>) -> Type {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, method_id, subst)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, method_id, subst)
            }
        }
    }
}

#[derive(Debug)]
pub struct ImplDefinition {
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinition>,
}

impl ImplDefinition {
    pub fn get_impl_trait_pair(&self) -> (TraitId, SelectionCandidate) {
        (self.trait_id.clone(), SelectionCandidate::ImplCandidate(ImplCandidate {
            trait_id: self.trait_id.clone(),
            impl_ty: self.impl_ty.clone(),
            asso_defs: self.asso_defs.clone(),
            require_methods: self.require_methods.iter().map(|(id, func)| (id.clone(), func.get_func_info().1)).collect(),
        }))
    }

    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &mut TraitsInfo) -> Result<Vec<TypeSubst>, String> {
        let next_self_type = Some(self.impl_ty.gen_type(equs)?);
        let before_self_type = equs.set_self_type(next_self_type);
        let mut substs = Vec::new();
        for def in self.require_methods.values() {
            let mut subst = def.unify_definition(equs, trs)?;
            substs.append(&mut subst);
        }
        equs.set_self_type(before_self_type);
        Ok(substs)
    }
}

pub fn parse_impl_definition(s: &str) -> IResult<&str, ImplDefinition> {
    let (s, (_, _, trait_id, _, _, _, impl_ty, _, _, _, many_types, many_methods, _, _)) = 
        tuple((tag("impl"), space1, parse_trait_id,
            space1, tag("for"), space1, parse_type_spec,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, space0, char('='), space0, parse_type_spec, space0, char(';'), space0))),
            many0(tuple((parse_func_definition, space0))),
            space0, char('}')))(s)?;
    let asso_defs = many_types.into_iter().map(|(_, _, id, _, _, _, ty, _, _, _)| (id, ty)).collect();
    let require_methods = many_methods.into_iter().map(|(func, _)| (TraitMethodIdentifier { id: func.func_id.clone() }, func)).collect();
    Ok((s, ImplDefinition { trait_id, impl_ty, asso_defs, require_methods }))
}

impl Transpile for ImplDefinition {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        let impl_def = format!("template<> struct {}<{}>", self.trait_id.transpile(ta), self.impl_ty.transpile(ta));
        let asso_defs = self.asso_defs.iter().map(|(id, spec)| {
            format!("using {} = {};\n", id.transpile(ta), spec.transpile(ta))
        }).collect::<Vec<_>>().join(" ");
        let require_methods = self.require_methods.iter().map(|(_, def)| format!("static {}", def.transpile(ta))).collect::<Vec<_>>().join("\n\n");
        format!("{} {{\nstatic constexpr bool value = true;\n{}\n\n{}}};\n", impl_def, asso_defs, require_methods)
    }
}


#[derive(Debug, Clone)]
pub struct ImplCandidate {
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
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

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, method_id: &TraitMethodIdentifier, subst: &Vec<TypeSubst>) -> Type {
        self.require_methods.get(&method_id).unwrap().generate_type(equs, &method_id.id).unwrap()
    }
}


#[derive(Debug, Clone)]
pub struct ParamCandidate {
    pub trait_id: TraitId,
    pub impl_ty: Type,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, Type>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}

impl ParamCandidate {
    pub fn new(trait_id: TraitId, impl_ty: Type, asso_defs: HashMap<AssociatedTypeIdentifier, Type>, require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>) -> SelectionCandidate {
        SelectionCandidate::ParamCandidate(ParamCandidate {
            trait_id, impl_ty, asso_defs, require_methods,
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

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, method_id: &TraitMethodIdentifier, subst: &Vec<TypeSubst>) -> Type {
        let before_self_type = equs.set_self_type(Some(self.impl_ty.clone()));
        let method_type = self.require_methods.get(method_id).unwrap().generate_type(equs, &method_id.id).unwrap();
        equs.set_self_type(before_self_type);
        method_type
    }
}
