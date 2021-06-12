use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::type_id::*;
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
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<(SubstsMap, &Self)> {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
        }
    }
    pub fn get_associated_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, subst: &SubstsMap) -> Type {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.get_associated_from_id(equs, trs, asso_id, subst)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.get_associated_from_id(equs, trs, asso_id, subst)
            }
        }
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap) -> Type {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, trs, method_id, subst)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, trs, method_id, subst)
            }
        }
    }

    pub fn get_trait_id(&self) -> TraitId {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.get_trait_id()
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.get_trait_id()
            }
        }
    }
}

#[derive(Debug)]
pub struct ImplDefinition {
    pub generics: Vec<TypeId>,
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinition>,
}

impl ImplDefinition {
    pub fn get_impl_trait_pair(&self) -> (TraitId, SelectionCandidate) {
        (self.trait_id.clone(), SelectionCandidate::ImplCandidate(ImplCandidate {
            generics: self.generics.clone(),
            trait_id: self.trait_id.clone(),
            impl_ty: self.impl_ty.clone(),
            where_sec: self.where_sec.clone(),
            asso_defs: self.asso_defs.clone(),
            require_methods: self.require_methods.iter().map(|(id, func)| (id.clone(), func.get_func_info().1)).collect(),
        }))
    }

    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<Vec<TypeSubst>, String> {
        let mut trs = trs.into_scope();
        for ty_id in self.generics.iter() {
            trs.regist_generics_type(ty_id)?;
        }
        self.where_sec.regist_candidate(equs, &mut trs)?;
        let next_self_type = Some(self.impl_ty.generate_type_no_auto_generics(equs, &trs)?);
        let before_self_type = equs.set_self_type(next_self_type);
        let mut substs = Vec::new();
        for def in self.require_methods.values() {
            let mut subst = def.unify_definition(equs, &trs)?;
            substs.append(&mut subst);
        }
        equs.set_self_type(before_self_type);
        Ok(substs)
    }
}

fn parse_generics_args(s: &str) -> IResult<&str, Vec<TypeId>> {
    let (s, op) = opt(tuple((space0, char('<'), space0, separated_list0(tuple((space0, char(','), space0)), parse_type_id), space0, char('>'))))(s)?;
    Ok((s, op.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new())))
}

pub fn parse_impl_definition(s: &str) -> IResult<&str, ImplDefinition> {
    let (s, (_, generics, _, trait_id, _, _, _, impl_ty, _, where_sec, _, _, _, many_types, many_methods, _, _)) = 
        tuple((tag("impl"), parse_generics_args,
            space1, parse_trait_id,
            space1, tag("for"), space1, parse_type_spec,
            space0, parse_where_section,
            space0, char('{'), space0,
            many0(tuple((tag("type"), space1, parse_associated_type_identifier, space0, char('='), space0, parse_type_spec, space0, char(';'), space0))),
            many0(tuple((parse_func_definition, space0))),
            space0, char('}')))(s)?;
    let asso_defs = many_types.into_iter().map(|(_, _, id, _, _, _, ty, _, _, _)| (id, ty)).collect();
    let require_methods = many_methods.into_iter().map(|(func, _)| (TraitMethodIdentifier { id: func.func_id.clone() }, func)).collect();
    Ok((s, ImplDefinition { generics, trait_id, impl_ty, where_sec, asso_defs, require_methods }))
}

impl Transpile for ImplDefinition {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta))).collect::<Vec<_>>().join(", ");
        let impl_def = format!("template<{}> struct {}<{}>", generics, self.trait_id.transpile(ta), self.impl_ty.transpile(ta));
        let asso_defs = self.asso_defs.iter().map(|(id, spec)| {
            format!("using {} = {};\n", id.transpile(ta), spec.transpile(ta))
        }).collect::<Vec<_>>().join(" ");
        let require_methods = self.require_methods.iter().map(|(_, def)| format!("static {}", def.transpile(ta))).collect::<Vec<_>>().join("\n\n");
        format!("{} {{\nstatic constexpr bool value = true;\nusing Self = {};\n{}\n\n\n{}}};\n", impl_def, self.impl_ty.transpile(ta), asso_defs, require_methods)
    }
}


#[derive(Debug, Clone)]
pub struct ImplCandidate {
    pub generics: Vec<TypeId>,
    pub trait_id: TraitId,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}


impl ImplCandidate {
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), self.trait_id.id.generate_type_variable(i)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut equs, trs).unwrap();
        equs.add_equation(ty.clone(), impl_ty);
        if self.where_sec.regist_equations(&gen_mp, &mut equs, trs).is_ok() {
            println!("match impl unify for {:?}", self.trait_id);
            equs.unify(trs).ok().map(|res| SubstsMap::new(res))
        }
        else {
            None
        }
    }

    pub fn get_associated_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, subst: &SubstsMap) -> Type {
        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get(&self.trait_id.id, i)?)))
            .collect::<Result<HashMap<_, _>, String>>().unwrap();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        self.asso_defs.get(asso_id).unwrap().generics_to_type(&gen_mp, equs, trs).unwrap()
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap) -> Type {
        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get(&self.trait_id.id, i)?)))
            .collect::<Result<HashMap<_, _>, String>>().unwrap();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        self.require_methods.get(&method_id).unwrap().generate_type(&gen_mp, equs, trs, &method_id.id).unwrap()
    }

    pub fn get_trait_id(&self) -> TraitId {
        self.trait_id.clone()
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
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();
        let alpha = self.trait_id.id.generate_type_variable(0);
        equs.add_equation(self.impl_ty.clone(), alpha.clone());
        equs.add_equation(ty.clone(), alpha);
        equs.unify(trs).ok().map(|res| SubstsMap::new(res))
    }
    pub fn get_associated_from_id(&self, _equs: &mut TypeEquations, _trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, _subst: &SubstsMap) -> Type {
        self.asso_defs.get(asso_id).unwrap().clone()
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap) -> Type {
        let before_self_type = equs.set_self_type(Some(subst.get(&self.trait_id.id, 0).unwrap()));
        let method_type = self.require_methods.get(method_id).unwrap().generate_type(&GenericsTypeMap::empty(), equs, trs, &method_id.id).unwrap();
        equs.set_self_type(before_self_type);
        method_type
    }

    pub fn get_trait_id(&self) -> TraitId {
        self.trait_id.clone()
    }
}
