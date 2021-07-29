use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::*;
use crate::type_id::*;
use crate::type_spec::*;
use crate::unify::*;
//use crate::unary_expr::Variable;
use crate::trans::*;
use crate::mut_checker::*;
use crate::traits::*;
use crate::func_definition::*;
use crate::structs::*;


#[derive(Debug, Clone)]
pub enum SelectionCandidate {
    ImplCandidate(ImplCandidate),
    ParamCandidate(ParamCandidate),
    ImplSelfCandidate(ImplSelfCandidate),
}

impl SelectionCandidate {
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<TypeEquations, String> {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.generate_equations_for_call_equation(call_eq, trs)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.generate_equations_for_call_equation(call_eq, trs)
            }
            SelectionCandidate::ImplSelfCandidate(ref cand) => {
                cand.generate_equations_for_call_equation(call_eq, trs)
            }
        }
    }
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<(SubstsMap, &Self)> {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
            SelectionCandidate::ImplSelfCandidate(ref cand) => {
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
            SelectionCandidate::ImplSelfCandidate(_) => {
                unreachable!("ImplSelfCandidate doesnt have associated type")
            }
        }
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, trs, method_id, subst, ty)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, trs, method_id, subst, ty)
            }
            SelectionCandidate::ImplSelfCandidate(ref cand) => {
                cand.get_trait_method_from_id(equs, trs, method_id, subst, ty)
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
            SelectionCandidate::ImplSelfCandidate(_) => {
                unreachable!("cant get trait_id from ImplSelfCandidate")
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

    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), String> {
        let mut trs = trs.into_scope();
        for ty_id in self.generics.iter() {
            trs.regist_generics_type(ty_id)?;
        }
        self.where_sec.regist_candidate(equs, &mut trs)?;
        let next_self_type = Some(self.impl_ty.generate_type_no_auto_generics(equs, &trs)?);
        let before_self_type = equs.set_self_type(next_self_type);
        for def in self.require_methods.values() {
            def.unify_definition(equs, &trs)?;
        }
        equs.set_self_type(before_self_type);
        Ok(())
    }

    pub fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<(), String> {
        for def in self.require_methods.values() {
            def.mut_check(ta, vars)?;
        }
        Ok(())
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
        let where_str = self.where_sec.transpile(ta);
        let impl_def = if where_str == "" {
            format!("template<{}> struct {}<{}>", generics, self.trait_id.transpile(ta), self.impl_ty.transpile(ta))
        }
        else {
            format!("template<{}> struct {}<{}, {}>", generics, self.trait_id.transpile(ta), self.impl_ty.transpile(ta), where_str)
        };
        let asso_defs = self.asso_defs.iter().map(|(id, spec)| {
            format!("using {} = {};\n", id.transpile(ta), spec.transpile(ta))
        }).collect::<Vec<_>>().join(" ");
        let require_methods = self.require_methods.iter().map(|(_, def)| {
            let def_str = def.transpile(ta);
            if def_str.is_empty() { format!("") }
            else { format!("static {}", def.transpile(ta)) }
        }).collect::<Vec<_>>().join("\n\n");
        format!("{} {{\nstatic constexpr bool value = true;\nusing Self = {};\n{}\n{}}};\n", impl_def, self.impl_ty.transpile(ta), asso_defs, require_methods)
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
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<TypeEquations, String> {
        if let Some(trait_id) = &call_eq.trait_id {
            if *trait_id != self.trait_id.clone() {
                return Err(format!("trait_id is not matched"))
            }
        }
        let mut equs = TypeEquations::new();
        let self_type = call_eq.tag.generate_type_variable("SelfType", 0, &mut equs);
        equs.set_self_type(Some(self_type.clone()));

        if let Some(ref caller_type) = call_eq.caller_type {
            equs.add_equation(self_type.clone(), caller_type.as_ref().clone());
        }

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), call_eq.tag.generate_type_variable("Generics", i, &mut equs)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut equs, trs).unwrap();
        equs.add_equation(impl_ty, self_type.clone());
        self.where_sec.regist_equations(&gen_mp, &mut equs, trs)?;
        let func_ty = self.require_methods
            .get(&TraitMethodIdentifier { id: call_eq.func_id.clone() })
            .ok_or(format!("require methods doesnt have {:?}", call_eq.func_id))?
            .generate_type(&gen_mp, &mut equs, trs, &call_eq.func_id)?;
        match func_ty {
            Type::Func(args, ret, info) => {
                let alpha = call_eq.tag.generate_type_variable("FuncTypeInfo", 0, &mut equs);
                let info = match info {
                    FuncTypeInfo::None => {
                        let tag = Tag::new();
                        let alpha = tag.generate_type_variable("SelfType", 0, &mut equs);
                        equs.add_equation(alpha, self_type.clone());
                        FuncTypeInfo::TraitFunc(self.trait_id.clone(), tag)
                    }
                    info => info,
                };
                equs.add_equation(alpha, Type::Func(args.clone(), ret.clone(), info));

                if args.len() != call_eq.args.len() {
                    return Err(format!("args len is not matched"))
                }
                for (l, r) in args.into_iter().zip(call_eq.args.iter()) {
                    equs.add_equation(l, r.clone())
                }
                let return_ty = call_eq.tag.generate_type_variable("ReturnType", 0, &mut equs);
                equs.add_equation(*ret, return_ty);
            }
            _ => unreachable!()
        }
        Ok(equs)
    }

    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();
        equs.set_self_type(Some(ty.clone()));

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), self.trait_id.id.generate_type_variable("Generics", i, &mut equs)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut equs, trs).unwrap();
        equs.add_equation(ty.clone(), impl_ty);
        if self.where_sec.regist_equations(&gen_mp, &mut equs, trs).is_ok() {
            equs.unify(trs).ok().map(|_| SubstsMap::new(equs.take_substs()))
        }
        else {
            None
        }
    }

    pub fn get_associated_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, subst: &SubstsMap) -> Type {
        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get(&self.trait_id.id, "Generics", i)?)))
            .collect::<Result<HashMap<_, _>, String>>().unwrap();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        self.asso_defs.get(asso_id).unwrap().generics_to_type(&gen_mp, equs, trs).unwrap()
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get(&self.trait_id.id, "Generics", i)?)))
            .collect::<Result<HashMap<_, _>, String>>().unwrap();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        let before_self_type = equs.set_self_type(Some(ty.clone()));
        let func_ty = self.require_methods.get(&method_id).unwrap().generate_type(&gen_mp, equs, trs, &method_id.id).unwrap();
        let res = match func_ty {
            Type::Func(args, ret, FuncTypeInfo::None) => {
                let tag = Tag::new();
                let alpha = tag.generate_type_variable("SelfType", 0, equs);
                equs.add_equation(alpha, ty.clone());
                Type::Func(args, ret, FuncTypeInfo::TraitFunc(self.trait_id.clone(), tag))
            }
            func_ty => func_ty
        };
        equs.set_self_type(before_self_type);
        res
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
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<TypeEquations, String> {
        if let Some(trait_id) = &call_eq.trait_id {
            if *trait_id != self.trait_id.clone() {
                return Err(format!("trait_id is not matched"))
            }
        }
        let mut equs = TypeEquations::new();
        let self_type = call_eq.tag.generate_type_variable("SelfType", 0, &mut equs);
        equs.set_self_type(Some(self_type.clone()));

        if let Some(ref caller_type) = call_eq.caller_type {
            equs.add_equation(self_type.clone(), caller_type.as_ref().clone());
        }

        equs.add_equation(self.impl_ty.clone(), self_type.clone());
        let func_ty = self.require_methods
            .get(&TraitMethodIdentifier { id: call_eq.func_id.clone() })
            .ok_or(format!("require methods doesnt have {:?}", call_eq.func_id))?
            .generate_type(&GenericsTypeMap::empty(), &mut equs, trs, &call_eq.func_id)?;
        match func_ty {
            Type::Func(args, ret, info) => {
                let alpha = call_eq.tag.generate_type_variable("FuncTypeInfo", 0, &mut equs);
                let info = match info {
                    FuncTypeInfo::None => {
                        let tag = Tag::new();
                        let alpha = tag.generate_type_variable("SelfType", 0, &mut equs);
                        equs.add_equation(alpha, self_type.clone());
                        FuncTypeInfo::TraitFunc(self.trait_id.clone(), tag)
                    }
                    info => info,
                };
                equs.add_equation(alpha, Type::Func(args.clone(), ret.clone(), info));
                if args.len() != call_eq.args.len() {
                    return Err(format!("args len is not matched"))
                }
                for (l, r) in args.into_iter().zip(call_eq.args.iter()) {
                    equs.add_equation(l, r.clone())
                }
                let return_ty = call_eq.tag.generate_type_variable("ReturnType", 0, &mut equs);
                equs.add_equation(*ret, return_ty);
            }
            _ => unreachable!()
        }
        Ok(equs)
    }

    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();
        let alpha = self.trait_id.id.generate_type_variable("ImplType", 0, &mut equs);
        equs.add_equation(self.impl_ty.clone(), alpha.clone());
        equs.add_equation(ty.clone(), alpha);
        equs.unify(trs).ok().map(|_| SubstsMap::new(equs.take_substs()))
    }
    pub fn get_associated_from_id(&self, _equs: &mut TypeEquations, _trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, _subst: &SubstsMap) -> Type {
        self.asso_defs.get(asso_id).unwrap().clone()
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        let before_self_type = equs.set_self_type(Some(subst.get(&self.trait_id.id, "ImplType", 0).unwrap()));
        let func_ty = self.require_methods.get(method_id).unwrap().generate_type(&GenericsTypeMap::empty(), equs, trs, &method_id.id).unwrap();
        let res = match func_ty {
            Type::Func(args, ret, FuncTypeInfo::None) => {
                let tag = Tag::new();
                let alpha = tag.generate_type_variable("SelfType", 0, equs);
                equs.add_equation(alpha, ty.clone());
                Type::Func(args, ret, FuncTypeInfo::TraitFunc(self.trait_id.clone(), tag))
            }
            func_ty => func_ty
        };
        equs.set_self_type(before_self_type);
        res
    }

    pub fn get_trait_id(&self) -> TraitId {
        self.trait_id.clone()
    }
}
