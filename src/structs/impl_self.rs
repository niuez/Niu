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
use crate::mut_checker::*;
use crate::trans::*;
//use crate::unary_expr::Variable;
use crate::traits::*;
use crate::func_definition::*;

#[derive(Debug)]
pub struct ImplSelfDefinition {
    pub generics: Vec<TypeId>,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub require_methods: HashMap<Identifier, FuncDefinition>,
    pub tag: Tag,
}

#[derive(Debug, Clone)]
pub struct ImplSelfCandidate {
    pub generics: Vec<TypeId>,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub require_methods: HashMap<Identifier, FuncDefinitionInfo>,
    tag: Tag,
}

impl ImplSelfDefinition {
    pub fn get_info(&self) -> ImplSelfCandidate {
        ImplSelfCandidate {
            generics: self.generics.clone(),
            impl_ty: self.impl_ty.clone(),
            where_sec: self.where_sec.clone(),
            require_methods: self.require_methods.iter().map(|(id, func)| (id.clone(), func.get_func_info().1)).collect(),
            tag: self.tag.clone(),
        }
    }
    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), String> {
        let mut trs = trs.into_scope();
        for ty_id in self.generics.iter() {
            trs.regist_generics_type(ty_id)?;
        }
        self.where_sec.regist_candidate(equs, &mut trs)?;
        let next_self_type = self.impl_ty.generate_type_no_auto_generics(equs, &trs)?;
        let next_self_type = Some(next_self_type);
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

impl ImplSelfCandidate {
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<TypeEquations, String> {
        if call_eq.trait_id != None {
            return Err(format!("trait_id is not matched"))
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
            .get(&call_eq.func_id)
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
                        FuncTypeInfo::SelfFunc(tag)
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

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), self.tag.generate_type_variable("Generics", i, &mut equs)))
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
    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        let gen_vec = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get_from_tag(&self.tag, "Generics", i)?)))
            .collect::<Result<Vec<_>, String>>().unwrap();
        let gen_hashmp = gen_vec.iter().cloned().collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_hashmp);
        let before_self_type = equs.set_self_type(Some(ty.clone()));
        let func_ty = self.require_methods.get(&method_id.id).unwrap().generate_type(&gen_mp, equs, trs, &method_id.id).unwrap();
        let res = match func_ty {
            Type::Func(args, ret, FuncTypeInfo::None) => {
                let tag = Tag::new();
                for (i, gen) in gen_vec.into_iter().enumerate() {
                    let alpha = tag.generate_type_variable("Generics", i, equs);
                    equs.add_equation(alpha, gen.1);
                }
                Type::Func(args, ret, FuncTypeInfo::SelfFunc(tag))
            }
            func_ty => func_ty
        };
        equs.set_self_type(before_self_type);
        res
    }
}

fn parse_generics_args(s: &str) -> IResult<&str, Vec<TypeId>> {
    let (s, op) = opt(tuple((multispace0, char('<'), multispace0, separated_list0(tuple((multispace0, char(','), multispace0)), parse_type_id), multispace0, char('>'))))(s)?;
    Ok((s, op.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new())))
}

pub fn parse_impl_self_definition(s: &str) -> IResult<&str, ImplSelfDefinition> {
    let (s, (_, generics, _, impl_ty, _, where_sec, _, _, _, many_methods, _, _)) = 
        tuple((tag("impl"), parse_generics_args,
            space1, parse_type_spec,
            multispace0, parse_where_section,
            multispace0, char('{'), multispace0,
            many0(tuple((parse_func_definition, multispace0))),
            multispace0, char('}')))(s)?;
    let require_methods = many_methods.into_iter().map(|(func, _)| (func.func_id.clone(), func)).collect();
    Ok((s, ImplSelfDefinition { generics, impl_ty, where_sec, require_methods, tag: Tag::new() }))
}
