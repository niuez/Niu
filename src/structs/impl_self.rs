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
use crate::traits::*;
use crate::func_definition::*;

#[derive(Debug)]
pub struct ImplSelfDefinition {
    pub generics: Vec<TypeId>,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub require_methods: HashMap<Identifier, FuncDefinition>,
    tag: Tag,
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
    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<Vec<TypeSubst>, String> {
        let mut trs = trs.into_scope();
        for ty_id in self.generics.iter() {
            trs.regist_generics_type(ty_id)?;
        }
        self.where_sec.regist_candidate(equs, &mut trs)?;
        let next_self_type = self.impl_ty.generate_type_no_auto_generics(equs, &trs)?;
        let next_self_type = Some(next_self_type);
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

impl ImplSelfCandidate {
    pub fn match_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();
        equs.set_self_type(Some(ty.clone()));

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), self.tag.generate_type_variable(i, &mut equs)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut equs, trs).unwrap();
        equs.add_equation(ty.clone(), impl_ty);
        if self.where_sec.regist_equations(&gen_mp, &mut equs, trs).is_ok() {
            println!("match impl unify for ImplSelfCandidate {:?}", ty);
            equs.unify(trs).ok().map(|res| SubstsMap::new(res))
        }
        else {
            None
        }
    }
    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        let gen_vec = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get_from_tag(&self.tag, i)?)))
            .collect::<Result<Vec<_>, String>>().unwrap();
        let gen_hashmp = gen_vec.iter().cloned().collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_hashmp);
        let before_self_type = equs.set_self_type(Some(ty.clone()));
        let func_ty = self.require_methods.get(&method_id.id).unwrap().generate_type(&gen_mp, equs, trs, &method_id.id).unwrap();
        let res = match func_ty {
            Type::Func(args, ret, FuncTypeInfo::None) => {
                let tag = Tag::new();
                let len = gen_vec.len();
                for (i, gen) in gen_vec.into_iter().enumerate() {
                    let alpha = tag.generate_type_variable(i, equs);
                    equs.add_equation(alpha, gen.1);
                }
                Type::Func(args, ret, FuncTypeInfo::SelfFunc(self.impl_ty.get_type_id().unwrap(), tag, len))
            }
            func_ty => func_ty
        };
        equs.set_self_type(before_self_type);
        res
    }
}

fn parse_generics_args(s: &str) -> IResult<&str, Vec<TypeId>> {
    let (s, op) = opt(tuple((space0, char('<'), space0, separated_list0(tuple((space0, char(','), space0)), parse_type_id), space0, char('>'))))(s)?;
    Ok((s, op.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new())))
}

pub fn parse_impl_self_definition(s: &str) -> IResult<&str, ImplSelfDefinition> {
    let (s, (_, generics, _, impl_ty, _, where_sec, _, _, _, many_methods, _, _)) = 
        tuple((tag("impl"), parse_generics_args,
            space1, parse_type_spec,
            space0, parse_where_section,
            space0, char('{'), space0,
            many0(tuple((parse_func_definition, space0))),
            space0, char('}')))(s)?;
    let require_methods = many_methods.into_iter().map(|(func, _)| (func.func_id.clone(), func)).collect();
    Ok((s, ImplSelfDefinition { generics, impl_ty, where_sec, require_methods, tag: Tag::new() }))
}
