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
}

#[derive(Debug, Clone)]
pub struct ImplSelfDefinitionInfo {
    pub generics: Vec<TypeId>,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub require_methods: HashMap<Identifier, FuncDefinitionInfo>,
}

impl ImplSelfDefinition {
    pub fn get_info(&self) -> ImplSelfDefinitionInfo {
        ImplSelfDefinitionInfo {
            generics: self.generics.clone(),
            impl_ty: self.impl_ty.clone(),
            where_sec: self.where_sec.clone(),
            require_methods: self.require_methods.iter().map(|(id, func)| (id.clone(), func.get_func_info().1)).collect()
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
    Ok((s, ImplSelfDefinition { generics, impl_ty, where_sec, require_methods }))
}
