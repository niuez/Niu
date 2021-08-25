use std::collections::{ HashSet, HashMap };

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::type_spec::*;
use crate::traits::*;

use crate::unify::*;
use crate::trans::*;

#[derive(Debug, Clone)]
pub struct WhereSection {
    has_traits: Vec<(TypeSpec, usize, TraitId, Vec<(AssociatedTypeIdentifier, TypeSpec)>)>,
}

impl WhereSection {
    pub fn empty() -> Self {
        WhereSection { has_traits: Vec::new() }
    }
    pub fn regist_equations(&self, mp: &GenericsTypeMap, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), String> {
        for (spec, _, tr_id, asso_eqs) in self.has_traits.iter() {
            let ty = spec.generics_to_type(mp, equs, trs)?;
            equs.add_has_trait(ty.clone(), tr_id.clone());
            for (asso_id, asso_spec) in asso_eqs.iter() {
                let asso_ty = Type::AssociatedType(Box::new(ty.clone()), AssociatedType { trait_id: tr_id.clone(), type_id: asso_id.clone() });
                let asso_spec_ty = asso_spec.generics_to_type(mp, equs, trs)?;
                equs.add_equation(asso_ty, asso_spec_ty)
            }
        }
        Ok(())
    }

    pub fn regist_candidate(&self, equs: &TypeEquations, trs: &mut TraitsInfo) -> Result<(), String> {
        for (spec, _, tr_id, asso_eqs) in self.has_traits.iter() {


            let mut tmp_equs = TypeEquations::new();

            let param_ty = spec.generate_type_no_auto_generics(equs, trs)?;
            let alpha = tr_id.id.generate_type_variable("ParamType", 0, &mut tmp_equs);
            tmp_equs.add_equation(param_ty, alpha);

            for (asso_id, asso_spec) in asso_eqs.iter() {
                let asso_spec_ty = asso_spec.generate_type_no_auto_generics(&equs, trs)?;
                let alpha = asso_id.id.generate_type_variable("AssociatedType", 0, &mut tmp_equs);
                tmp_equs.add_equation(asso_spec_ty, alpha);
            }
            tmp_equs.unify(trs).map_err(|err| err.to_string())?;
            let substs = SubstsMap::new(tmp_equs.take_substs().clone());

            let param_ty = substs.get(&tr_id.id, "ParamType", 0)?;
            let asso_mp = asso_eqs.iter().map(|(asso_id, _)| {
                    let asso_spec_ty = substs.get(&asso_id.id, "AssociatedType", 0)?;
                    Ok((asso_id.clone(), asso_spec_ty))
                }).collect::<Result<HashMap<_, _>, String>>()?;
            
            trs.regist_param_candidate(param_ty, tr_id, asso_mp)?;
            
        }
        Ok(())
    }

    pub fn check_equal(&self, right: &Self) -> bool {
                self.has_traits.clone().into_iter().collect::<HashSet<_>>()
            == right.has_traits.clone().into_iter().collect::<HashSet<_>>()
    }

    pub fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut conds = Vec::new();
        for (ty, _, tr, assos) in self.has_traits.iter() {
            let trait_ty = format!("{}<{}>", tr.transpile(ta), ty.transpile(ta));
            conds.push(trait_ty.clone());
            for (id, asso_ty) in assos.iter() {
                conds.push(format!("std::is_same<typename {}::{}, {}>", trait_ty.clone(), id.transpile(ta), asso_ty.transpile(ta)));
            }
        }
        if conds.len() == 0 {
            format!("")
        }
        else {
            format!("std::conjunction<{}>", conds.join(", "))
        }
    }
}

fn parse_associated_type_specifier_elem(s: &str) -> IResult<&str, (AssociatedTypeIdentifier, TypeSpec)> {
    let (s, (id, _, _, _, spec)) = tuple((parse_associated_type_identifier, multispace0, char('='), multispace0, parse_type_spec))(s)?;
    Ok((s, (id, spec)))
}

fn parse_associated_type_specifiers(s: &str) -> IResult<&str, Vec<(AssociatedTypeIdentifier, TypeSpec)>> {
    let (s, op) = opt(tuple((char('('), multispace0, separated_list0(tuple((multispace0, char(','), multispace0)), parse_associated_type_specifier_elem), multispace0, char(')'))))(s)?;
    let res = match op {
        Some((_, _, res, _, _)) => res,
        None => Vec::new(),
    };
    Ok((s, res))
}

fn parse_has_trait_element(s: &str) -> IResult<&str, (TypeSpec, usize, TraitId, Vec<(AssociatedTypeIdentifier, TypeSpec)>)> {
    let (s, (spec, _, _, _, tr_id, _, assos)) = tuple((parse_type_spec, multispace0, char(':'), multispace0, parse_trait_id, multispace0, parse_associated_type_specifiers))(s)?;
    let dep = spec.associated_type_depth();
    Ok((s, (spec, dep, tr_id, assos)))
}

pub fn parse_where_section(s: &str) -> IResult<&str, WhereSection> {
    let (s, op) = opt(
        tuple((
                tag("where"), space1,
                separated_list0(tuple((multispace0, char(','), multispace0)), parse_has_trait_element),
                opt(tuple((multispace0, char(','))))
                ))
        )(s)?;
    let has_traits = match op {
        Some((_, _, equs, _)) => equs,
        None => Vec::new(),
    };
    Ok((s, WhereSection { has_traits }))
}

#[test]
fn parse_where_section_test() {
    println!("{:?}", parse_where_section("where T: Add, T#Hoge::Output: Add, T#Hoge::Output=i64"));
    println!("{:?}", parse_where_section("where S: Add(Output=T)"));
}
