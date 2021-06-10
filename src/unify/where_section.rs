use std::collections::{ HashSet, HashMap };

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use nom::branch::*;

use crate::type_id::*;
use crate::type_spec::*;
use crate::traits::*;

use crate::unify::*;

#[derive(Debug, Clone)]
pub enum WhereElem {
    HasTrait(TypeSpec, TraitId),
}

#[derive(Debug, Clone)]
pub struct WhereSection {
    has_traits: Vec<(TypeSpec, usize, TraitId)>,
}

impl WhereSection {
    pub fn regist_equations(&self, mp: &HashMap<TypeId, Type>, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), String> {
        for (spec, _, tr_id) in self.has_traits.iter() {
            let ty = spec.generics_to_type(Some(mp), equs, trs)?;
            equs.add_has_trait(ty, tr_id.clone());
        }
        Ok(())
    }

    pub fn regist_candidate(&self, equs: &TypeEquations, trs: &mut TraitsInfo) -> Result<(), String> {
        for (spec, _, tr_id) in self.has_traits.iter() {
            let param_ty = spec.generate_type_no_auto_generics(equs, trs)?;

            let mut equs = TypeEquations::new();
            equs.add_equation(param_ty, tr_id.id.generate_type_variable(0));
            let param_ty = SubstsMap::new(equs.unify(trs)?).get(&tr_id.id, 0)?;
            trs.regist_param_candidate(param_ty, tr_id)?;
        }
        Ok(())
    }

    pub fn check_equal(&self, right: &Self) -> bool {
                self.has_traits.clone().into_iter().collect::<HashSet<_>>()
            == right.has_traits.clone().into_iter().collect::<HashSet<_>>()
    }
}

fn parse_has_trait_element(s: &str) -> IResult<&str, WhereElem> {
    let (s, (spec, _, _, _, tr_id)) = tuple((parse_type_spec, space0, char(':'), space0, parse_trait_id))(s)?;
    Ok((s, WhereElem::HasTrait(spec, tr_id)))
}

pub fn parse_where_section(s: &str) -> IResult<&str, WhereSection> {
    let (s, op) = opt(
        tuple((
                tag("where"), space1,
                separated_list0(tuple((space0, char(','), space0)), parse_has_trait_element),
                opt(tuple((space0, char(','))))
                ))
        )(s)?;
    let mut has_traits = match op {
        Some((_, _, equs, _)) => {
            equs.into_iter().fold(Vec::new(), |mut has_traits, elem| {
                match elem {
                    WhereElem::HasTrait(spec, id) => {
                        let dep = spec.associated_type_depth();
                        has_traits.push((spec, dep, id))
                    }
                };
                has_traits
            })
        }
        None => Vec::new(),
    };
    has_traits.sort_by_key(|(_, ref d, _)| *d);
    Ok((s, WhereSection { has_traits }))
}

#[test]
fn parse_where_section_test() {
    println!("{:?}", parse_where_section("where T: Add, T#Hoge::Output: Add, T#Hoge::Output=i64"));
}
