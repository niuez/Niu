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
    Equal(TypeSpec, TypeSpec),
    HasTrait(TypeSpec, TraitId),
}

#[derive(Debug, Clone)]
pub struct WhereSection {
    has_traits: HashSet<(TypeSpec, TraitId)>,
    equals: HashSet<(TypeSpec, TypeSpec)>,
}

impl WhereSection {
    pub fn regist_equations(&self, mp: &HashMap<TypeId, Type>, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), String> {
        for (spec, tr_id) in self.has_traits.iter() {
            let ty = spec.generics_to_type(Some(mp), equs, trs)?;
            equs.add_has_trait(ty, tr_id.clone());
        }
        if self.equals.len() > 0 {
            unreachable!("it is not support now")
        }
        Ok(())
    }

    pub fn regist_candidate(&self, trs: &mut TraitsInfo) -> Result<(), String> {
        for (spec, tr_id) in self.has_traits.iter() {
            trs.regist_param_candidate(spec, tr_id)?;
        }
        Ok(())
    }

    pub fn check_equal(&self, right: &Self) -> bool {
        self.has_traits == right.has_traits && self.equals == right.equals
    }
}

fn parse_has_trait_element(s: &str) -> IResult<&str, WhereElem> {
    let (s, (spec, _, _, _, tr_id)) = tuple((parse_type_spec, space0, char(':'), space0, parse_trait_id))(s)?;
    Ok((s, WhereElem::HasTrait(spec, tr_id)))
}

fn parse_equal_element(s: &str) -> IResult<&str, WhereElem> {
    let (s, (spec_left, _, _, _, spec_right)) = tuple((parse_type_spec, space0, char('='), space0, parse_type_spec))(s)?;
    Ok((s, WhereElem::Equal(spec_left, spec_right)))
}

pub fn parse_where_section(s: &str) -> IResult<&str, WhereSection> {
    let (s, op) = opt(
        tuple((
                tag("where"), space1,
                separated_list0(tuple((space0, char(','), space0)), alt((parse_has_trait_element, parse_equal_element))),
                opt(tuple((space0, char(','))))
                ))
        )(s)?;
    let (equals, has_traits) = match op {
        Some((_, _, equs, _)) => {
            equs.into_iter().fold((HashSet::new(), HashSet::new()), |(mut equals, mut has_traits), elem| {
                match elem {
                    WhereElem::Equal(left, right) => equals.insert((left, right)),
                    WhereElem::HasTrait(spec, id) => has_traits.insert((spec, id)),
                };
                (equals, has_traits)
            })
        }
        None => (HashSet::new(), HashSet::new())
    };
    Ok((s, WhereSection { equals, has_traits }))
}

#[test]
fn parse_where_section_test() {
    println!("{:?}", parse_where_section("where T: Add, T#Hoge::Output: Add, T#Hoge::Output=i64"));
}
