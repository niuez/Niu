use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use nom::branch::*;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_id::{ TypeId, parse_type_id };
use crate::block::{ Block, parse_block };
use crate::unify::*;
use crate::unary_expr::Variable;
use crate::trans::*;
use crate::type_spec::*;
use crate::traits::*;

use crate::unify::*;

#[derive(Debug)]
pub enum WhereElem {
    Equal(TypeSpec, TypeSpec),
    HasTrait(TypeSpec, TraitId),
}

#[derive(Debug)]
pub struct WhereSection {
    equs: Vec<WhereElem>,
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
    let equs = match op {
        Some((_, _, equs, _)) => equs,
        None => Vec::new(),
    };
    Ok((s, WhereSection { equs }))
}

#[test]
fn parse_where_section_test() {
    println!("{:?}", parse_where_section("where T: Add, T#Hoge::Output: Add, T#Hoge::Output=i64"));
}
