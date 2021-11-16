use std::collections::{ HashSet, HashMap };

use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::branch::*;
use nom::IResult;

use crate::type_spec::*;
use crate::traits::*;

use crate::identifier::Tag;
use crate::unify::*;
use crate::trans::*;
use crate::error::*;

#[derive(Debug, Clone)]
pub struct WhereSection {
    has_traits: Vec<(TypeSpec, usize, TraitSpec, Vec<(AssociatedTypeIdentifier, TypeSpec)>, SourceRange)>,
    tuple_traits: Vec<(TypeSpec, TraitId, SourceRange)>,
    range: SourceRange,
}

impl WhereSection {
    pub fn empty() -> Self {
        WhereSection { has_traits: Vec::new(), tuple_traits: Vec::new(), range: SourceRange::empty(), }
    }
    pub fn is_empty(&self) -> bool {
        self.has_traits.is_empty()
    }
    pub fn regist_equations(&self, mp: &GenericsTypeMap, equs: &mut TypeEquations, trs: &TraitsInfo, define_source: &ErrorHint) -> Result<(), Error> {
        for (spec, _, tr_spec, asso_eqs, range) in self.has_traits.iter() {
            let ty = spec.generics_to_type(mp, equs, trs)?;
            let tr_gen = tr_spec.generate_trait_generics(equs, trs, mp)?;
            equs.add_has_trait(ty.clone(), tr_gen.clone(),
                range.hint("param candidate of where section", define_source.clone())
            );
            for (asso_id, asso_spec) in asso_eqs.iter() {
                let asso_ty = Type::AssociatedType(AssociatedTypeEquation {
                    caller_type: Box::new(ty.clone()),
                    trait_gen: Some(tr_gen.clone()),
                    associated_type_id: asso_id.clone(),
                    caller_range: range.clone().hint("associated type of where section", define_source.clone()),
                    tag: Tag::new(),
                });
                let asso_spec_ty = asso_spec.generics_to_type(mp, equs, trs)?;
                equs.add_equation(asso_ty, asso_spec_ty)
            }
        }
        for (spec, tr_id, range) in self.tuple_traits.iter() {
            let ty = spec.generics_to_type(mp, equs, trs)?;
            equs.add_tuple_trait(ty, tr_id.clone(), range.hint("tuple param candidate", define_source.clone()));
        }
        Ok(())
    }

    pub fn regist_candidate(&self, equs: &TypeEquations, trs: &mut TraitsInfo, define_hint: &ErrorHint) -> Result<(), Error> {
        for (spec, _, tr_spec, asso_eqs, _range) in self.has_traits.iter() {


            let mut tmp_equs = TypeEquations::new();

            let param_ty = spec.generate_type_no_auto_generics(equs, trs)?;
            let alpha = tr_spec.get_tag().generate_type_variable("ParamType", 0, &mut tmp_equs);
            tmp_equs.add_equation(param_ty, alpha);

            let tr_gen = tr_spec.generate_trait_generics_with_no_map(equs, trs)?;

            for (asso_id, asso_spec) in asso_eqs.iter() {
                let asso_spec_ty = asso_spec.generate_type_no_auto_generics(&equs, trs)?;
                let alpha = asso_id.id.generate_type_variable("AssociatedType", 0, &mut tmp_equs);
                tmp_equs.add_equation(asso_spec_ty, alpha);
            }
            tmp_equs.unify(trs).map_err(|err| err.into_err())?;
            let substs = SubstsMap::new(tmp_equs.take_substs().clone());

            let param_ty = substs.get_from_tag(&tr_spec.get_tag(), "ParamType", 0)?;
            let asso_mp = asso_eqs.iter().map(|(asso_id, _)| {
                    let asso_spec_ty = substs.get(&asso_id.id, "AssociatedType", 0)?;
                    Ok((asso_id.clone(), asso_spec_ty))
                }).collect::<Result<HashMap<_, _>, Error>>()?;
            
            trs.regist_param_candidate(param_ty, &tr_gen, asso_mp, define_hint)?;
            
        }
        Ok(())
    }

    pub fn check_equal(&self, right: &Self) -> bool {
                self.has_traits.clone().into_iter().map(|(t0, t1, t2, t3, _)| (t0, t1, t2, t3)).collect::<HashSet<_>>()
            == right.has_traits.clone().into_iter().map(|(t0, t1, t2, t3, _)| (t0, t1, t2, t3)).collect::<HashSet<_>>()
    }

    pub fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut conds = Vec::new();
        //let bin_opes = BINARY_OPERATOR_TRAITS.iter().cloned().collect::<HashMap<_, _>>();
        for (ty, _, tr, assos, _range) in self.has_traits.iter() {
            match find_operator(tr.trait_id.id.into_string().as_str()) {
                Some(ResultFindOperator::Binary((_, ope))) => {
                    let assos = assos.iter().map(|(id, asso_ty)| (id.id.into_string(), asso_ty.clone())).collect::<HashMap<_, _>>();
                    let arg_ty = tr.generics[0].transpile(ta);
                    let output_ty = assos.get("Output").cloned();
                    match output_ty {
                        Some(output_ty) => {
                            conds.push(format!("std::is_same<decltype(std::declval<{}>() {} std::declval<{}>()), {}>",
                                ty.transpile(ta), ope, arg_ty, output_ty.transpile(ta)));
                        }
                        None => {
                            conds.push(format!("decltype(std::declval<{}>() {} std::declval<{}>(), std::true_type())",
                                ty.transpile(ta), ope, arg_ty));
                        }
                    }
                }
                Some(ResultFindOperator::Unary((_, ope))) => {
                    let assos = assos.iter().map(|(id, asso_ty)| (id.id.into_string(), asso_ty.clone())).collect::<HashMap<_, _>>();
                    let output_ty = assos.get("Output").cloned();
                    match output_ty {
                        Some(output_ty) => {
                            conds.push(format!("std::is_same<decltype({}std::declval<{}>()), {}>",
                                ope, ty.transpile(ta), output_ty.transpile(ta)));
                        }
                        None => {
                            conds.push(format!("decltype({}std::declval<{}>(), std::true_type())",
                                ope, ty.transpile(ta)));
                        }
                    }
                }
                Some(ResultFindOperator::Eq) => {
                    conds.push(format!("decltype(std::declval<{0}>() == std::declval<{0}>(), std::true_type())", ty.transpile(ta)))
                }
                Some(ResultFindOperator::Ord) => {
                    conds.push(format!("decltype(std::declval<{0}>() < std::declval<{0}>(), std::true_type())", ty.transpile(ta)))
                }
                Some(ResultFindOperator::Clone) => {
                    conds.push(format!("std::is_copy_assignable<{}>", ty.transpile(ta)))
                }
                Some(ResultFindOperator::Copy) => {
                    conds.push(format!("std::is_copy_assignable<{}>", ty.transpile(ta)))
                }
                None => {
                    let generics = std::iter::once(ty.transpile(ta)).chain(tr.generics.iter().map(|g| g.transpile(ta))).collect::<Vec<_>>().join(", ");
                    let trait_ty = format!("{}<{}>", tr.trait_id.transpile(ta), generics);
                    conds.push(trait_ty.clone());
                    for (id, asso_ty) in assos.iter() {
                        conds.push(format!("std::is_same<typename {}::{}, {}>", trait_ty.clone(), id.transpile(ta), asso_ty.transpile(ta)));
                    }
                }
            }
        }
        if conds.len() == 0 {
            format!("void")
        }
        else {
            format!("std::enable_if_t<std::conjunction_v<{}>>", conds.join(", "))
        }
    }
}

fn parse_associated_type_specifier_elem(s: &str) -> IResult<&str, (AssociatedTypeIdentifier, TypeSpec)> {
    let (s, ((id, _, _, _, spec), _range)) = with_range(tuple((parse_associated_type_identifier, multispace0, char('='), multispace0, parse_type_spec)))(s)?;
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

fn parse_has_trait_element(s: &str) -> IResult<&str, WhereElem> {
    let (s, ((spec, _, _, _, tr_id, _, assos), range)) = with_range(tuple((parse_type_spec, multispace0, char(':'), multispace0, parse_trait_spec, multispace0, parse_associated_type_specifiers)))(s)?;
    let dep = spec.associated_type_depth();
    Ok((s, WhereElem::HasTrait((spec, dep, tr_id, assos, range))))
}

fn parse_tuple_trait_element(s: &str) -> IResult<&str, WhereElem> {
    let (s, ((_, _, spec, _, _, _, tr_id), range)) = with_range(tuple((tag("tuple"), multispace1, parse_type_spec, multispace0, char(':'), multispace0, parse_trait_id)))(s)?;
    Ok((s, WhereElem::TupleTrait((spec, tr_id, range))))
}

enum WhereElem {
    HasTrait((TypeSpec, usize, TraitSpec, Vec<(AssociatedTypeIdentifier, TypeSpec)>, SourceRange)),
    TupleTrait((TypeSpec, TraitId, SourceRange)),
}

pub fn parse_where_section(s: &str) -> IResult<&str, WhereSection> {
    let (s, (op, range)) = with_range(opt(
        tuple((
                tag("where"), multispace1,
                separated_list0(tuple((multispace0, char(','), multispace0)), alt((parse_has_trait_element, parse_tuple_trait_element))),
                opt(tuple((multispace0, char(','))))
                ))
        ))(s)?;
    let mut has_traits = Vec::new();
    let mut tuple_traits = Vec::new();
    if let Some((_, _, equs, _)) = op {
        for e in equs {
            match e {
                WhereElem::HasTrait(h) => has_traits.push(h),
                WhereElem::TupleTrait(h) => tuple_traits.push(h),
            }
        }
    }
    Ok((s, WhereSection { has_traits, tuple_traits, range }))
}

#[test]
fn parse_where_section_test() {
    log::debug!("{:?}", parse_where_section("where S: Add(Output=T)").unwrap());
    log::debug!("{:?}", parse_where_section("where S: Add<T>(Output=T)").unwrap());
    log::debug!("{:?}", parse_where_section("where S: Add<T>(Output=T), S: Sub<T>(Output=T)").unwrap());
}
