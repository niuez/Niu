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
use crate::move_checker::*;
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
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<(TypeEquations, ErrorHint), CallEquationSolveError> {
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
    pub fn match_self_impl_for_ty(&self, ty: &Type, trs: &TraitsInfo) -> Option<(SubstsMap, &Self)> {
        match *self {
            SelectionCandidate::ImplSelfCandidate(ref cand) => {
                cand.match_impl_for_ty(ty, trs).map(|sub| (sub, self))
            }
            _ => None,
        }
    }
    pub fn generate_equations_for_associated_type_equation(&self, associated_eq: &AssociatedTypeEquation, trs: &TraitsInfo) -> Result<(TypeEquations, ErrorHint), CallEquationSolveError> {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.generate_equations_for_associated_type_equation(associated_eq, trs)
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.generate_equations_for_associated_type_equation(associated_eq, trs)
            }
            SelectionCandidate::ImplSelfCandidate(_) => {
                unreachable!("ImplSelfCandidate doesnt have associated type")
            }
        }
    }
    pub fn match_impl_for_ty(&self, trait_gen: &TraitGenerics, ty: &Type, trs: &TraitsInfo) -> Option<(SubstsMap, &Self)> {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.match_impl_for_ty(trait_gen, ty, trs).map(|sub| (sub, self))
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.match_impl_for_ty(trait_gen, ty, trs).map(|sub| (sub, self))
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

    pub fn debug_str(&self) -> String {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.debug_str()
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.debug_str()
            }
            SelectionCandidate::ImplSelfCandidate(ref cand) => {
                cand.debug_str()
            }
        }
    }
    pub fn hint(&self) -> ErrorHint {
        match *self {
            SelectionCandidate::ImplCandidate(ref cand) => {
                cand.hint()
            }
            SelectionCandidate::ParamCandidate(ref cand) => {
                cand.hint()
            }
            SelectionCandidate::ImplSelfCandidate(ref cand) => {
                cand.hint()
            }
        }
    }
}

#[derive(Debug)]
pub struct ImplDefinition {
    pub generics: Vec<TypeId>,
    pub trait_spec: TraitSpec,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub without_member_range: SourceRange,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinition>,
}

impl ImplDefinition {
    pub fn get_trait_id(&self) -> TraitId {
        self.trait_spec.trait_id.clone()
    }
    pub fn get_impl_ty_id(&self) -> Option<TypeId> {
        self.impl_ty.get_type_id().ok()
    }
    pub fn get_impl_trait_pair(&self) -> (TraitId, SelectionCandidate) {
        (self.trait_spec.trait_id.clone(), SelectionCandidate::ImplCandidate(ImplCandidate {
            generics: self.generics.clone(),
            trait_spec: self.trait_spec.clone(),
            impl_ty: self.impl_ty.clone(),
            where_sec: self.where_sec.clone(),
            without_member_range: self.without_member_range.clone(),
            asso_defs: self.asso_defs.clone(),
            require_methods: self.require_methods.iter().map(|(id, func)| (id.clone(), func.get_func_info().1)).collect(),
        }))
    }

    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), Error> {
        let mut trs = trs.into_scope();
        for ty_id in self.generics.iter() {
            trs.regist_generics_type(ty_id)?;
        }
        self.where_sec.regist_candidate(equs, &mut trs, &self.without_member_range.hint("impl definition", ErrorHint::None))?;
        let next_self_type = Some(self.impl_ty.generate_type_no_auto_generics(equs, &trs)?);
        let before_self_type = equs.set_self_type(next_self_type);
        for def in self.require_methods.values() {
            def.unify_definition(equs, &trs, &self.without_member_range.hint("impl definition", ErrorHint::None))?;
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
    pub fn transpile_functions(&self, ta: &TypeAnnotation) -> String {
        match find_operator(self.trait_spec.trait_id.id.into_string().as_str()) {
            None => {
                let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta))).collect::<Vec<_>>().join(", ");
                let templates = if generics == "" { format!("") } else { format!("template<{}> ", generics) };
                let where_str = self.where_sec.transpile(ta);
                let generics_param = std::iter::once(self.impl_ty.transpile(ta)).chain(self.trait_spec.generics.iter().map(|g| g.transpile(ta)))
                    .collect::<Vec<_>>().join(", ");
                let class_str = format!("{}<{}, {}>::", self.trait_spec.trait_id.transpile(ta), generics_param, where_str);
                let require_methods = self.require_methods.iter().map(|(_, def)| {
                    format!("{}{}", templates, def.transpile_for_impl(ta, &class_str, false))
                }).collect::<Vec<_>>().join("\n");
                require_methods
            }
            Some(ResultFindOperator::Unary((_func, _))) | Some(ResultFindOperator::Binary((_func, _))) => {
                
                let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta)))
                    .chain(std::iter::once(format!("class")))
                    .collect::<Vec<_>>().join(", ");
                let templates = format!("template<{}> ", generics);
                let require_methods = self.require_methods.iter().map(|(_, def)| {
                    let func = def.transpile(ta, false);
                    if func == "" {
                        format!("")
                    }
                    else {
                        format!("{}{}\n", templates, func)
                    }
                }).collect::<Vec<_>>().join("");
                require_methods
            }
            Some(ResultFindOperator::Eq) => {
                let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta)))
                    .chain(std::iter::once(format!("class")))
                    .collect::<Vec<_>>().join(", ");
                let templates = format!("template<{}> ", generics);
                let require_methods = self.require_methods.iter().map(|(_, def)| {
                    let func = def.transpile(ta, false);
                    if func == "" {
                        format!("")
                    }
                    else {
                        let impl_type = self.impl_ty.transpile(ta);
                        let right_type = impl_type.clone();
                        let not_eq = format!("{} bool operator!=({} const& left, {} const& right) {{ return !(left == right); }}\n", templates, impl_type, right_type);
                        format!("{}{}\n{}", templates, func, not_eq)
                    }
                })
                .collect::<Vec<_>>().join("");
                require_methods
            }
            Some(ResultFindOperator::Ord) => {
                let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta)))
                    .chain(std::iter::once(format!("class")))
                    .collect::<Vec<_>>().join(", ");
                let templates = format!("template<{}> ", generics);
                let require_methods = self.require_methods.iter().map(|(_, def)| {
                    let func = def.transpile(ta, false);
                    if func == "" {
                        format!("")
                    }
                    else {
                        let impl_type = self.impl_ty.transpile(ta);
                        let right_type = impl_type.clone();
                        let gr = format!("{} bool operator>({} const& left, {} const& right) {{ return right < left; }}\n", templates, impl_type, right_type);
                        let leq = format!("{} bool operator<=({} const& left, {} const& right) {{ return left < right || left == right; }}\n", templates, impl_type, right_type);
                        let geq = format!("{} bool operator>=({} const& left, {} const& right) {{ return right < left || left == right; }}\n", templates, impl_type, right_type);
                        format!("{}{}\n{}{}{}", templates, func, gr, leq, geq)
                    }
                })
                .collect::<Vec<_>>().join("");
                require_methods
            }
            Some(ResultFindOperator::Clone) => {
                if let FuncBlock::CppInline(_) = self.require_methods[&TraitMethodIdentifier { id: Identifier::from_str("clone") }].block {
                    format!("")
                }
                else {
                    let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta))).collect::<Vec<_>>().join(", ");
                    let templates = if generics == "" { format!("") } else { format!("template<{}> ", generics) };
                    let where_str = self.where_sec.transpile(ta);
                    let generics_param = std::iter::once(self.impl_ty.transpile(ta)).chain(self.trait_spec.generics.iter().map(|g| g.transpile(ta)))
                        .collect::<Vec<_>>().join(", ");
                    let class_str = format!("{}<{}, {}>::", self.trait_spec.trait_id.transpile(ta), generics_param, where_str);
                    let require_methods = self.require_methods.iter().map(|(_, def)| {
                        let func = def.transpile_for_impl(ta, &class_str, false);
                        format!("{}{}", templates, func)
                    }).collect::<Vec<_>>().join("\n");
                    require_methods
                }
            }
            Some(ResultFindOperator::Copy) => {
                format!("")
            }
        }
    }
    pub fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<(), String> {
        for (_, func) in self.require_methods.iter() {
            func.move_check(mc, ta)?;
        }
        Ok(())
    }
}

fn parse_generics_args(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Vec<TypeId>> {
    let (s, op) = opt(tuple((multispace0, char('<'), multispace0, separated_list0(tuple((multispace0, char(','), multispace0)), parse_type_id), multispace0, char('>'))))(s)?;
    Ok((s, op.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new())))
}

/*fn parse_generics_params(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Vec<TypeSpec>> {
    let (s, op) = opt(tuple((multispace0, char('<'), multispace0, separated_list0(tuple((multispace0, char(','), multispace0)), parse_type_spec), multispace0, char('>'))))(s)?;
    Ok((s, op.map(|(_, _, _, res, _, _)| res).unwrap_or(Vec::new())))
}*/

pub fn parse_impl_definition(s: ContentStr<'_>) -> IResult<ContentStr<'_>, ImplDefinition> {
    let (s, (((_, generics, _, trait_spec, _, _, _, impl_ty, _, where_sec), range), _, _, _, many_types, many_methods, _, _)) = 
        tuple((
            with_range(tuple((tag("impl"), parse_generics_args,
                multispace1, parse_trait_spec,
                multispace1, tag("for"), multispace1, parse_type_spec,
                multispace0, parse_where_section,
            ))),
            multispace0, char('{'), multispace0,
            many0(tuple((tag("type"), multispace1, parse_associated_type_identifier, multispace0, char('='), multispace0, parse_type_spec, multispace0, char(';'), multispace0))),
            many0(tuple((parse_func_definition, multispace0))),
            multispace0, char('}')))(s)?;
    let asso_defs = many_types.into_iter().map(|(_, _, id, _, _, _, ty, _, _, _)| (id, ty)).collect();
    let require_methods = match find_operator(trait_spec.trait_id.id.into_string().as_str()) {
        None | Some(ResultFindOperator::Clone) => {
            many_methods.into_iter().map(|(func, _)| (TraitMethodIdentifier { id: func.func_id.clone() }, func)).collect()
        }
        Some(ResultFindOperator::Unary((_, ope))) | Some(ResultFindOperator::Binary((_, ope))) => {
            many_methods.into_iter().map(|(mut func, _)| {
                func.func_id = Identifier::from_str(format!("operator{}", ope).as_str());
                (TraitMethodIdentifier { id: func.func_id.clone() }, func)
            }).collect()
        }
        Some(ResultFindOperator::Eq) => {
            many_methods.into_iter().map(|(mut func, _)| {
                func.func_id = Identifier::from_str("operator==");
                (TraitMethodIdentifier { id: func.func_id.clone() }, func)
            }).collect()
        }
        Some(ResultFindOperator::Ord) => {
            many_methods.into_iter().map(|(mut func, _)| {
                func.func_id = Identifier::from_str("operator<");
                (TraitMethodIdentifier { id: func.func_id.clone() }, func)
            }).collect()
        }
        Some(ResultFindOperator::Copy) => {
            HashMap::new()
        }
    };
    Ok((s, ImplDefinition { generics, trait_spec, impl_ty, where_sec, asso_defs, require_methods, without_member_range: range }))
}

impl Transpile for ImplDefinition {
    fn transpile(&self, ta: &TypeAnnotation) -> String {

        match find_operator(self.trait_spec.trait_id.id.into_string().as_str()) {
            None => {
                let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta))).collect::<Vec<_>>().join(", ");
                let where_str = self.where_sec.transpile(ta);
                let templates = format!("template<{}> ", generics);
                let generics_param = std::iter::once(self.impl_ty.transpile(ta)).chain(self.trait_spec.generics.iter().map(|g| g.transpile(ta)))
                    .collect::<Vec<_>>().join(", ");
                let impl_def = if self.where_sec.is_empty() {
                    format!("{}struct {}<{}, void>: std::true_type", templates, self.trait_spec.trait_id.transpile(ta), generics_param.clone())
                }
                else {
                    format!("{}struct {}<{}, {}>: std::true_type", templates, self.trait_spec.trait_id.transpile(ta), generics_param.clone(), where_str)
                };
                let asso_defs = self.asso_defs.iter().map(|(id, spec)| {
                    format!("using {} = {};\n", id.transpile(ta), spec.transpile(ta))
                }).collect::<Vec<_>>().join(" ");
                let require_methods = self.require_methods.iter().map(|(_, def)| {
                    let def_str = def.transpile_definition_only(ta, "", true);
                    format!("{};", def_str)
                }).collect::<Vec<_>>().join("\n");
                format!("{} {{\n{}\n{}\n}};\n", impl_def, asso_defs, require_methods)
            }
            Some(ResultFindOperator::Unary((func, _))) | Some(ResultFindOperator::Binary((func, _))) => {
                if let FuncBlock::CppInline(_) = self.require_methods[&TraitMethodIdentifier { id: Identifier::from_str(func) }].block {
                    format!("")
                }
                else {
                    let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta)))
                        .chain(std::iter::once(format!("class = {}", self.where_sec.transpile(ta))))
                        .collect::<Vec<_>>().join(", ");
                    let templates = format!("template<{}> ", generics);
                    let require_methods = self.require_methods.iter().map(|(_, def)| {
                        let func = def.transpile_definition_only(ta, "", false);
                        if func == "" {
                            format!("")
                        }
                        else {
                            format!("{}{};\n", templates, func)
                        }
                    }).collect::<Vec<_>>().join("");
                    require_methods
                }
            }
            Some(ResultFindOperator::Eq) => {
                if let FuncBlock::CppInline(_) = self.require_methods[&TraitMethodIdentifier { id: Identifier::from_str("operator==") }].block {
                    format!("")
                }
                else {
                    let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta)))
                        .chain(std::iter::once(format!("class = {}", self.where_sec.transpile(ta))))
                        .collect::<Vec<_>>().join(", ");
                    let templates = format!("template<{}> ", generics);
                    let require_methods = self.require_methods.iter().map(|(_, def)| {
                        let func = def.transpile_definition_only(ta, "", false);
                        if func == "" {
                            format!("")
                        }
                        else {
                            format!("{}{};\n", templates, func)
                        }
                    })
                    .chain(std::iter::once( {
                        let impl_type = self.impl_ty.transpile(ta);
                        let right_type = impl_type.clone();
                        //format!("{} bool operator!=({} const& left, {} const& right) {{ return !(left == right); }}\n", templates, impl_type, right_type)
                        format!("{} bool operator!=({} const& left, {} const& right);\n", templates, impl_type, right_type)
                    } ))
                    .collect::<Vec<_>>().join("");
                    require_methods
                }
            }
            Some(ResultFindOperator::Ord) => {
                if let FuncBlock::CppInline(_) = self.require_methods[&TraitMethodIdentifier { id: Identifier::from_str("operator<") }].block {
                    format!("")
                }
                else {
                    let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta)))
                        .chain(std::iter::once(format!("class = {}", self.where_sec.transpile(ta))))
                        .collect::<Vec<_>>().join(", ");
                    let templates = format!("template<{}> ", generics);
                    let require_methods = self.require_methods.iter().map(|(_, def)| {
                        let func = def.transpile_definition_only(ta, "", false);
                        if func == "" {
                            format!("")
                        }
                        else {
                            format!("{}{};\n", templates, func)
                        }
                    })
                    .chain(std::iter::once( {
                        let impl_type = self.impl_ty.transpile(ta);
                        let right_type = impl_type.clone();
                        //format!("{} bool operator!=({} const& left, {} const& right) {{ return !(left == right); }}\n", templates, impl_type, right_type)
                        let gr = format!("{} bool operator>({} const& left, {} const& right);\n", templates, impl_type, right_type);
                        let leq = format!("{} bool operator<=({} const& left, {} const& right);\n", templates, impl_type, right_type);
                        let geq = format!("{} bool operator>=({} const& left, {} const& right);\n", templates, impl_type, right_type);
                        format!("{}\n{}\n{}", gr, leq, geq)
                    } ))
                    .collect::<Vec<_>>().join("");
                    require_methods
                }
            }
            Some(ResultFindOperator::Clone) => {
                if let FuncBlock::CppInline(_) = self.require_methods[&TraitMethodIdentifier { id: Identifier::from_str("clone") }].block {
                    format!("")
                }
                else {
                    let generics = self.generics.iter().map(|id| format!("class {}", id.transpile(ta))).collect::<Vec<_>>().join(", ");
                    let where_str = self.where_sec.transpile(ta);
                    let templates = format!("template<{}> ", generics);
                    let generics_param = std::iter::once(self.impl_ty.transpile(ta)).chain(self.trait_spec.generics.iter().map(|g| g.transpile(ta)))
                        .collect::<Vec<_>>().join(", ");
                    let impl_def = if self.where_sec.is_empty() {
                        format!("{}struct {}<{}, void>: std::true_type", templates, self.trait_spec.trait_id.transpile(ta), generics_param.clone())
                    }
                    else {
                        format!("{}struct {}<{}, {}>: std::true_type", templates, self.trait_spec.trait_id.transpile(ta), generics_param.clone(), where_str)
                    };
                    let asso_defs = self.asso_defs.iter().map(|(id, spec)| {
                        format!("using {} = {};\n", id.transpile(ta), spec.transpile(ta))
                    }).collect::<Vec<_>>().join(" ");
                    let require_methods = self.require_methods.iter().map(|(_, def)| {
                        let def_str = def.transpile_definition_only(ta, "", true);
                        format!("{};", def_str)
                    }).collect::<Vec<_>>().join("\n");
                    format!("{} {{\n{}\n{}\n}};\n", impl_def, asso_defs, require_methods)
                }
            }
            Some(ResultFindOperator::Copy) => {
                format!("")
            }
        }
    }
}


#[derive(Debug, Clone)]
pub struct ImplCandidate {
    pub generics: Vec<TypeId>,
    pub trait_spec: TraitSpec,
    pub impl_ty: TypeSpec,
    pub where_sec: WhereSection,
    pub without_member_range: SourceRange,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, TypeSpec>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}


impl ImplCandidate {
    pub fn hint(&self) -> ErrorHint {
        self.without_member_range.hint("impl defined", ErrorHint::None)
    }
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<(TypeEquations, ErrorHint), CallEquationSolveError> {
        if let Some(trait_spec) = &call_eq.trait_gen {
            if trait_spec.trait_id != self.get_trait_id() {
                return Err(CallEquationSolveError::Error(ErrorComment::empty(format!("trait_id is not matched"))))
            }
        }
        let mut impl_equs = TypeEquations::new();
        let mut func_equs = TypeEquations::new();
        let self_type = call_eq.tag.generate_type_variable("SelfType", 0, &mut impl_equs);
        impl_equs.set_self_type(Some(self_type.clone()));
        func_equs.set_self_type(Some(self_type.clone()));

        if let Some(ref caller_type) = call_eq.caller_type {
            impl_equs.add_equation(self_type.clone(), caller_type.as_ref().clone(), ErrorComment::empty(format!("caller type equation")));
        }

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), call_eq.tag.generate_type_variable("Generics", i, &mut impl_equs)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);

        let self_trait_gen = self.trait_spec.generate_trait_generics(&mut impl_equs, trs, &gen_mp).unwrap();
        if let Some(ref trait_gen) = call_eq.trait_gen {
            for (i, (self_g, right_g)) in self_trait_gen.generics.iter().zip(trait_gen.generics.iter()).enumerate() {
                impl_equs.add_equation(self_g.clone(), right_g.clone(), ErrorComment::empty(format!("{}-th trait generics param equation", i)));
            }
        }

        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut impl_equs, trs).unwrap();
        impl_equs.add_equation(impl_ty, self_type.clone(), ErrorComment::empty(format!("type variable for self type")));
        self.where_sec.regist_equations(&gen_mp, &mut impl_equs, trs, &self.hint())
            .map_err(|e| CallEquationSolveError::Error(e))?;
        let func_def = self.require_methods
            .get(&TraitMethodIdentifier { id: call_eq.func_id.clone() })
            .ok_or(ErrorComment::empty(format!("require methods doesnt have {:?}", call_eq.func_id)))
                .map_err(|e| CallEquationSolveError::Error(e))?;
        let func_hint = func_def.hint(&self.hint());
        let func_ty = func_def
            .generate_type(&gen_mp, &mut func_equs, trs, &call_eq.func_id, &self.hint())
                .map_err(|e| CallEquationSolveError::Error(e))?;
        let mut not_same_args_length = false;
        match func_ty {
            Type::Func(args, ret, info) => {
                let alpha = call_eq.tag.generate_type_variable("FuncTypeInfo", 0, &mut func_equs);
                let info = match info {
                    FuncTypeInfo::None => {
                        let tag = Tag::new();
                        let alpha = tag.generate_type_variable("SelfType", 0, &mut func_equs);
                        func_equs.add_equation(alpha, self_type.clone(), ErrorComment::empty(format!("type variable for self type")));
                        let generics_cnt = self_trait_gen.generics.len();
                        for (i, g) in self_trait_gen.generics.into_iter().enumerate() {
                            let beta = tag.generate_type_variable("TraitGenerics", i, &mut func_equs);
                            func_equs.add_equation(beta, g, ErrorComment::empty(format!("type variable for trait generics")));
                        }
                        FuncTypeInfo::TraitFunc(self.trait_spec.trait_id.clone(), generics_cnt, tag)
                    }
                    info => info,
                };
                func_equs.add_equation(alpha, Type::Func(args.clone(), ret.clone(), info), ErrorComment::empty(format!("type variable for func type info")));

                if call_eq.caller_type.is_none() {
                    impl_equs.add_equation(args[0].clone(), call_eq.args[0].clone(), ErrorComment::empty(format!("0-th function arg equation")));
                }

                if args.len() != call_eq.args.len() {
                    not_same_args_length = true;
                }
                for (i, (l, r)) in args.into_iter().zip(call_eq.args.iter()).enumerate() {
                    func_equs.add_equation(l, r.clone(), ErrorComment::empty(format!("{}-th function arg equation", i)))
                }
                let return_ty = call_eq.tag.generate_type_variable("ReturnType", 0, &mut func_equs);
                func_equs.add_equation(*ret, return_ty, ErrorComment::empty(format!("type variable for return type")));
            }
            _ => unreachable!()
        }
        match impl_equs.unify(trs) {
            Err(UnifyErr::Contradiction(err)) => {
                Err(CallEquationSolveError::Error(ErrorUnify::new(format!(""), self.hint(), err)))
            }
            Ok(_) | Err(UnifyErr::Deficiency(_)) => {
                if not_same_args_length {
                    Err(CallEquationSolveError::ImplOk(ErrorComment::empty(format!("args len is not match"))))
                }
                else {
                    func_equs.take_over_equations(impl_equs);
                    match func_equs.unify(trs) {
                        Err(UnifyErr::Contradiction(err)) => {
                            Err(CallEquationSolveError::ImplOk(ErrorUnify::new(format!(""), func_hint, err)))
                        }
                        Ok(_) | Err(UnifyErr::Deficiency(_)) => {
                            Ok((func_equs, func_hint))
                        }
                    }
                }
            }
        }
    }

    pub fn generate_equations_for_associated_type_equation(&self, associated_eq: &AssociatedTypeEquation, trs: &TraitsInfo) -> Result<(TypeEquations, ErrorHint), CallEquationSolveError> {
        if let Some(trait_spec) = &associated_eq.trait_gen {
            if trait_spec.trait_id != self.get_trait_id() {
                return Err(CallEquationSolveError::Error(ErrorComment::empty(format!("trait_id is not matched"))))
            }
        }
        let mut impl_equs = TypeEquations::new();
        let self_type = associated_eq.tag.generate_type_variable("SelfType", 0, &mut impl_equs);
        impl_equs.set_self_type(Some(self_type.clone()));

        impl_equs.add_equation(self_type.clone(), associated_eq.caller_type.as_ref().clone(), ErrorComment::empty(format!("self type equals to caller type")));

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), associated_eq.tag.generate_type_variable("Generics", i, &mut impl_equs)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);

        let self_trait_gen = self.trait_spec.generate_trait_generics(&mut impl_equs, trs, &gen_mp).unwrap();
        if let Some(ref trait_gen) = associated_eq.trait_gen {
            for (i, (self_g, right_g)) in self_trait_gen.generics.iter().zip(trait_gen.generics.iter()).enumerate() {
                impl_equs.add_equation(self_g.clone(), right_g.clone(), ErrorComment::empty(format!("{}-th trait generics param equation", i)));
            }
        }

        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut impl_equs, trs).unwrap();
        impl_equs.add_equation(impl_ty, self_type.clone(), ErrorComment::empty(format!("self type equals to impl type")));
        self.where_sec.regist_equations(&gen_mp, &mut impl_equs, trs, &self.hint())
            .map_err(|e| CallEquationSolveError::Error(e))?;
        let associated_type = self.asso_defs
            .get(&associated_eq.associated_type_id)
            .ok_or(ErrorComment::empty(format!("require methods doesnt have {:?}", associated_eq.associated_type_id)))
                .map_err(|e| CallEquationSolveError::Error(ErrorUnify::new(format!(""), self.hint(), e)))?
            .generics_to_type(&gen_mp, &mut impl_equs, trs)
                .map_err(|e| CallEquationSolveError::Error(e))?;
        let return_ty = associated_eq.tag.generate_type_variable("ReturnType", 0, &mut impl_equs);
        impl_equs.add_equation(return_ty, associated_type, ErrorComment::empty(format!("type variable for return type")));
        match impl_equs.unify(trs) {
            Err(UnifyErr::Contradiction(err)) => {
                Err(CallEquationSolveError::Error(ErrorUnify::new(format!(""), self.hint(), err)))
            }
            Ok(_) | Err(UnifyErr::Deficiency(_)) => {
                Ok((impl_equs, self.hint()))
            }
        }
    }

    pub fn match_impl_for_ty(&self, trait_gen: &TraitGenerics, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();
        equs.set_self_type(Some(ty.clone()));

        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| (id.clone(), self.trait_spec.get_tag().generate_type_variable("Generics", i, &mut equs)))
            .collect::<HashMap<_, _>>();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        
        let self_trait_gen = self.trait_spec.generate_trait_generics(&mut equs, trs, &gen_mp).unwrap();
        for (i, (self_g, right_g)) in self_trait_gen.generics.into_iter().zip(trait_gen.generics.iter()).enumerate() {
            equs.add_equation(self_g, right_g.clone(), ErrorComment::empty(format!("{}-th trait generics equation", i)));
        }

        let impl_ty = self.impl_ty.generics_to_type(&gen_mp, &mut equs, trs).unwrap();
        equs.add_equation(ty.clone(), impl_ty, ErrorComment::empty(format!("impl type equals to call type")));
        if self.where_sec.regist_equations(&gen_mp, &mut equs, trs, &self.without_member_range.hint("impl defined", ErrorHint::None)).is_ok() {
            //equs.debug();
            equs.unify(trs).ok().map(|_| SubstsMap::new(equs.take_substs()))
        }
        else {
            None
        }
    }

    pub fn get_associated_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, subst: &SubstsMap) -> Type {
        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get_from_tag(&self.trait_spec.get_tag(), "Generics", i)?)))
            .collect::<Result<HashMap<_, _>, Error>>().unwrap();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        self.asso_defs.get(asso_id).unwrap().generics_to_type(&gen_mp, equs, trs).unwrap()
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        let gen_mp = self.generics.iter().enumerate().map(|(i, id)| Ok((id.clone(), subst.get_from_tag(&self.trait_spec.get_tag(), "Generics", i)?)))
            .collect::<Result<HashMap<_, _>, Error>>().unwrap();
        let mp = GenericsTypeMap::empty();
        let gen_mp = mp.next(gen_mp);
        let before_self_type = equs.set_self_type(Some(ty.clone()));
        let func_ty = self.require_methods.get(&method_id).unwrap().generate_type(&gen_mp, equs, trs, &method_id.id, &self.without_member_range.hint("impl defined", ErrorHint::None)).unwrap();
        let res = match func_ty {
            Type::Func(args, ret, FuncTypeInfo::None) => {
                let tag = Tag::new();
                let alpha = tag.generate_type_variable("SelfType", 0, equs);
                equs.add_equation(alpha, ty.clone(), ErrorComment::empty(format!("self type equals to call type")));
                let self_trait_gen = self.trait_spec.generate_trait_generics(equs, trs, &gen_mp).unwrap();
                let generics_cnt = self_trait_gen.generics.len();
                for (i, g) in self_trait_gen.generics.into_iter().enumerate() {
                    let beta = tag.generate_type_variable("TraitGenerics", i, equs);
                    equs.add_equation(beta, g, ErrorComment::empty(format!("type variable for {}-th generics", i)));
                }
                Type::Func(args, ret, FuncTypeInfo::TraitFunc(self.trait_spec.trait_id.clone(), generics_cnt, tag))
            }
            func_ty => func_ty
        };
        equs.set_self_type(before_self_type);
        res
    }

    pub fn get_trait_id(&self) -> TraitId {
        self.trait_spec.trait_id.clone()
    }

    pub fn debug_str(&self) -> String {
        format!("ImplCandidate {:?} for {:?}", self.trait_spec.trait_id, self.impl_ty)
    }
}


#[derive(Debug, Clone)]
pub struct ParamCandidate {
    pub trait_gen: TraitGenerics,
    pub trait_generics_arg: Vec<TypeId>,
    pub impl_ty: Type,
    pub define_hint: ErrorHint,
    pub asso_defs: HashMap<AssociatedTypeIdentifier, Type>,
    pub require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>,
}

impl ParamCandidate {
    pub fn hint(&self) -> ErrorHint {
        self.define_hint.clone()
    }
    pub fn new(trait_gen: TraitGenerics, trait_generics_arg: Vec<TypeId>, impl_ty: Type, asso_defs: HashMap<AssociatedTypeIdentifier, Type>, require_methods: HashMap<TraitMethodIdentifier, FuncDefinitionInfo>, define_hint: &ErrorHint) -> SelectionCandidate {
        SelectionCandidate::ParamCandidate(ParamCandidate {
            trait_gen, trait_generics_arg, impl_ty, asso_defs, require_methods, define_hint: define_hint.clone(),
        })
    }
    pub fn generate_equations_for_call_equation(&self, call_eq: &CallEquation, trs: &TraitsInfo) -> Result<(TypeEquations, ErrorHint), CallEquationSolveError> {
        if let Some(trait_spec) = &call_eq.trait_gen {
            if trait_spec.trait_id != self.trait_gen.trait_id {
                return Err(CallEquationSolveError::Error(ErrorComment::empty(format!("trait_id is not matched"))))
            }
        }
        let empty_gen_mp = GenericsTypeMap::empty();
        let gen_mp = self.trait_generics_arg.iter().cloned().zip(self.trait_gen.generics.iter().cloned()).collect::<HashMap<_, _>>();
        let gen_mp = empty_gen_mp.next(gen_mp);
        let mut impl_equs = TypeEquations::new();
        let mut func_equs = TypeEquations::new();
        let self_type = call_eq.tag.generate_type_variable("SelfType", 0, &mut impl_equs);
        impl_equs.set_self_type(Some(self_type.clone()));
        func_equs.set_self_type(Some(self_type.clone()));


        if let Some(ref trait_gen) = call_eq.trait_gen {
            for (i, (self_g, right_g)) in self.trait_gen.generics.iter().zip(trait_gen.generics.iter()).enumerate() {
                impl_equs.add_equation(self_g.clone(), right_g.clone(), ErrorComment::empty(format!("{}-th trait generics param equation", i)));
            }
        }

        if let Some(ref caller_type) = call_eq.caller_type {
            impl_equs.add_equation(self_type.clone(), caller_type.as_ref().clone(), ErrorComment::empty(format!("type variable for self type")));
        }

        impl_equs.add_equation(self.impl_ty.clone(), self_type.clone(), ErrorComment::empty(format!("type variable for self type")));
        let func_def = self.require_methods
            .get(&TraitMethodIdentifier { id: call_eq.func_id.clone() })
            .ok_or(ErrorComment::empty(format!("require methods doesnt have {:?}", call_eq.func_id)))
                .map_err(|e| CallEquationSolveError::Error(e))?;
        let func_hint = func_def.hint(&self.hint());
        let func_ty = func_def
            .generate_type(&gen_mp, &mut func_equs, trs, &call_eq.func_id, &self.define_hint)
                .map_err(|e| CallEquationSolveError::Error(e))?;
        let mut not_same_args_length = false;
        match func_ty {
            Type::Func(args, ret, info) => {
                let alpha = call_eq.tag.generate_type_variable("FuncTypeInfo", 0, &mut func_equs);
                let info = match info {
                    FuncTypeInfo::None => {
                        let tag = Tag::new();
                        let alpha = tag.generate_type_variable("SelfType", 0, &mut func_equs);
                        func_equs.add_equation(alpha, self_type.clone(), ErrorComment::empty(format!("type variable for self type")));
                        let generics_cnt = self.trait_gen.generics.len();
                        for (i, g) in self.trait_gen.generics.iter().enumerate() {
                            let beta = tag.generate_type_variable("TraitGenerics", i, &mut func_equs);
                            func_equs.add_equation(beta, g.clone(), ErrorComment::empty(format!("type variable for trait generics")));
                        }
                        FuncTypeInfo::TraitFunc(self.trait_gen.trait_id.clone(), generics_cnt, tag)
                    }
                    info => info,
                };
                func_equs.add_equation(alpha, Type::Func(args.clone(), ret.clone(), info), ErrorComment::empty(format!("type variable for func type info")));
                if args.len() != call_eq.args.len() {
                    not_same_args_length = true;
                }
                if call_eq.caller_type.is_none() {
                    impl_equs.add_equation(args[0].clone(), call_eq.args[0].clone(), ErrorComment::empty(format!("0-th function arg equation")));
                }
                for (i, (l, r)) in args.into_iter().zip(call_eq.args.iter()).enumerate() {
                    func_equs.add_equation(l, r.clone(), ErrorComment::empty(format!("{}-th function arg equation", i)))
                }
                let return_ty = call_eq.tag.generate_type_variable("ReturnType", 0, &mut func_equs);
                func_equs.add_equation(*ret, return_ty, ErrorComment::empty(format!("type variable for return type")));
            }
            _ => unreachable!()
        }

        match impl_equs.unify(trs) {
            Err(UnifyErr::Contradiction(err)) => {
                Err(CallEquationSolveError::ImplOk(ErrorUnify::new(format!(""), self.hint(), err)))
            }
            Ok(_) | Err(UnifyErr::Deficiency(_)) => {
                if not_same_args_length {
                    Err(CallEquationSolveError::ImplOk(ErrorComment::empty(format!("args len is not match"))))
                }
                else {
                    func_equs.take_over_equations(impl_equs);
                    match func_equs.unify(trs) {
                        Err(UnifyErr::Contradiction(err)) => {
                            Err(CallEquationSolveError::ImplOk(ErrorUnify::new(format!(""), func_hint, err)))
                        }
                        Ok(_) | Err(UnifyErr::Deficiency(_)) => {
                            Ok((func_equs, func_hint))
                        }
                    }
                }
            }
        }
    }

    pub fn generate_equations_for_associated_type_equation(&self, associated_eq: &AssociatedTypeEquation, trs: &TraitsInfo) -> Result<(TypeEquations, ErrorHint), CallEquationSolveError> {
        if let Some(trait_spec) = &associated_eq.trait_gen {
            if trait_spec.trait_id != self.get_trait_id() {
                return Err(CallEquationSolveError::Error(ErrorComment::empty(format!("trait_id is not matched"))))
            }
        }
        let mut impl_equs = TypeEquations::new();
        let self_type = associated_eq.tag.generate_type_variable("SelfType", 0, &mut impl_equs);
        impl_equs.set_self_type(Some(self_type.clone()));

        impl_equs.add_equation(self_type.clone(), associated_eq.caller_type.as_ref().clone(), ErrorComment::empty(format!("self type equals to caller type")));

        if let Some(ref trait_gen) = associated_eq.trait_gen {
            for (i, (self_g, right_g)) in self.trait_gen.generics.iter().zip(trait_gen.generics.iter()).enumerate() {
                impl_equs.add_equation(self_g.clone(), right_g.clone(), ErrorComment::empty(format!("{}-th trait generics param equation", i)));
            }
        }

        impl_equs.add_equation(self.impl_ty.clone(), self_type.clone(), ErrorComment::empty(format!("self type equals to impl type")));
        let associated_type = self.asso_defs
            .get(&associated_eq.associated_type_id)
            .ok_or(ErrorComment::empty(format!("require methods doesnt have {:?}", associated_eq.associated_type_id)))
                .map_err(|e| CallEquationSolveError::Error(ErrorUnify::new(format!(""), self.hint(), e)))?;
        let return_ty = associated_eq.tag.generate_type_variable("ReturnType", 0, &mut impl_equs);
        impl_equs.add_equation(return_ty, associated_type.clone(), ErrorComment::empty(format!("type variable for return type")));
        match impl_equs.unify(trs) {
            Err(UnifyErr::Contradiction(err)) => {
                Err(CallEquationSolveError::Error(ErrorUnify::new(format!(""), self.hint(), err)))
            }
            Ok(_) | Err(UnifyErr::Deficiency(_)) => {
                Ok((impl_equs, self.hint()))
            }
        }
    }

    pub fn match_impl_for_ty(&self, trait_gen: &TraitGenerics, ty: &Type, trs: &TraitsInfo) -> Option<SubstsMap> {
        let mut equs = TypeEquations::new();
        let alpha = self.trait_gen.get_tag().generate_type_variable("ImplType", 0, &mut equs);
        for (i, (self_g, right_g)) in self.trait_gen.generics.iter().zip(trait_gen.generics.iter()).enumerate() {
            equs.add_equation(self_g.clone(), right_g.clone(), ErrorComment::empty(format!("{}-th trait generics param equation", i)));
        }
        equs.add_equation(self.impl_ty.clone(), alpha.clone(), ErrorComment::empty(format!("type variable for impl type")));
        equs.add_equation(ty.clone(), alpha, ErrorComment::empty(format!("impl type equals to call type")));
        //equs.debug();
        //equs.unify(trs).ok().map(|_| SubstsMap::new(equs.take_substs()))
        equs.unify(trs).ok().map(|_| SubstsMap::new(equs.take_substs()))
    }
    pub fn get_associated_from_id(&self, _equs: &mut TypeEquations, _trs: &TraitsInfo, asso_id: &AssociatedTypeIdentifier, _subst: &SubstsMap) -> Type {
        self.asso_defs.get(asso_id).unwrap().clone()
    }

    pub fn get_trait_method_from_id(&self, equs: &mut TypeEquations, trs: &TraitsInfo, method_id: &TraitMethodIdentifier, subst: &SubstsMap, ty: &Type) -> Type {
        let before_self_type = equs.set_self_type(Some(subst.get_from_tag(&self.trait_gen.get_tag(), "ImplType", 0).unwrap()));
        let func_ty = self.require_methods.get(method_id).unwrap().generate_type(&GenericsTypeMap::empty(), equs, trs, &method_id.id, &self.define_hint).unwrap();
        let res = match func_ty {
            Type::Func(args, ret, FuncTypeInfo::None) => {
                let tag = Tag::new();
                let alpha = tag.generate_type_variable("SelfType", 0, equs);
                equs.add_equation(alpha, ty.clone(), ErrorComment::empty(format!("type variable for self type")));
                let generics_cnt = self.trait_gen.generics.len();
                for (i, g) in self.trait_gen.generics.iter().enumerate() {
                    let beta = tag.generate_type_variable("TraitGenerics", i, equs);
                    equs.add_equation(beta, g.clone(), ErrorComment::empty(format!("type variable for {}-th trait genreics", i)));
                }
                Type::Func(args, ret, FuncTypeInfo::TraitFunc(self.trait_gen.trait_id.clone(), generics_cnt, tag))
            }
            func_ty => func_ty
        };
        equs.set_self_type(before_self_type);
        res
    }

    pub fn get_trait_id(&self) -> TraitId {
        self.trait_gen.trait_id.clone()
    }

    pub fn debug_str(&self) -> String {
        format!("ParamCandidate {:?} for {:?}", self.trait_gen.trait_id, self.impl_ty)
    }
}
