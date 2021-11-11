use std::collections::{ HashSet, HashMap };

use crate::identifier::*;
use crate::structs::*;
use crate::traits::*;
use crate::type_spec::*;
use crate::unify::*;
use crate::type_id::*;
use crate::error::*;

#[derive(Debug, Clone)]
pub enum StructDefinitionInfo {
    Def(StructMemberDefinition),
    Generics,
    Primitive
}

#[derive(Debug)]
pub struct TraitsInfo<'a> {
    typeids: HashMap<TypeId, StructDefinitionInfo>,
    pub traits: HashMap<TraitId, TraitDefinitionInfo>,
    pub impls: HashMap<TraitId, Vec<SelectionCandidate>>,
    self_impls: HashMap<TypeId, Vec<SelectionCandidate>>,
    member_to_traits: HashMap<Identifier, HashSet<TraitId>>,
    member_to_self_impls: HashMap<Identifier, HashSet<TypeId>>,
    depth: usize,
    upper_info: Option<&'a TraitsInfo<'a>>,
}

#[derive(Debug, Clone)]
pub enum CallEquationSolveError {
    Error(Error),
    ImplOk(Error),
    Ok(Error),
}


pub fn select_impls_by_priority<I: Iterator<Item=usize>>(priorities: I, len: usize) -> Option<usize> {
    let idx = priorities.into_iter().enumerate().max_by_key(|(_, x)| *x);
    match idx {
        Some((_idx, pri)) if pri == 0 && len > 1 => None,
        Some((idx, _)) => Some(idx),
        None => None,
    }
}

impl<'a> TraitsInfo<'a> {
    pub fn new() -> Self {
        TraitsInfo {
            typeids: vec![
                (TypeId::from_str("i64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("u64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("f64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("char"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("bool"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("void"), StructDefinitionInfo::Primitive),
                ].into_iter().collect(),
            traits: HashMap::new(),
            impls: HashMap::new(),
            self_impls: HashMap::new(),
            member_to_traits: HashMap::new(),
            member_to_self_impls: HashMap::new(),
            depth: 0,
            upper_info: None,
        }
    }
    pub fn into_scope(&'a self) -> Self {
        TraitsInfo {
            typeids: HashMap::new(),
            traits: HashMap::new(),
            impls: HashMap::new(),
            self_impls: HashMap::new(),
            member_to_traits: HashMap::new(),
            member_to_self_impls: HashMap::new(),
            depth: self.depth + 1,
            upper_info: Some(self),
        }
    }
    pub fn regist_structs_info(&mut self, st: &StructMemberDefinition) -> Result<(), Error> {
        let id = st.get_id();
        match self.typeids.insert(id.clone(), StructDefinitionInfo::Def(st.clone())) {
            Some(_) => Err(ErrorComment::empty(format!("duplicate struct definition: {:?}", id))),
            None => Ok(()),
        }
    }
    pub fn regist_generics_type(&mut self, generics_id: &TypeId) -> Result<(), Error> {
        match self.typeids.insert(generics_id.clone(), StructDefinitionInfo::Generics) {
            Some(_) => Err(ErrorComment::empty(format!("duplicate generics definition: {:?}", generics_id))),
            None => Ok(()),
        }
    }

    pub fn check_typeid_exist(&self, _id: &TypeId) -> TResult {
        unreachable!("check_typeid_exist");
        /* if let Some(def_info) = self.typeids.get(id) {
            match *def_info {
                StructDefinitionInfo::Def(ref def) => {
                    Ok(Type::Generics(id.clone(), (0..def.get_generics_len()).map(|i| id.id.generate_type_variable(i)).collect()))
                }
                StructDefinitionInfo::Primitive => {
                    Ok(Type::Generics(id.clone(), vec![]))
                }
                StructDefinitionInfo::Generics => {
                    Ok(Type::Generics(id.clone(), vec![]))
                }
            }
        }
        else if let Some(trs) = self.upper_info {
            trs.check_typeid_exist(id)
        }
        else {
            Err(format!("not exist definition: {:?}", id))
        }*/
    }

    pub fn get_struct_definition_info(&self, id: &TypeId) -> Result<&StructDefinitionInfo, Error> {
        if let Some(def_info) = self.typeids.get(id) {
            Ok(def_info)
        }
        else if let Some(trs) = self.upper_info {
            trs.get_struct_definition_info(id)
        }
        else {
            Err(ErrorComment::empty(format!("type id {:?} is not found", id)))
        }
    }
    
    pub fn check_typeid_with_generics(&self, equs: &mut TypeEquations, id: TypeId, gens: Vec<Type>, top_trs: &Self) -> TResult {
        //log::debug!("id = {:?}", id);
        //log::debug!("typeids = {:?}", self.typeids);
        if let Some(def_info) = self.typeids.get(&id) {
            match *def_info {
                StructDefinitionInfo::Def(ref def) => {
                    if def.get_generics_len() == gens.len() {
                        let gen_mp = GenericsTypeMap::empty();
                        let mp = def.generics.iter().cloned().zip(gens.iter().cloned()).collect::<HashMap<_, _>>();
                        let mp = gen_mp.next(mp);
                        def.where_sec.regist_equations(&mp, equs, self, &def.without_member_range.hint("struct definition", ErrorHint::None))?;
                        Ok(Type::Generics(id, gens))
                    }
                    else if gens.len() == 0 && def.get_generics_len() > 0 {
                        let gens: Vec<_> = (0..def.get_generics_len()).map(|i| id.id.generate_type_variable("Generics", i, equs)).collect();
                        let gen_mp = GenericsTypeMap::empty();
                        let mp = def.generics.iter().cloned().zip(gens.iter().cloned()).collect::<HashMap<_, _>>();
                        let mp = gen_mp.next(mp);
                        def.where_sec.regist_equations(&mp, equs, self, &def.without_member_range.hint("struct definition", ErrorHint::None))?;
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(ErrorComment::empty(format!("type {:?} has {:?} generics but not match to {:?}", id, def.get_generics_len(), gens)))
                    }
                }
                StructDefinitionInfo::Primitive => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(ErrorComment::empty(format!("primitive type {:?} doesnt have generics argument", id)))
                    }
                }
                StructDefinitionInfo::Generics => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(ErrorComment::empty(format!("primitive type {:?} doesnt have generics argument", id)))
                    }
                }
            }
        }
        else if let Some(trs) = self.upper_info {
            trs.check_typeid_with_generics(equs, id, gens, top_trs)
        }
        else {
            Err(ErrorComment::empty(format!("not exist definition: {:?}", id)))
        }
    }

    pub fn check_typeid_no_auto_generics(&self, id: TypeId, gens: Vec<Type>, top_trs: &Self) -> TResult {
        //log::debug!("id = {:?}", id);
        //log::debug!("typeids = {:?}", self.typeids);
        if let Some(def_info) = self.typeids.get(&id) {
            match *def_info {
                StructDefinitionInfo::Def(ref def) => {
                    if def.get_generics_len() == gens.len() {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(ErrorComment::empty(format!("type {:?} has {:?} generics but not match to {:?}", id, def.get_generics_len(), gens)))
                    }
                }
                StructDefinitionInfo::Primitive => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(ErrorComment::empty(format!("primitive type {:?} doesnt have generics argument", id)))
                    }
                }
                StructDefinitionInfo::Generics => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(ErrorComment::empty(format!("primitive type {:?} doesnt have generics argument", id)))
                    }
                }
            }
        }
        else if let Some(trs) = self.upper_info {
            trs.check_typeid_no_auto_generics(id, gens, top_trs)
        }
        else {
            Err(ErrorComment::empty(format!("not exist definition: {:?}", id)))
        }
    }

    pub fn check_trait(&self, tr: &TraitSpec) -> Result<(), Error> {
        match self.traits.get(&tr.trait_id) {
            None => {
                if let Some(trs) = self.upper_info {
                    trs.check_trait(tr)
                }
                else {
                    Err(ErrorComment::empty(format!("trait {:?} not found", tr)))
                }
            }
            Some(tr_def) => {
                if tr_def.generics.len() == tr.generics.len() {
                    Ok(())
                }
                else {
                    Err(ErrorComment::empty(format!("Generics of {:?} is not match to trait {:?}", tr, tr.trait_id)))
                }
            }
        }
    }

    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), Error> {
        let (trait_id, trait_def) = tr.get_trait_id_pair();
        for (id, _) in trait_def.required_methods.iter() {
            match self.member_to_traits.get_mut(&id.id) {
                Some(st) => {
                    st.insert(trait_id.clone());
                }
                None => {
                    self.member_to_traits.insert(id.id.clone(), vec![trait_id.clone()].into_iter().collect());
                }
            }
        }
        self.traits.insert(trait_id.clone(), trait_def)
            .map_or(Ok(()), |_| Err(ErrorComment::empty(format!("trait {:?} is already defined", trait_id))))
    }

    fn regist_selection_candidate(&mut self, trait_id: &TraitId, cand: SelectionCandidate) {
        match self.impls.get_mut(trait_id) {
            Some(v) => {
                v.push(cand);
            }
            None => {
                self.impls.insert(trait_id.clone(), vec![cand]);
            }
        }
    }

    pub fn regist_self_impl(&mut self, def: &ImplSelfDefinition) -> Result<(), Error> {
        let info = def.get_info();
        let typeid = info.impl_ty.get_type_id()?;
        match self.get_struct_definition_info(&typeid)? {
            StructDefinitionInfo::Def(_) => {
                for (func_id, _) in info.require_methods.iter() {
                    match self.member_to_self_impls.get_mut(&func_id) {
                        None => {
                            self.member_to_self_impls.insert(func_id.clone(), std::iter::once(typeid.clone()).collect());
                        }
                        Some(st) => {
                            st.insert(typeid.clone());
                        }
                    }
                }
                match self.self_impls.get_mut(&typeid) {
                    None => {
                        self.self_impls.insert(typeid, vec![SelectionCandidate::ImplSelfCandidate(info)]);
                    }
                    Some(vec) => {
                        vec.push(SelectionCandidate::ImplSelfCandidate(info));
                    }
                }
                Ok(())
            }
            StructDefinitionInfo::Primitive => Err(ErrorComment::empty(format!("cant impl self for primitive type"))),
            StructDefinitionInfo::Generics => Err(ErrorComment::empty(format!("cant impl self for primitive type"))),
        }
    }

    fn get_traitinfo(&self, trait_id: &TraitId) -> Option<&TraitDefinitionInfo> {
        if let Some(info) = self.traits.get(trait_id) {
            Some(info)
        }
        else if let Some(trs) = self.upper_info {
            trs.get_traitinfo(trait_id)
        }
        else {
            None
        }
    }
    pub fn preregist_impl_candidate(&mut self, ti: &ImplDefinition) {
        let (trait_id, cand) = ti.get_impl_trait_pair();
        self.regist_selection_candidate(&trait_id, cand);
    }

    pub fn regist_impl_candidate(&self, equs: &mut TypeEquations, ti: &ImplDefinition) -> Result<(), Error> {
        let (trait_id, _) = ti.get_impl_trait_pair();
        let mut gen_trs = self.into_scope();
        for id in ti.generics.iter() {
            gen_trs.regist_generics_type(id)?;
        }
        let impl_ty = ti.impl_ty.generics_to_type(&GenericsTypeMap::empty(), equs, &gen_trs)?;
        let before_self_type = equs.set_self_type(Some(impl_ty));
        ti.where_sec.regist_candidate(equs, &mut gen_trs, &ti.without_member_range.hint("impl definition", ErrorHint::None))?;
        self.check_trait(&ti.trait_spec)?;

        match self.get_traitinfo(&trait_id) {
            None => Err(ErrorComment::empty(format!("trait {:?} is not defined", trait_id))),
            Some(tr) => {
                let empty_gen_map = GenericsTypeMap::empty();
                let tr_gen_map = tr.generics.iter().zip(ti.trait_spec.generics.iter())
                    .map(|(id, param)| Ok((id.clone(), param.generate_type_no_auto_generics(&equs, &gen_trs)?)))
                    .collect::<Result<HashMap<_, _>, Error>>()?;
                let tr_gen_map = empty_gen_map.next(tr_gen_map);
                log::debug!("{:?}", tr_gen_map);
                {
                    tr.where_sec.regist_equations(&GenericsTypeMap::empty(), equs, &gen_trs, &tr.without_member_range.hint("trait defined", ErrorHint::None))?;
                    match equs.unify(&gen_trs) {
                        Ok(_) => Ok(()),
                        Err(UnifyErr::Deficiency(s)) => Err(ErrorComment::empty(format!("trait {:?} where section error {}", tr.trait_id, s))),
                        Err(UnifyErr::Contradiction(s)) => Err(ErrorComment::new(format!("trait {:?} where section error", tr.trait_id), s)),
                    }?;
                }
                for (id, info) in tr.required_methods.iter() {
                    match ti.require_methods.get(id) {
                        None => Err(ErrorComment::empty(format!("method {:?}::{:?} is not defined for {:?}", tr, id, ti.impl_ty)))?,
                        Some(impl_method) => {
                            {
                                equs.clear_equations();
                                info.check_equal(&impl_method.get_func_info().1, equs, &gen_trs, &tr_gen_map, &empty_gen_map)?;
                            }
                        }
                    }
                }
                equs.set_self_type(before_self_type);
                Ok(())
            }
        }
    }
    pub fn regist_param_candidate(&mut self, ty: Type, trait_gen: &TraitGenerics, mut asso_mp: HashMap<AssociatedTypeIdentifier, Type>, define_hint: &ErrorHint) -> Result<(), Error> {
        log::debug!("param {:?}, {:?}", ty, trait_gen);
        match self.get_traitinfo(&trait_gen.trait_id).cloned() {
            None => Err(ErrorComment::empty(format!("trait {:?} is not defined", trait_gen))),
            Some(tr_def) => {
                let mut equs = TypeEquations::new();
                equs.set_self_type(Some(ty.clone()));
                tr_def.where_sec.regist_candidate(&equs, self, &tr_def.without_member_range.hint(&format!("regist candidate by where section of {:?}", trait_gen), define_hint.clone()))?;
                //dbg!(&tr_def.asso_ids);
                let asso_tys = tr_def.asso_ids.iter().map(|asso_id| {
                    let asso_ty = match asso_mp.remove(asso_id) {
                        Some(asso_ty) => asso_ty,
                        None => {
                            match self.match_to_impls_for_type(trait_gen, &ty) {
                                Err(len) if len == 0 => Type::SolvedAssociatedType(Box::new(ty.clone()), trait_gen.clone(), asso_id.clone()),
                                Ok((substs, cand)) => cand.get_associated_from_id(&mut equs, self, asso_id, &substs),
                                Err(_) => unreachable!("regist_param bug")
                            }
                        }
                    };
                    (asso_id.clone(), asso_ty)
                }).collect::<HashMap<_, _>>();
                if asso_mp.len() > 0 {
                    Err(ErrorComment::empty(format!("undefined associated type speficier: {:?}", asso_mp)))
                }
                else {
                    let cand = ParamCandidate::new(trait_gen.clone(), tr_def.generics.clone(), ty.clone(), asso_tys, tr_def.required_methods.clone(), define_hint);
                    self.regist_selection_candidate(&trait_gen.trait_id, cand);
                    Ok(())
                }
            }
        }
    }

    fn match_to_impls(&self, trait_gen: &TraitGenerics, ty: &Type, top_trs: &Self) -> Vec<(SubstsMap, &SelectionCandidate, usize)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.impls.get(&trait_gen.trait_id) {
            let mut vs = impls.iter().enumerate()
                .map(|(i, impl_trait)| {
                    impl_trait.match_impl_for_ty(trait_gen, &ty, top_trs).map(|(a, b)| (a, b, i * self.depth + self.depth * 10000))
                })
            .filter_map(|x| x)
                .collect::<Vec<_>>();
            ans.append(&mut vs);
        }

        if let Some(trs) = self.upper_info {
            let mut vs = trs.match_to_impls(trait_gen, ty, top_trs);
            ans.append(&mut vs);
        }
        ans
    }

    pub fn match_to_impls_for_type(&self, trait_gen: &TraitGenerics, ty: &Type) -> Result<(SubstsMap, &SelectionCandidate), usize> {
        log::debug!("search {:?} {:?}--------------", trait_gen, ty);
        let mut cands = self.match_to_impls(trait_gen, ty, self);
        let idx = select_impls_by_priority(cands.iter().map(|(_, _, i)| *i), cands.len());
        //log::debug!("cands {:?}", cands);
        log::debug!("solve  {:?} {:?} ------------> idx {:?} / {}", trait_gen, ty, idx, cands.len());
        match idx {
            Some(i) => {
                let (substs, cand, _) = cands.swap_remove(i);
                Ok((substs, cand))
            }
            None => {
                Err(cands.len())
            }
        }
    }


    fn match_to_self_impls(&self, typeid: &TypeId, ty: &Type, top_trs: &Self) -> Vec<(SubstsMap, &SelectionCandidate)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.self_impls.get(typeid) {
            let mut vs = impls.iter()
                .map(|impl_trait| {
                    impl_trait.match_self_impl_for_ty(&ty, top_trs)
                })
            .filter_map(|x| {
                x
            })
            .collect::<Vec<_>>();
            ans.append(&mut vs);
        }

        if let Some(trs) = self.upper_info {
            let mut vs = trs.match_to_self_impls(typeid, ty, top_trs);
            ans.append(&mut vs);
        }
        ans
    }

    pub fn match_to_self_impls_for_type(&self, typeid: &TypeId, ty: &Type) -> Vec<(SubstsMap, &SelectionCandidate)> {
        self.match_to_self_impls(typeid, ty, self)
    }

    fn generate_call_equations_for_trait(&self, trait_id: &TraitId, call_eq: &CallEquation, top_trs: &Self) -> Vec<(TypeEquations, &SelectionCandidate, usize)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.impls.get(trait_id) {
            let mut vs = impls.iter().enumerate()
                .map(|(i, impl_trait)| {
                    let res = impl_trait.generate_equations_for_call_equation(call_eq, top_trs);
                    log::debug!("{:?}", res.as_ref().map(|_| impl_trait.debug_str()));
                    res.ok().map(|eq| (eq, impl_trait, i * self.depth + self.depth * 10000))
                })
                .filter_map(|x| {
                    x
                })
                .collect::<Vec<_>>();
            ans.append(&mut vs);
        }

        if let Some(trs) = self.upper_info {
            let mut vs = trs.generate_call_equations_for_trait(trait_id, call_eq, top_trs);
            ans.append(&mut vs);
        }
        ans
    }

    fn generate_call_equations_for_self_type(&self, typeid: &TypeId, call_eq: &CallEquation, top_trs: &Self) -> Vec<(TypeEquations, &SelectionCandidate)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.self_impls.get(typeid) {
            let mut vs = impls.iter()
                .map(|impl_trait| {
                    let res = impl_trait.generate_equations_for_call_equation(call_eq, top_trs);
                    log::debug!("{:?}", res.as_ref().map(|_| impl_trait.debug_str()));
                    res.ok().map(|eq| (eq, impl_trait))
                })
                .filter_map(|x| {
                    x
                })
                .collect::<Vec<_>>();
            ans.append(&mut vs);
        }

        if let Some(trs) = self.upper_info {
            let mut vs = trs.generate_call_equations_for_self_type(typeid, call_eq, top_trs);
            ans.append(&mut vs);
        }
        ans
    }

    pub fn regist_for_call_equtions(&self, equs: &mut TypeEquations, call_eq: &CallEquation) -> Result<Type, Vec<&SelectionCandidate>> {
        log::debug!("------------------------------\nCallEquation Solve {:?}\n{}",
                   call_eq.func_id,
                   call_eq.args.iter().map(|a| format!("{:?}", a)).collect::<Vec<_>>().join("\n")
                   );
        let mut st = HashSet::new();
        self.search_traits_for_member(&call_eq.func_id, &mut st);
        let mut unify_res = Vec::new();
        for t in st.into_iter() {
            let vs = self.generate_call_equations_for_trait(&t, call_eq, self);
            let mut vs = vs.into_iter().filter_map(|(mut equs, cand, pri)| {
                //log::debug!("{:?}", equs);
                let res = equs.unify(self);
                //log::info!("\n[{}] {}\n{:?}", pri, cand.debug_str(), res.as_ref());
                match res {
                    Ok(_) => Some((equs, cand, pri)),
                    Err(UnifyErr::Deficiency(_)) => Some((equs, cand, pri)),
                    Err(UnifyErr::Contradiction(_)) => None
                }
            }).collect::<Vec<_>>();
            let idx = select_impls_by_priority(vs.iter().map(|(_, _, i)| *i), vs.len());
            if let Some(idx) = idx {
                let (equs, cand, _) = vs.swap_remove(idx);
                unify_res.push((equs, cand));
            }
            /* else {
                return Err(vs.into_iter().map(|(_, cand, _)| cand).collect())
            } */
        }

        let mut st = HashSet::new();
        self.search_typeid_for_member(&call_eq.func_id, &mut st);
        for t in st.into_iter() {
            let vs = self.generate_call_equations_for_self_type(&t, call_eq, self);
            for (mut gen_equ, cand) in vs.into_iter() {
                let res = gen_equ.unify(self);
                //log::debug!("\n{}\n{:?}", cand.debug_str(), res.as_ref().err());
                match res {
                    Ok(_) => {
                        unify_res.push((gen_equ, cand));
                    }
                    Err(UnifyErr::Deficiency(_)) => {
                        unify_res.push((gen_equ, cand));
                    }
                    Err(UnifyErr::Contradiction(_st)) => {}
                }
            }
        }
        if unify_res.len() == 1 {
            let (gen_equ, cand) = unify_res.pop().unwrap();
            log::debug!("OK {}\n-------------------------------", cand.debug_str());
            let ret_ty = gen_equ.try_get_substs(TypeVariable::Counter(call_eq.tag.get_num(), "ReturnType", 0));

            //log::debug!("take over by call >> ");
            //log::debug!("{:?}", call_eq);
            //log::debug!("ret = {:?}", ret_ty);
            //log::debug!(">> ");
            equs.take_over_equations(gen_equ);
            Ok(ret_ty)
        }
        else {
            log::debug!("NG-------------------------------------------");
            Err(unify_res.into_iter().map(|(_, cand)| cand).collect())
        }
    }


    fn search_traits_for_member(&self, mem_id: &Identifier, st: &mut HashSet<TraitId>) {
        if let Some(traits) = self.member_to_traits.get(mem_id) {
            for t in traits { st.insert(t.clone()); }
        }
        if let Some(trs) = self.upper_info {
            trs.search_traits_for_member(mem_id, st);
        }
    }

    fn search_typeid_for_member(&self, mem_id: &Identifier, st: &mut HashSet<TypeId>) {
        if let Some(traits) = self.member_to_self_impls.get(mem_id) {
            for t in traits { st.insert(t.clone()); }
        }
        if let Some(trs) = self.upper_info {
            trs.search_typeid_for_member(mem_id, st);
        }
    }


    pub fn match_to_member_for_type(&self, mem_id: &Identifier, ty: &Type) -> Vec<(SubstsMap, &SelectionCandidate)> {
        /* let mut st = HashSet::new();
        self.search_traits_for_member(mem_id, &mut st);
        for t in st.into_iter() {
            let mut vs = self.match_to_impls_for_type(&t, ty);
            ans.append(&mut vs);
        }*/

        let mut ans = Vec::new();
        let mut st = HashSet::new();
        self.search_typeid_for_member(mem_id, &mut st);
        for t in st.into_iter() {
            let mut vs = self.match_to_self_impls_for_type(&t, ty);
            ans.append(&mut vs);
        }
        ans
    }

    pub fn search_typeid(&self, id: &TypeId) -> Result<&StructDefinitionInfo, String> {
        if let Some(def_info) = self.typeids.get(&id) {
            Ok(def_info)
        }
        else if let Some(trs) = self.upper_info {
            trs.search_typeid(&id)
        }
        else {
            Err(format!("not exist definition: {:?}", id))
        }
    }
}
