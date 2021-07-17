use std::collections::{ HashSet, HashMap };

use crate::identifier::*;
use crate::structs::*;
use crate::traits::*;
use crate::type_spec::*;
use crate::unify::*;
use crate::type_id::*;

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
    upper_info: Option<&'a TraitsInfo<'a>>,
}

impl<'a> TraitsInfo<'a> {
    pub fn new() -> Self {
        TraitsInfo {
            typeids: vec![
                (TypeId::from_str("i64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("u64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("bool"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("void"), StructDefinitionInfo::Primitive),
                ].into_iter().collect(),
            traits: HashMap::new(),
            impls: HashMap::new(),
            self_impls: HashMap::new(),
            member_to_traits: HashMap::new(),
            member_to_self_impls: HashMap::new(),
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
            upper_info: Some(self),
        }
    }
    pub fn regist_structs_info(&mut self, st: &StructMemberDefinition) -> Result<(), String> {
        let id = st.get_id();
        match self.typeids.insert(id.clone(), StructDefinitionInfo::Def(st.clone())) {
            Some(_) => Err(format!("duplicate struct definition: {:?}", id)),
            None => Ok(()),
        }
    }
    pub fn regist_generics_type(&mut self, generics_id: &TypeId) -> Result<(), String> {
        match self.typeids.insert(generics_id.clone(), StructDefinitionInfo::Generics) {
            Some(_) => Err(format!("duplicate generics definition: {:?}", generics_id)),
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

    pub fn get_struct_definition_info(&self, id: &TypeId) -> Result<&StructDefinitionInfo, String> {
        if let Some(def_info) = self.typeids.get(id) {
            Ok(def_info)
        }
        else if let Some(trs) = self.upper_info {
            trs.get_struct_definition_info(id)
        }
        else {
            Err(format!("type id {:?} is not found", id))
        }
    }
    
    pub fn check_typeid_with_generics(&self, equs: &mut TypeEquations, id: TypeId, gens: Vec<Type>, top_trs: &Self) -> TResult {
        //println!("id = {:?}", id);
        //println!("typeids = {:?}", self.typeids);
        if let Some(def_info) = self.typeids.get(&id) {
            match *def_info {
                StructDefinitionInfo::Def(ref def) => {
                    if def.get_generics_len() == gens.len() {
                        Ok(Type::Generics(id, gens))
                    }
                    else if gens.len() == 0 && def.get_generics_len() > 0 {
                        let gens = (0..def.get_generics_len()).map(|i| id.id.generate_type_variable("Generics", i, equs)).collect();
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(format!("type {:?} has {:?} generics but not match to {:?}", id, def.get_generics_len(), gens))
                    }
                }
                StructDefinitionInfo::Primitive => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(format!("primitive type {:?} doesnt have generics argument", id))
                    }
                }
                StructDefinitionInfo::Generics => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(format!("primitive type {:?} doesnt have generics argument", id))
                    }
                }
            }
        }
        else if let Some(trs) = self.upper_info {
            trs.check_typeid_with_generics(equs, id, gens, top_trs)
        }
        else {
            Err(format!("not exist definition: {:?}", id))
        }
    }

    pub fn check_typeid_no_auto_generics(&self, id: TypeId, gens: Vec<Type>, top_trs: &Self) -> TResult {
        //println!("id = {:?}", id);
        //println!("typeids = {:?}", self.typeids);
        if let Some(def_info) = self.typeids.get(&id) {
            match *def_info {
                StructDefinitionInfo::Def(ref def) => {
                    if def.get_generics_len() == gens.len() {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(format!("type {:?} has {:?} generics but not match to {:?}", id, def.get_generics_len(), gens))
                    }
                }
                StructDefinitionInfo::Primitive => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(format!("primitive type {:?} doesnt have generics argument", id))
                    }
                }
                StructDefinitionInfo::Generics => {
                    if gens.len() == 0 {
                        Ok(Type::Generics(id, gens))
                    }
                    else {
                        Err(format!("primitive type {:?} doesnt have generics argument", id))
                    }
                }
            }
        }
        else if let Some(trs) = self.upper_info {
            trs.check_typeid_no_auto_generics(id, gens, top_trs)
        }
        else {
            Err(format!("not exist definition: {:?}", id))
        }
    }

    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), String> {
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
            .map_or(Ok(()), |_| Err(format!("trait {:?} is already defined", trait_id)))
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

    pub fn regist_self_impl(&mut self, def: &ImplSelfDefinition) -> Result<(), String> {
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
            StructDefinitionInfo::Primitive => Err(format!("cant impl self for primitive type")),
            StructDefinitionInfo::Generics => Err(format!("cant impl self for primitive type")),
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
    

    pub fn regist_impl_candidate(&mut self, ti: &ImplDefinition) -> Result<(), String> {
        let (trait_id, cand) = ti.get_impl_trait_pair();
        self.regist_selection_candidate(&trait_id, cand);
        let mut gen_trs = self.into_scope();
        for id in ti.generics.iter() {
            gen_trs.regist_generics_type(id)?;
        }
        let mut equs = TypeEquations::new();
        let impl_ty = ti.impl_ty.generics_to_type(&GenericsTypeMap::empty(), &mut equs, &gen_trs)?;
        equs.set_self_type(Some(impl_ty));
        ti.where_sec.regist_candidate(&mut equs, &mut gen_trs)?;

        match self.get_traitinfo(&trait_id) {
            None => Err(format!("trait {:?} is not defined", trait_id)),
            Some(tr) => {
                {
                    tr.where_sec.regist_equations(&GenericsTypeMap::empty(), &mut equs, self)?;
                    match equs.unify(self) {
                        Ok(_) => Ok(()),
                        Err(UnifyErr::Deficiency(s)) => Err(s),
                        Err(UnifyErr::Contradiction(s)) => Err(s),
                    }?;
                }
                for (id, info) in tr.required_methods.iter() {
                    match ti.require_methods.get(id) {
                        None => Err(format!("method {:?}::{:?} is not defined for {:?}", tr, id, ti.impl_ty))?,
                        Some(impl_method) => {
                            {
                                equs.clear_equations();
                                info.check_equal(&impl_method.get_func_info().1, &mut equs, &gen_trs)?;
                            }
                        }
                    }
                }
                Ok(())
            }
        }
    }
    pub fn regist_param_candidate(&mut self, ty: Type, trait_id: &TraitId, mut asso_mp: HashMap<AssociatedTypeIdentifier, Type>) -> Result<(), String> {
        println!("param {:?}, {:?}", ty, trait_id);
        match self.get_traitinfo(trait_id).cloned() {
            None => Err(format!("trait {:?} is not defined", trait_id)),
            Some(tr_def) => {
                let mut equs = TypeEquations::new();
                equs.set_self_type(Some(ty.clone()));
                tr_def.where_sec.regist_candidate(&equs, self)?;
                let asso_tys = tr_def.asso_ids.iter().map(|asso_id| {
                    let asso_ty = match asso_mp.remove(asso_id) {
                        Some(asso_ty) => asso_ty,
                        None => Type::SolvedAssociatedType(Box::new(ty.clone()), AssociatedType { trait_id: trait_id.clone(), type_id: asso_id.clone() }),
                    };
                    (asso_id.clone(), asso_ty)
                }).collect::<HashMap<_, _>>();
                if asso_mp.len() > 0 {
                    Err(format!("undefined associated type speficier: {:?}", asso_mp))
                }
                else {
                    let cand = ParamCandidate::new(trait_id.clone(), ty.clone(), asso_tys, tr_def.required_methods.clone());
                    self.regist_selection_candidate(trait_id, cand);
                    Ok(())
                }
            }
        }
    }

    fn match_to_impls(&self, trait_id: &TraitId, ty: &Type, top_trs: &Self) -> Vec<(SubstsMap, &SelectionCandidate)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.impls.get(trait_id) {
            let mut vs = impls.iter()
                .map(|impl_trait| {
                    impl_trait.match_impl_for_ty(&ty, top_trs)
                })
            .filter_map(|x| x)
                .collect::<Vec<_>>();
            ans.append(&mut vs);
        }

        if let Some(trs) = self.upper_info {
            let mut vs = trs.match_to_impls(trait_id, ty, top_trs);
            ans.append(&mut vs);
        }
        ans
    }

    pub fn match_to_impls_for_type(&self, trait_id: &TraitId, ty: &Type) -> Vec<(SubstsMap, &SelectionCandidate)> {
        self.match_to_impls(trait_id, ty, self)
    }

    fn match_to_self_impls(&self, typeid: &TypeId, ty: &Type, top_trs: &Self) -> Vec<(SubstsMap, &SelectionCandidate)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.self_impls.get(typeid) {
            let mut vs = impls.iter()
                .map(|impl_trait| {
                    impl_trait.match_impl_for_ty(&ty, top_trs)
                })
            .filter_map(|x| x)
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

    fn generate_call_equations_for_trait(&self, trait_id: &TraitId, call_eq: &CallEquation, top_trs: &Self) -> Vec<(TypeEquations, &SelectionCandidate)> {
        let mut ans = Vec::new();
        if let Some(impls) = self.impls.get(trait_id) {
            let mut vs = impls.iter()
                .map(|impl_trait| {
                    impl_trait.generate_equations_for_call_equation(call_eq, top_trs).ok().map(|eq| (eq, impl_trait))
                })
                .filter_map(|x| x)
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
                    impl_trait.generate_equations_for_call_equation(call_eq, top_trs).ok().map(|eq| (eq, impl_trait))
                })
                .filter_map(|x| x)
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
        let mut st = HashSet::new();
        self.search_traits_for_member(&call_eq.func_id, &mut st);
        let mut gen_equs = Vec::new();
        for t in st.into_iter() {
            let mut vs = self.generate_call_equations_for_trait(&t, call_eq, self);
            gen_equs.append(&mut vs);
        }

        let mut st = HashSet::new();
        self.search_typeid_for_member(&call_eq.func_id, &mut st);
        for t in st.into_iter() {
            let mut vs = self.generate_call_equations_for_self_type(&t, call_eq, self);
            gen_equs.append(&mut vs);
        }

        let mut unify_res = gen_equs.into_iter().map(|(mut gen_equ, cand)| {
            match gen_equ.unify(self) {
                Ok(_) => {
                    Ok((gen_equ, cand))
                }
                Err(UnifyErr::Deficiency(_)) => {
                    Ok((gen_equ, cand))
                }
                Err(UnifyErr::Contradiction(st)) => {
                    Err(st)
                }
            }
        }).filter_map(|x| x.ok()).collect::<Vec<_>>();
        if unify_res.len() == 1 {
            let (gen_equ, _) = unify_res.pop().unwrap();
            let ret_ty = gen_equ.try_get_substs(TypeVariable::Counter(call_eq.tag.get_num(), "ReturnType", 0));

            //println!("take over by call >> ");
            //println!("{:?}", call_eq);
            //println!("ret = {:?}", ret_ty);
            //println!(">> ");
            equs.take_over_equations(gen_equ);
            Ok(ret_ty)
        }
        else {
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
        let mut st = HashSet::new();
        self.search_traits_for_member(mem_id, &mut st);
        let mut ans = Vec::new();
        for t in st.into_iter() {
            let mut vs = self.match_to_impls_for_type(&t, ty);
            ans.append(&mut vs);
        }

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
