use std::collections::HashMap;

use crate::structs::*;
use crate::traits::*;
use crate::type_spec::*;
use crate::unify::*;
use crate::type_id::*;
use crate::func_definition::*;

#[derive(Debug, Clone)]
pub enum StructDefinitionInfo {
    Def(StructDefinition),
    Generics,
    Primitive
}

#[derive(Debug)]
pub struct TraitsInfo<'a> {
    typeids: HashMap<TypeId, StructDefinitionInfo>,
    pub traits: HashMap<TraitId, TraitDefinitionInfo>,
    pub impls: HashMap<TraitId, Vec<SelectionCandidate>>,
    upper_info: Option<&'a TraitsInfo<'a>>,
}

impl<'a> TraitsInfo<'a> {
    pub fn new() -> Self {
        TraitsInfo {
            typeids: vec![
                (TypeId::from_str("i64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("u64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("bool"), StructDefinitionInfo::Primitive)].into_iter().collect(),
            traits: HashMap::new(),
            impls: HashMap::new(),
            upper_info: None,
        }
    }
    pub fn into_scope(&'a self) -> Self {
        TraitsInfo {
            typeids: HashMap::new(),
            traits: HashMap::new(),
            impls: HashMap::new(),
            upper_info: Some(self),
        }
    }
    pub fn regist_structs_info(&mut self, st: &StructDefinition) -> Result<(), String> {
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
    
    pub fn check_typeid_with_generics(&self, id: TypeId, gens: Vec<Type>, top_trs: &Self) -> TResult {
        //println!("id = {:?}", id);
        //println!("typeids = {:?}", self.typeids);
        if let Some(def_info) = self.typeids.get(&id) {
            match *def_info {
                StructDefinitionInfo::Def(ref def) => {
                    if def.get_generics_len() == gens.len() {
                        Ok(Type::Generics(id, gens))
                    }
                    else if gens.len() == 0 && def.get_generics_len() > 0 {
                        let gens = (0..def.get_generics_len()).map(|i| id.id.generate_type_variable(i)).collect();
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
            trs.check_typeid_with_generics(id, gens, top_trs)
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
            trs.check_typeid_with_generics(id, gens, top_trs)
        }
        else {
            Err(format!("not exist definition: {:?}", id))
        }
    }

    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), String> {
        let (trait_id, trait_def) = tr.get_trait_id_pair();
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
        match self.get_traitinfo(trait_id) {
            None => Err(format!("trait {:?} is not defined", trait_id)),
            Some(tr_def) => {
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
