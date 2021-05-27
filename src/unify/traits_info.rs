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
pub struct TraitsInfo {
    pub typeids: HashMap<TypeId, StructDefinitionInfo>,
    pub traits: HashMap<TraitId, TraitDefinitionInfo>,
    pub impls: Vec<HashMap<TraitId, Vec<SelectionCandidate>>>,
}

impl TraitsInfo {
    pub fn new() -> Self {
        TraitsInfo {
            typeids: vec![
                (TypeId::from_str("i64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("u64"), StructDefinitionInfo::Primitive),
                (TypeId::from_str("bool"), StructDefinitionInfo::Primitive)].into_iter().collect(),
            traits: HashMap::new(),
            impls: vec![HashMap::new()],
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
    pub fn delete_generics_type(&mut self, generics_id: &TypeId) {
        self.typeids.remove(generics_id);
    }

    pub fn check_typeid_exist(&self, id: &TypeId) -> TResult {
       match self.typeids.get(id) {
           Some(def_info) => {
                match *def_info {
                    StructDefinitionInfo::Def(ref def) => {
                        Ok(Type::Generics(id.clone(), (0..def.get_generics_len()).map(|_| equs.get_type_variable()).collect()))
                    }
                    StructDefinitionInfo::Primitive => {
                        Ok(Type::Type(TypeSpec::from_id(id)))
                    }
                    StructDefinitionInfo::Generics => {
                        Ok(Type::Type(TypeSpec::from_id(id)))
                    }
                }
           }
           None => Err(format!("not exist definition: {:?}", id)),
       }
    }
    
    pub fn check_typeid_with_generics(&self, id: TypeId, gens: Vec<Type>) -> TResult {
        match self.typeids.get(&id) {
            Some(def_info) => {
                match *def_info {
                    StructDefinitionInfo::Def(ref def) => {
                        if def.get_generics_len() == gens.len() {
                            let gens = gens.into_iter().map(|ty| ty.check_typeid(self)).collect::<Result<Vec<_>, _>>()?;
                            Ok(Type::Generics(id, gens))
                        }
                        else {
                            Err(format!("type {:?} has {:?} generics but not match to {:?}", id, def.get_generics_len(), gens))
                        }
                    }
                    StructDefinitionInfo::Primitive => {
                        Err(format!("primitive type {:?} doesnt have generics argument", id))
                    }
                    StructDefinitionInfo::Generics => {
                        Err(format!("generics type {:?} doesnt have generics argument", id))
                    }
                }
            }
            None => Err(format!("not exist definition: {:?}", id)),
        }
    }

    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), String> {
        let (trait_id, trait_def) = tr.get_trait_id_pair();
        self.traits.insert(trait_id.clone(), trait_def)
            .map_or(Ok(()), |_| Err(format!("trait {:?} is already defined", trait_id)))
    }
    fn regist_selection_candidate(&mut self, trait_id: &TraitId, cand: SelectionCandidate) {
        match self.impls.last_mut().unwrap().get_mut(&trait_id) {
            Some(v) => {
                v.push(cand);
            }
            None => {
                self.impls.last_mut().unwrap().insert(trait_id.clone(), vec![cand]);
            }
        }
    }

    pub fn regist_impl_candidate(&mut self, ti: &ImplDefinition) -> Result<(), String> {
        let (trait_id, cand) = ti.get_impl_trait_pair();
        self.regist_selection_candidate(&trait_id, cand);
        match self.traits.get(&trait_id) {
            None => Err(format!("trait {:?} is not defined", trait_id)),
            Some(tr) => {
                for (id, info) in tr.required_methods.iter() {
                    match ti.require_methods.get(id) {
                        None => Err(format!("method {:?}::{:?} is not defined for {:?}", tr, id, ti.impl_ty))?,
                        Some(impl_method) => {
                            {
                                let mut equs = TypeEquations::new();
                                let impl_ty = ti.impl_ty.gen_type(&mut equs)?;
                                equs.set_self_type(Some(impl_ty));
                                info.check_equal(&impl_method.get_func_info().1, &mut equs, self)?;
                            }
                        }
                    }
                }
                Ok(())
            }
        }
    }
    pub fn regist_param_candidate(&mut self, _equs: &mut TypeEquations, ty_spec: &TypeSpec, trait_id: &TraitId) -> Result<(), String> {
        match self.traits.get(trait_id) {
            None => Err(format!("trait {:?} is not defined", trait_id)),
            Some(tr_def) => {
                let cand = ParamCandidate::new(trait_id.clone(), Type::Type(ty_spec.clone()), tr_def.asso_ids.iter().map(|asso_id| {
                    (asso_id.clone(), Type::Type(TypeSpec::Associated(
                        Box::new(ty_spec.clone()), AssociatedType { trait_id: trait_id.clone(), type_id: asso_id.clone() }
                        )),
                    )
                }).collect(),
                tr_def.required_methods.clone());
                self.regist_selection_candidate(trait_id, cand);
                Ok(())
            }
        }
    }

    pub fn into_scope(&mut self) {
        self.impls.push(HashMap::new());
    }

    pub fn out_scope(&mut self) {
        self.impls.pop();
    }

    pub fn match_to_impls_for_type(&self, trait_id: &TraitId, ty: &Type) -> Vec<(Vec<TypeSubst>, &SelectionCandidate)> {
        let mut ans = Vec::new();
        for impls in self.impls.iter() {
            if let Some(impls) = impls.get(trait_id) {
                let mut vs = impls.iter()
                    .map(|impl_trait| {
                        impl_trait.match_impl_for_ty(&ty, self)
                    })
                    .filter_map(|x| x)
                    .collect::<Vec<_>>();
                ans.append(&mut vs);
            }
        }
        ans
    }
}
