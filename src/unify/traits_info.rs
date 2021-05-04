use std::collections::HashMap;

use crate::traits::*;
use crate::type_spec::*;
use crate::unify::*;
use crate::func_definition::*;

#[derive(Debug)]
pub struct TraitsInfo {
    pub traits: HashMap<TraitId, TraitDefinitionInfo>,
    pub impls: Vec<HashMap<TraitId, Vec<SelectionCandidate>>>,

}

impl TraitsInfo {
    pub fn new() -> Self {
        TraitsInfo {
            traits: HashMap::new(),
            impls: vec![HashMap::new()],
        }
    }
    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), String> {
        println!("tr = {:?}", tr);
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
            println!("impl get {:?}", impls.get(trait_id));
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
