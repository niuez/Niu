use std::collections::HashMap;

use crate::traits::*;
use crate::unify::*;

#[derive(Debug)]
pub struct TraitsInfo {
    pub traits: HashMap<TraitId, TraitDefinition>,
    pub impls: Vec<HashMap<TraitId, Vec<ImplTrait>>>,

}

impl TraitsInfo {
    pub fn new() -> Self {
        TraitsInfo {
            traits: HashMap::new(),
            impls: vec![HashMap::new()],
        }
    }
    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), String> {
        let (trait_id, trait_def) = tr.get_trait_id_pair();
        self.traits.insert(trait_id.clone(), trait_def);
        self.impls.last_mut().unwrap().insert(trait_id.clone(), Vec::new())
            .map_or(Err(format!("trait {:?} is already defined", trait_id)), |_| Ok(()))
    }
    pub fn regist_trait_impl(&mut self, ti: &ImplTrait) -> Result<(), String> {
        let (trait_id, trait_impl) = ti.get_impl_trait_pair();
        if self.traits.get(&trait_id).is_none() {
            Err(format!("trait {:?} is not defined", trait_id))
        }
        else {
            match self.impls.last_mut().unwrap().get_mut(&trait_id) {
                Some(v) => {
                    v.push(trait_impl);
                }
                None => {
                    self.impls.last_mut().unwrap().insert(trait_id, vec![trait_impl]);
                }
            }
            Ok(())
        }
    }

    pub fn into_scope(&mut self) {
        self.impls.push(HashMap::new());
    }

    pub fn out_scope(&mut self) {
        self.impls.pop();
    }

    pub fn match_to_impls_for_type(&self, trait_id: &TraitId, ty: &Type) -> Vec<(Vec<TypeSubst>, &ImplTrait)> {
        let mut ans = Vec::new();
        for impls in self.impls.iter() {
            let mut vs = impls.get(trait_id).unwrap().iter()
                .map(|impl_trait| {
                    impl_trait.match_impl_for_ty(&ty, self)
                })
                .filter_map(|x| x)
                .collect::<Vec<_>>();
            ans.append(&mut vs);
        }
        ans
    }
}

