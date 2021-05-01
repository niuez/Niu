use std::collections::HashMap;

use crate::traits::*;

#[derive(Debug)]
pub struct TraitsInfo {
    pub traits: HashMap<TraitId, TraitDefinition>,
    pub impls: HashMap<TraitId, Vec<ImplTrait>>,
}

impl TraitsInfo {
    pub fn new() -> Self {
        TraitsInfo {
            traits: HashMap::new(),
            impls: HashMap::new(),
        }
    }
    pub fn regist_trait(&mut self, tr: &TraitDefinition) -> Result<(), String> {
        let (trait_id, trait_def) = tr.get_trait_id_pair();
        self.traits.insert(trait_id.clone(), trait_def);
        self.impls.insert(trait_id.clone(), Vec::new())
            .map_or(Err(format!("trait {:?} is already defined", trait_id)), |_| Ok(()))
    }
    pub fn regist_trait_impl(&mut self, ti: &ImplTrait) -> Result<(), String> {
        let (trait_id, trait_impl) = ti.get_impl_trait_pair();
        match self.impls.get_mut(&trait_id) {
            Some(v) => {
                v.push(trait_impl);
                Ok(())
            }
            None => {
                Err(format!("trait {:?} is not defined", trait_id))
            }
        }
    }
}

