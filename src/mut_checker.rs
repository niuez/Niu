use std::collections::HashMap;
use crate::trans::TypeAnnotation;
use crate::identifier::Identifier;


#[derive(Debug, Clone, Copy)]
pub enum MutResult {
    NotMut,
    Mut,
    NoType
}

pub trait MutCheck {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String>;
}

#[derive(Debug)]
pub struct VariablesInfo {
    is_mut: Vec<HashMap<Identifier, bool>>,
}

impl VariablesInfo {
    pub fn new() -> Self {
        Self { is_mut: Vec::new() }
    }
    pub fn into_scope(&mut self) {
        self.is_mut.push(HashMap::new());
    }
    pub fn out_scope(&mut self) {
        self.is_mut.pop();
    }
    pub fn regist_variable(&mut self, var: &Identifier, is_mut: bool) {
        self.is_mut.last_mut().unwrap().insert(var.clone(), is_mut);
    }
    pub fn find_variable(&self, var: &Identifier) -> Result<MutResult, String> {
        for is_mut in self.is_mut.iter().rev() {
            if let Some(res) = is_mut.get(var) {
                return Ok(
                    if *res { MutResult::Mut }
                    else { MutResult::NotMut }
                    );
            }
        }
        Err(format!("not found variable {:?}", var))
    }
}
