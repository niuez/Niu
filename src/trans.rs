use std::collections::HashMap;

use crate::unify::*;


#[derive(Debug)]
pub struct TypeAnnotation {
    theta: HashMap<usize, Type>,
    cnt: usize,
}

impl TypeAnnotation {
    pub fn new() -> Self {
        Self { theta: HashMap::new(), cnt: 0 }
    }
    pub fn insert(&mut self, tv: TypeVariable, t: Type) {
        let TypeVariable::Counter(i) = tv;
        self.theta.insert(i, t);
    }
    pub fn annotation(&self) -> &Type {
        self.theta.get(&self.cnt).unwrap()
    }
    pub fn count(&mut self) {
        self.cnt += 1;
    }
}

pub trait Transpile {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String;
}
