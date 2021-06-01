use std::collections::HashMap;

use crate::unary_expr::Variable;
use crate::func_definition::{ FuncDefinition, FuncDefinitionInfo};
use crate::unify::*;


#[derive(Debug)]
pub struct TypeAnnotation {
    func: HashMap<Variable, FuncDefinitionInfo>,
    theta: HashMap<(usize, usize), Type>,
}

impl TypeAnnotation {
    pub fn new() -> Self {
        Self { func: HashMap::new(), theta: HashMap::new(), }
    }
    pub fn insert(&mut self, tv: TypeVariable, t: Type) {
        let TypeVariable::Counter(i, num) = tv;
        self.theta.insert((i, num), t);
    }
    pub fn regist_func_info(&mut self, func: &FuncDefinition) {
        let (fvar, finfo) = func.get_func_info();
        self.func.insert(fvar, finfo);
    }
    pub fn size(&self) -> usize {
        self.theta.len() 
    }
    pub fn annotation(&self, i: usize, num: usize) -> Type {
        self.theta.get(&(i, num)).unwrap().clone()
    }
    pub fn trans_variable(&mut self, var: &Variable) -> String {
        if let Some(f) = self.func.get(var).cloned() {
            format!("{}{}", var.id.into_string(), f.get_generics_annotation(self, &var.id))
        }
        else {
            var.id.into_string()
        }
    }
}

pub trait Transpile {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String;
}
