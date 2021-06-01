use std::collections::HashMap;

use crate::type_id::TypeId;
use crate::identifier::Identifier;
use crate::unary_expr::Variable;
use crate::func_definition::{ FuncDefinition, FuncDefinitionInfo};
use crate::structs::StructDefinition;
use crate::unify::*;


#[derive(Debug)]
pub struct TypeAnnotation {
    func: HashMap<Variable, FuncDefinitionInfo>,
    structs: HashMap<TypeId, Vec<Identifier>>,
    theta: HashMap<(usize, usize), Type>,
}

impl TypeAnnotation {
    pub fn new() -> Self {
        Self { func: HashMap::new(), structs: HashMap::new(), theta: HashMap::new(), }
    }
    pub fn insert(&mut self, tv: TypeVariable, t: Type) {
        let TypeVariable::Counter(i, num) = tv;
        self.theta.insert((i, num), t);
    }
    pub fn regist_func_info(&mut self, func: &FuncDefinition) {
        let (fvar, finfo) = func.get_func_info();
        self.func.insert(fvar, finfo);
    }
    pub fn regist_structs_info(&mut self, st: &StructDefinition) {
        self.structs.insert(st.struct_id.clone(), st.members_order.clone());
    }
    pub fn size(&self) -> usize {
        self.theta.len() 
    }
    pub fn annotation(&self, i: usize, num: usize) -> Type {
        self.theta.get(&(i, num)).unwrap().clone()
    }
    pub fn trans_variable(&self, var: &Variable) -> String {
        if let Some(f) = self.func.get(var).cloned() {
            format!("{}{}", var.id.into_string(), f.get_generics_annotation(self, &var.id))
        }
        else {
            var.id.into_string()
        }
    }
    pub fn get_struct_members_order(&self, tyid: &TypeId) -> &Vec<Identifier> {
        self.structs.get(tyid).unwrap()
    }
}

pub trait Transpile {
    fn transpile(&self, ta: &TypeAnnotation) -> String;
}
