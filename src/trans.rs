use std::collections::HashMap;

use crate::type_id::TypeId;
use crate::identifier::Identifier;
use crate::unary_expr::Variable;
use crate::func_definition::{ FuncDefinition, FuncDefinitionInfo};
use crate::structs::*;
use crate::cpp_inline::*;
use crate::unify::*;

#[derive(Debug)]
pub struct TypeAnnotation {
    func: HashMap<Variable, FuncDefinitionInfo>,
    structs: HashMap<TypeId, (Vec<TypeId>, StructMember)>,
    theta: HashMap<(usize, &'static str, usize), Type>,
}

impl TypeAnnotation {
    pub fn new() -> Self {
        Self { func: HashMap::new(), structs: HashMap::new(), theta: HashMap::new(), }
    }
    pub fn insert(&mut self, tv: TypeVariable, t: Type) {
        let TypeVariable::Counter(i, label, num) = tv;
        self.theta.insert((i, label, num), t);
    }
    pub fn regist_func_info(&mut self, func: &FuncDefinition) {
        let (fvar, finfo) = func.get_func_info();
        self.func.insert(fvar, finfo);
    }
    pub fn regist_structs_info(&mut self, st: &StructMemberDefinition) {
        self.structs.insert(st.struct_id.clone(), (st.generics.clone(), st.member.clone()));
    }
    pub fn size(&self) -> usize {
        self.theta.len() 
    }
    pub fn annotation(&self, i: usize, label: &'static str, num: usize) -> Type {
        match self.theta.get(&(i, label, num)) {
            Some(ty) => ty.clone(),
            None => {
                let err = format!("cant get annotation {} {} {}", i, label, num);
                unreachable!(err);
            }
        }
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
        if let StructMember::MemberInfo(ref mem) = self.structs.get(tyid).unwrap().1 {
            &mem.members_order
        }
        else {
            unreachable!("dont have member {:?}", tyid)
        }
    }
    pub fn is_inline_struct(&self, tyid: &TypeId) -> Option<(&Vec<TypeId>, &CppInline)> {
        if let Some((ref gens, StructMember::CppInline(ref cppinline))) = self.structs.get(tyid) {
            Some((&gens, cppinline))
        }
        else {
            None
        }
    }
}

pub trait Transpile {
    fn transpile(&self, ta: &TypeAnnotation) -> String;
}
