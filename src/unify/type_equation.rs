use std::collections::HashMap;

use crate::unary_expr::Variable;
use crate::func_definition::{ FuncDefinitionInfo, FuncDefinition };
use crate::trans::*;
use crate::traits::*;
use crate::unify::*;
use crate::type_spec::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Type(TypeSpec),
    Func(Vec<Type>, Box<Type>),
    TypeVariable(TypeVariable),
    AssociatedType(Box<Type>, AssociatedType),
    TraitMethod(Box<Type>, TraitMethod),
    End,
}

impl Type {
    fn occurs(&self, t: &TypeVariable) -> bool {
        match *self {
            Type::TypeVariable(ref s) if s == t => true,
            Type::Func(ref args, ref ret) => {
                for arg in args.iter() {
                    if arg.occurs(t) { return true; }
                }
                if ret.occurs(t) { return true; }
                false
            }
            Type::AssociatedType(ref ty, _) => {
                ty.as_ref().occurs(t)
            }
            Type::TraitMethod(ref ty, _) => {
                ty.as_ref().occurs(t)
            }
            _ => false,
        }
    }

    fn subst(&mut self, theta: &TypeSubst) {
        match *self {
            Type::Func(ref mut args, ref mut ret) => {
                for arg in args.iter_mut() {
                    arg.subst(theta);
                }
                ret.subst(theta);
            }
            Type::Type(_) => {},
            Type::AssociatedType(ref mut ty, _) => {
                ty.as_mut().subst(theta)
            }
            Type::TraitMethod(ref mut ty, _) => {
                ty.as_mut().subst(theta)
            }
            Type::End => {},
            // TypeVariable
            ref mut t => {
                let x = t.clone_type_variable();
                let TypeSubst { tv: y, t: into_t } = theta;
                if x == *y {
                    *t = into_t.clone();
                }
            }
        }
    }

    fn clone_type_variable(&self) -> TypeVariable {
        if let Type::TypeVariable(ref tv) = *self { tv.clone() }
        else { unreachable!("it is not TypeVariable") }
    }
}

impl Transpile for Type {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        match *self {
            Type::Type(ref t) => t.transpile(ta),
            _ => unreachable!("it is not Type"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVariable {
    Counter(usize),
}

#[derive(Debug)]
pub enum TypeEquation {
    HasTrait(Type, TraitId),
    Equal(Type, Type),
}

#[derive(Debug)]
pub struct TypeEquations {
    func: HashMap<Variable, FuncDefinitionInfo>,
    pub cnt: usize,
    variables: Vec<HashMap<Variable, Type>>,
    equs: Vec<TypeEquation>,
    self_type: Option<Type>,
}

#[derive(Debug)]
pub struct TypeSubst {
    pub tv: TypeVariable,
    pub t: Type,
}

pub type TResult = Result<Type, String>;

pub trait GenType {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult;
}

impl TypeEquations {
    pub fn new() -> Self {
        Self {
            func: HashMap::new(),
            equs: Vec::new(),
            cnt: 0,
            variables: Vec::new(),
            self_type: None,
        }
    }
    pub fn set_self_type(&mut self, self_type: Option<Type>) -> Option<Type> {
        std::mem::replace(&mut self.self_type, self_type)
    }
    pub fn get_self_type(&self) -> Option<Type> {
        self.self_type.clone()
    }
    pub fn add_has_trait(&mut self, ty: Type, tr: TraitId) {
        self.equs.push(TypeEquation::HasTrait(ty, tr));
    }
    pub fn add_equation(&mut self, left: Type, right: Type) {
        self.equs.push(TypeEquation::Equal(left, right));
    }
    pub fn get_type_variable(&mut self) -> Type {
        let i = self.cnt;
        self.cnt += 1;
        Type::TypeVariable(TypeVariable::Counter(i))
    }
    pub fn into_scope(&mut self) {
        self.variables.push(HashMap::new());
    }
    pub fn out_scope(&mut self) {
        self.variables.pop();
    }
    pub fn regist_variable(&mut self, var: Variable, t: Type) {
        self.variables.last_mut().unwrap().insert(var.clone(), t.clone());
    }
    pub fn regist_func_info(&mut self, func: &FuncDefinition) {
        let (fvar, finfo) = func.get_func_info();
        self.func.insert(fvar, finfo);
    }
    pub fn get_type_from_variable(&mut self, var: &Variable) -> TResult {
        if let Some(func) = self.func.get(var).cloned() {
            return func.generate_type(self);
        }
        for mp in self.variables.iter().rev() {
            if let Some(t) = mp.get(var) {
                return Ok(t.clone())
            }
        }
        Err(format!("Variable {:?} is not found", var))
    }
    pub fn clear_equations(&mut self) {
        self.equs.clear();
    }
    fn subst(&mut self, theta: &TypeSubst) {
        for equation in self.equs.iter_mut() {
            match *equation {
                TypeEquation::Equal(ref mut left, ref mut right) => {
                    left.subst(theta);
                    right.subst(theta);
                }
                TypeEquation::HasTrait(ref mut ty, _) => {
                    ty.subst(theta)
                }
            }
        }
    }

    fn solve_associated_type(&mut self, ty: Type, trs: &TraitsInfo) -> Result<Type, String> {
        match ty {
            Type::AssociatedType(inner_ty, asso) => {
                let inner_ty = self.solve_associated_type(*inner_ty, trs)?;
                if let Type::Type(_) = inner_ty {
                    let AssociatedType { ref trait_id, ref type_id } = asso;
                    let substs = trs.match_to_impls_for_type(trait_id, &inner_ty);
                    if substs.len() == 1 {
                        let mut substs = substs;
                        let (subst, impl_trait) = substs.pop().unwrap();
                        Ok(impl_trait.get_associated_from_id(self, type_id, &subst))
                    }
                    else {
                        Err(format!("type {:?} is not implemented trait {:?}", inner_ty, trait_id))
                    }
                }
                else {
                    Ok(Type::AssociatedType(Box::new(inner_ty), asso))
                }
            }
            Type::TraitMethod(inner_ty, tr_method) => {
                Ok(Type::TraitMethod(Box::new(self.solve_associated_type(*inner_ty, trs)?), tr_method))
            }
            _ => Ok(ty),
        }
    }

    fn solve_has_trait(&mut self, ty: &Type, tr_id: &TraitId, trs: &TraitsInfo) -> bool {
        let substs = trs.match_to_impls_for_type(tr_id, ty);
        substs.len() == 1
    }

    fn solve_trait_method(&mut self, ty: Type, trs: &TraitsInfo) -> Result<Type, String> {
        if let Type::TraitMethod(inner_ty, tr_method) = ty {
            let inner_ty = *inner_ty;
            if let Type::Type(_) = inner_ty {
                let TraitMethod { trait_id, method_id } = tr_method;
                let substs = trs.match_to_impls_for_type(&trait_id, &inner_ty);
                if substs.len() == 1 {
                    let mut substs = substs;
                    let (subst, impl_trait) = substs.pop().unwrap();
                    Ok(impl_trait.get_trait_method_from_id(self, &method_id, &subst))
                }
                else {
                    Err(format!("type {:?} is not implemented trait {:?}", inner_ty, trait_id))
                }
            }
            else {
                Ok(Type::TraitMethod(Box::new(inner_ty), tr_method))
            }
        }
        else {
            Ok(ty)
        }
    }

    pub fn unify(&mut self, trs: &TraitsInfo) -> Result<Vec<TypeSubst>, String> {
        let mut thetas = Vec::new();
        while let Some(equation) = self.equs.pop() {
            match equation {
                TypeEquation::HasTrait(Type::Type(ty_spec), tr) => {
                    if !self.solve_has_trait(&Type::Type(ty_spec.clone()), &tr, trs) {
                        Err(format!("type {:?} is not implemented trait {:?}", ty_spec, tr))?;
                    }
                }
                TypeEquation::HasTrait(left, right) => {
                    self.equs.push(TypeEquation::HasTrait(left, right));
                }
                TypeEquation::Equal(left, right) => {
                    let left = self.solve_associated_type(left, trs)?;
                    let left = self.solve_trait_method(left, trs)?;
                    let right = self.solve_associated_type(right, trs)?;
                    let right = self.solve_trait_method(right, trs)?;
                    match (left, right) {
                        (l, r) if l == r => {}
                        (Type::AssociatedType(b, a), right) => {
                            self.equs.push(TypeEquation::Equal(Type::AssociatedType(b, a), right));
                        }
                        (left, Type::AssociatedType(b, a)) => {
                            self.equs.push(TypeEquation::Equal(left, Type::AssociatedType(b, a)));
                        }
                        (Type::TraitMethod(b, a), right) => {
                            self.equs.push(TypeEquation::Equal(Type::TraitMethod(b, a), right));
                        }
                        (left, Type::TraitMethod(b, a)) => {
                            self.equs.push(TypeEquation::Equal(left, Type::TraitMethod(b, a)));
                        }
                        (Type::Func(l_args, l_return), Type::Func(r_args, r_return)) => {
                            if l_args.len() != r_args.len() {
                                Err("length of args is not equal.")?;
                            }
                            for (l, r) in l_args.into_iter().zip(r_args.into_iter()) {
                                self.equs.push(TypeEquation::Equal(l, r ));
                            }
                            self.equs.push(TypeEquation::Equal(*l_return, *r_return));
                        }
                        (Type::TypeVariable(lv), rt) => {
                            if rt.occurs(&lv) {
                                Err("unification failed, occurs")?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            for TypeSubst { t, .. } in thetas.iter_mut() {
                                t.subst(&th);
                            }
                            thetas.push(th);
                        }
                        (rt, Type::TypeVariable(lv)) => {
                            if rt.occurs(&lv) {
                                Err("unification failed, occurs")?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            for TypeSubst { t, .. } in thetas.iter_mut() {
                                t.subst(&th);
                            }
                            thetas.push(th);
                        }
                        (l, r) => {
                            Err(format!("unfication failed, {:?} != {:?}", l, r))?
                        }
                    }
                }
            }
        }
        Ok(thetas)
    }
}

/*#[test]
fn test_unify() {
    let mut traits_info = TraitsInfo::new();
    traits_info.regist_trait(&parse_trait_definition("trait MyTrait { type Output; }").unwrap().1);
    traits_info.regist_impl_candidate(&parse_impl_candidate("impl MyTrait for i64 { type Output = bool; }").unwrap().1);
    let mut equs = TypeEquations::new();
    let left = equs.get_type_variable();
    let right = crate::type_spec::parse_type_spec("i64#MyTrait::Output").unwrap().1.gen_type(&mut equs).unwrap();
    equs.add_equation(left, right);
    println!("{:?}", equs.unify(&traits_info));
}*/

