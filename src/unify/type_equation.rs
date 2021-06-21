use std::collections::{ HashMap, HashSet, VecDeque };

use crate::unary_expr::Variable;
use crate::func_definition::{ FuncDefinitionInfo, FuncDefinition };
use crate::trans::*;
use crate::traits::*;
use crate::unify::*;
use crate::type_spec::*;
use crate::type_id::*;
use crate::structs::*;
use crate::identifier::*;
#[derive(Debug, Clone, PartialEq, Eq)] pub struct CppInlineInfo {
    pub elems: Vec<CppInlineInfoElem>,
    pub tag: Tag,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CppInlineInfoElem {
    Type(usize),
    Arg(Identifier),
    End,
    Any(char),
}

impl CppInlineInfo {
    pub fn transpile(&self, ta: &TypeAnnotation, mp: &HashMap<Identifier, String>) -> String {
        self.elems.iter().map(|inline| {
            match inline {
                CppInlineInfoElem::Type(i) => {
                    ta.annotation(self.tag.get_num(), *i).transpile(ta)
                }
                CppInlineInfoElem::Arg(id) => {
                    mp.get(id).cloned().unwrap()
                }
                CppInlineInfoElem::Any(c) => {
                    c.to_string()
                }
                _ => unreachable!("End???"),
            }
        }).collect::<Vec<_>>().join("")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FuncTypeInfo {
    TraitFunc(TraitId, Tag),
    SelfFunc(Tag),
    CppInline(CppInlineInfo, Vec<Identifier>),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    SolvedAssociatedType(Box<Type>, AssociatedType),
    Func(Vec<Type>, Box<Type>, FuncTypeInfo),
    TypeVariable(TypeVariable),
    Generics(TypeId, Vec<Type>),
    AssociatedType(Box<Type>, AssociatedType),
    TraitMethod(Box<Type>, Option<TraitId>, Identifier),
    Member(Box<Type>, Identifier),
    CallEquation(CallEquation),
    End,
}

impl Type {
    pub fn from_str(s: &str) -> Self {
        Type::Generics(TypeId::from_str(s), vec![])
    }

    fn is_solved_type(&self) -> bool {
        match self {
            Type::SolvedAssociatedType(_, _) => true,
            Type::Generics(_, gens) => gens.iter().map(|gen| gen.is_solved_type()).all(|t| t),
            _ => false,
        }
    }
    fn occurs(&self, t: &TypeVariable) -> bool {
        match *self {
            Type::TypeVariable(ref s) if s == t => true,
            Type::Func(ref args, ref ret, _) => {
                for arg in args.iter() {
                    if arg.occurs(t) { return true; }
                }
                if ret.occurs(t) { return true; }
                false
            }
            Type::Generics(_, ref gens) => {
                for gen in gens.iter() {
                    if gen.occurs(t) { return true; }
                }
                false
            }
            Type::AssociatedType(ref ty, _) => {
                ty.as_ref().occurs(t)
            }
            Type::TraitMethod(ref ty, _, _) => {
                ty.as_ref().occurs(t)
            }
            Type::Member(ref ty, _) => {
                ty.as_ref().occurs(t)
            }
            Type::CallEquation(ref call_eq) => {
                call_eq.occurs(t)
            }
            _ => false,
        }
    }

    fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        match *self {
            Type::Func(ref mut args, ref mut ret, ref mut _info) => {
                let mut changed = SolveChange::Not;
                for arg in args.iter_mut() {
                    changed &= arg.subst(theta);
                }
                changed &= ret.subst(theta);
                changed
            }
            Type::Generics(ref ty, ref mut gens) => {
                gens.iter_mut().map(|gen| gen.subst(theta)).fold(SolveChange::Not, |a, b| a & b)
            }
            Type::SolvedAssociatedType(_, _) => { SolveChange::Not },
            Type::AssociatedType(ref mut ty, _) => {
                ty.as_mut().subst(theta)
            }
            Type::TraitMethod(ref mut ty, _, _) => {
                ty.as_mut().subst(theta)
            }
            Type::Member(ref mut ty, _) => {
                ty.as_mut().subst(theta)
            }
            Type::CallEquation(ref mut call_eq) => {
                call_eq.subst(theta)
            }
            Type::End => { SolveChange::Not },
            // TypeVariable
            ref mut ty => {
                let TypeSubst { tv: y, t: into_t } = theta;
                if ty.clone() == Type::TypeVariable(y.clone()) {
                    *ty = into_t.clone();
                    SolveChange::Changed
                }
                else {
                    SolveChange::Not
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
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Type::SolvedAssociatedType(ref ty, ref asso) => {
                format!("typename {}<{}>::{}", asso.trait_id.transpile(ta), ty.as_ref().transpile(ta), asso.type_id.transpile(ta))
            }
            Type::Generics(ref ty_id, ref gens) => {
                if let Some((ids, cppinline)) = ta.is_inline_struct(ty_id) {
                    let mp = ids.iter().cloned().zip(gens.iter().map(|g| g.transpile(ta))).collect::<HashMap<_, _>>();
                    cppinline.transpile(ta, &mp)
                }
                else {
                    let gens_trans = if gens.len() > 0 {
                        format!("<{}>", gens.iter().map(|gen| gen.transpile(ta)).collect::<Vec<_>>().join(", "))
                    }
                    else {
                        format!("")
                    };
                    format!("{}{}", ty_id.transpile(ta), gens_trans)
                }
            }
            _ => unreachable!("it is not Type"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVariable {
    Counter(usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveChange {
    Changed,
    Not,
}

impl SolveChange {
    pub fn cnt(&self) -> usize {
        match self {
            Self::Changed => 1,
            Self::Not => 0,
        }
    }
}

impl std::ops::BitAnd for SolveChange {
    type Output = Self;
    fn bitand(self, right: Self) -> Self {
        match (self, right) {
            (Self::Not, Self::Not) => Self::Not,
            _ => Self::Changed,
        }
    }
}

impl std::ops::BitAndAssign for SolveChange {
    fn bitand_assign(&mut self, right: Self) {
        *self = *self & right;
    }
}


#[derive(Debug)]
pub enum TypeEquation {
    HasTrait(Type, TraitId, SolveChange),
    Equal(Type, Type, SolveChange),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallEquation {
    pub caller_type: Box<Type>,
    pub trait_id: Option<TraitId>,
    pub func_id: Identifier,
    pub args: Vec<Type>,
    pub tag: Tag,
}

impl CallEquation {
    pub fn occurs(&self, tv: &TypeVariable) -> bool {
        self.caller_type.as_ref().occurs(tv)
            || self.args.iter().map(|arg| arg.occurs(tv)).any(|f| f)
    }
    pub fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        let mut changed = SolveChange::Not;
        changed &= self.caller_type.as_mut().subst(theta);
        self.args.iter_mut().map(|arg| arg.subst(theta))
            .fold(changed, |a, b| a & b)
    }

    pub fn solve(mut self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let (caller_type, caller_changed) = equs.solve_relations(*self.caller_type, trs)?;
        self.caller_type = Box::new(caller_type);

        let args = self.args.into_iter().map(|arg| equs.solve_relations(arg, trs)).collect::<Result<Vec<_>, UnifyErr>>()?;
        let args_changed = args.iter().map(|(_, c)| *c).fold(SolveChange::Not, |b, a| b & a);
        self.args = args.into_iter().map(|(a, _)| a).collect();

        let next_change = caller_changed & args_changed;

        match trs.regist_for_call_equtions(equs, &self) {
            Ok(ret_ty) => {
                Ok((ret_ty, SolveChange::Changed))
            }
            Err(cands) => {
                Ok((Type::CallEquation(self), next_change))
            }
        }
    }
}


#[derive(Debug)]
pub struct TypeEquations {
    func: HashMap<Variable, FuncDefinitionInfo>,
    pub cnt: usize,
    change_cnt: usize,
    variables: Vec<HashMap<Variable, Type>>,
    equs: VecDeque<TypeEquation>,
    want_solve: HashSet<TypeVariable>,
    substs: Vec<TypeSubst>,
    self_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct TypeSubst {
    pub tv: TypeVariable,
    pub t: Type,
}

#[derive(Debug, Clone)]
pub struct SubstsMap {
    mp: HashMap<(usize, usize), Type>,
}

impl SubstsMap {
    pub fn new(vec: Vec<TypeSubst>) -> Self {
        SubstsMap {
            mp: vec.into_iter().map(|TypeSubst { tv: TypeVariable::Counter(i, n), t }| ((i, n), t)).collect()
        }
    }
    pub fn get(&self, id: &Identifier, i: usize) -> TResult {
        match self.mp.get(&(id.get_tag_number(), i)) {
            Some(t) => Ok(t.clone()),
            None => Err(format!("undefined TypeVariable({:?}, {:?})", id, i)),
        }
    }
    pub fn get_from_tag(&self, tag: &Tag, i: usize) -> TResult {
        match self.mp.get(&(tag.get_num(), i)) {
            Some(t) => Ok(t.clone()),
            None => Err(format!("undefined TypeVariable({:?}, {:?})", tag, i)),
        }
    }
}

pub type TResult = Result<Type, String>;

pub trait GenType {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult;
}

#[derive(Debug, Clone)]
pub enum UnifyErr {
    Contradiction(String),
    Deficiency(String),
}

impl UnifyErr {
    pub fn to_string(self) -> String {
        match self {
            Self::Contradiction(st) => st,
            Self::Deficiency(st) => st,
        }
    }
}

impl TypeEquations {
    pub fn new() -> Self {
        Self {
            func: HashMap::new(),
            equs: VecDeque::new(),
            cnt: 0,
            change_cnt: 0,
            variables: Vec::new(),
            want_solve: HashSet::new(),
            substs: Vec::new(),
            self_type: None,
        }
    }
    pub fn debug(&self){
        println!("TypeEquations {{");
        for equ in self.equs.iter() {
            println!("    {:?}", equ);
        }
        for subst in self.substs.iter() {
            println!("     {:?}", subst);
        }
        println!("    {:?}", self.want_solve);
        println!("}}");
    }
    pub fn try_get_substs(&self, tv: TypeVariable) -> Type {
        self.substs.iter().find(|TypeSubst { tv: tsv, t: _t }| *tsv == tv)
            .map_or(Type::TypeVariable(tv.clone()), |TypeSubst{ tv: _tv, t }| t.clone())
    }
    pub fn take_substs(&mut self) -> Vec<TypeSubst> {
        std::mem::replace(&mut self.substs, Vec::new())
    }
    pub fn take_over_equations(&mut self, mut gen_equs: Self) {
        self.equs.append(&mut gen_equs.equs);
        self.substs.append(&mut gen_equs.substs);
        self.change_cnt += gen_equs.change_cnt;
        for elem in std::mem::replace(&mut gen_equs.want_solve, HashSet::new()).into_iter() {
            self.want_solve.insert(elem);
        }
    }
    pub fn add_want_solve(&mut self, var: &TypeVariable) {
        self.want_solve.insert(var.clone());
    }
    pub fn remove_want_solve(&mut self, var: &TypeVariable) -> bool {
        self.want_solve.remove(var)
    }
    pub fn set_self_type(&mut self, self_type: Option<Type>) -> Option<Type> {
        std::mem::replace(&mut self.self_type, self_type)
    }
    pub fn get_self_type(&self) -> TResult {
        match self.self_type.clone() {
            Some(ty) => Ok(ty),
            None => Err(format!("cant use Self")),
        }
    }
    pub fn add_has_trait(&mut self, ty: Type, tr: TraitId) {
        self.equs.push_back(TypeEquation::HasTrait(ty, tr, SolveChange::Changed));
        self.change_cnt += 1;
    }
    pub fn add_equation(&mut self, left: Type, right: Type) {
        self.equs.push_back(TypeEquation::Equal(left, right, SolveChange::Changed));
        self.change_cnt += 1;
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
    pub fn get_type_from_variable(&mut self, trs: &TraitsInfo, var: &Variable) -> TResult {
        if let Some(func) = self.func.get(var).cloned() {
            return func.generate_type(&GenericsTypeMap::empty(), self, trs, &var.id);
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
        self.change_cnt = 0;
        for equation in self.equs.iter_mut() {
            match *equation {
                TypeEquation::Equal(ref mut left, ref mut right, ref mut changed) => {
                    *changed &= left.subst(theta);
                    *changed &= right.subst(theta);
                    self.change_cnt += changed.cnt();
                }
                TypeEquation::HasTrait(ref mut ty, _, ref mut changed) => {
                    *changed &= ty.subst(theta);
                    self.change_cnt += changed.cnt();
                }
                /* TypeEquation::Call(ref mut call) => {
                    call.subst(theta);
                }*/
            }
        }
    }

    fn solve_relations(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let (ty, b0) = self.solve_call_equation(ty, trs)?;
        let (ty, b1) = self.solve_associated_type(ty, trs)?;
        let (ty, b2) = self.solve_trait_method(ty, trs)?;
        let (ty, b3) = self.solve_member(ty, trs)?;
        let (ty, b4) = self.solve_generics(ty, trs)?;
        Ok((ty, b0 & b1 & b2 & b3 & b4))
    }

    fn solve_call_equation(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        match ty {
            Type::CallEquation(call) => call.solve(self, trs),
            _ => Ok((ty, SolveChange::Not)),
        }
    }
    fn solve_associated_type(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        match ty {
            Type::AssociatedType(inner_ty, asso) => {
                let (inner_ty, inner_changed) = self.solve_relations(*inner_ty, trs)?;
                //if inner_ty.is_solved_type() {
                {
                    let AssociatedType { ref trait_id, ref type_id } = asso;
                    let substs = trs.match_to_impls_for_type(trait_id, &inner_ty);
                    if substs.len() == 1 {
                        let mut substs = substs;
                        let (subst, impl_trait) = substs.pop().unwrap();
                        let before = self.set_self_type(Some(inner_ty));
                        let res = impl_trait.get_associated_from_id(self, trs, type_id, &subst);
                        self.set_self_type(before);
                        self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                    }
                    else if inner_ty.is_solved_type() {
                        if substs.len() == 0 { 
                            Err(UnifyErr::Contradiction(format!("type {:?} is not implemented trait {:?}", inner_ty, trait_id)))
                        }
                        else if substs.len() > 1 {
                            Err(UnifyErr::Contradiction(format!("type {:?} is implemented too many trait {:?}", inner_ty, substs)))
                        }
                        else {
                            unreachable!();
                        }
                    }
                    else {
                        Ok((Type::AssociatedType(Box::new(inner_ty), asso), inner_changed))
                    }
                }
            }
            _ => Ok((ty, SolveChange::Not)),
        }
    }

    fn solve_has_trait(&mut self, ty: &Type, tr_id: &TraitId, trs: &TraitsInfo) -> bool {
        let substs = trs.match_to_impls_for_type(tr_id, ty);
        substs.len() == 1
    }

    fn solve_trait_method(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        //if let Type::TraitMethod(inner_ty, tr_method) = ty {
        if let Type::TraitMethod(inner_ty, Some(trait_id), method_id) = ty {
            let (inner_ty, inner_changed) = self.solve_relations(*inner_ty, trs)?;
            //if inner_ty.is_solved_type() {
            {
                //let TraitMethod { trait_id, method_id } = tr_method;
                let substs = trs.match_to_impls_for_type(&trait_id, &inner_ty);
                if substs.len() == 1 {
                    let mut substs = substs;
                    let (subst, impl_trait) = substs.pop().unwrap();
                    let before = self.set_self_type(Some(inner_ty.clone()));
                    let res = impl_trait.get_trait_method_from_id(self, trs, &TraitMethodIdentifier { id: method_id }, &subst, &inner_ty); self.set_self_type(before);
                    self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                }
                else if inner_ty.is_solved_type() {
                    if substs.len() == 0 { 
                        Err(UnifyErr::Contradiction(format!("type {:?} is not implemented trait {:?}", inner_ty, trait_id)))
                    }
                    else if substs.len() > 1 {
                        Err(UnifyErr::Contradiction(format!("type {:?} is implemented too many trait {:?}", inner_ty, substs)))
                    }
                    else {
                        unreachable!();
                    }
                }
                else {
                    Ok((Type::TraitMethod(Box::new(inner_ty), Some(trait_id), method_id), inner_changed))
                }
            }
        }
        else if let Type::TraitMethod(inner_ty, _, method_id) = ty {
            let (inner_ty, inner_changed) = self.solve_relations(*inner_ty, trs)?;
            //if inner_ty.is_solved_type() {
            {
                //let TraitMethod { trait_id, method_id } = tr_method;
                let substs = trs.match_to_member_for_type(&method_id, &inner_ty);
                if substs.len() == 1 {
                    let mut substs = substs;
                    let (subst, impl_trait) = substs.pop().unwrap();
                    let before = self.set_self_type(Some(inner_ty.clone()));
                    let res = impl_trait.get_trait_method_from_id(self, trs, &TraitMethodIdentifier { id: method_id }, &subst, &inner_ty);
                    self.set_self_type(before);
                    self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                }
                else if inner_ty.is_solved_type() {
                    if substs.len() == 0 { 
                        Err(UnifyErr::Contradiction(format!("type {:?} is not implemented function {:?}", inner_ty, method_id)))
                    }
                    else if substs.len() > 1 {
                        Err(UnifyErr::Contradiction(format!("type {:?} is implemented too many trait {:?}", inner_ty, substs)))
                    }
                    else {
                        unreachable!();
                    }
                }
                else {
                    Ok((Type::TraitMethod(Box::new(inner_ty), None, method_id), inner_changed))
                }
            }
        }
        else {
            Ok((ty, SolveChange::Not))
        }
    }

    fn solve_member(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Member(inner_ty, mem_id) = ty {
            let (inner_ty, inner_changed) = self.solve_relations(*inner_ty, trs)?;
            if inner_ty.is_solved_type() {
                let substs = trs.match_to_member_for_type(&mem_id, &inner_ty);
                if substs.len() == 1 {
                    let mut substs = substs;
                    let (subst, impl_trait) = substs.pop().unwrap();
                    let before = self.set_self_type(Some(inner_ty.clone()));
                    let res = impl_trait.get_trait_method_from_id(self, trs, &TraitMethodIdentifier { id: mem_id.clone() } , &subst, &inner_ty);
                    self.set_self_type(before);
                    if let Type::Func(args, returns, info) = res {
                        let mut iter = args.into_iter();
                        let self_ty = iter.next().ok_or(UnifyErr::Contradiction(format!("trait method {:?} have no argument", mem_id)))?;
                        self.add_equation(self_ty.clone(), inner_ty.clone());
                        let res = Type::Func(iter.collect(), returns, info);
                        self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                    }
                    else { unreachable!() }
                }
                else if let Type::Generics(ref id, ref gens) = inner_ty {
                    match trs.search_typeid(id).map_err(|st| UnifyErr::Contradiction(st))? {
                        StructDefinitionInfo::Def(def)  => {
                            let res = def.get_member_type(self, trs, gens, &mem_id).map_err(|st| UnifyErr::Contradiction(st))?;
                            self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                        }
                        StructDefinitionInfo::Generics  => Err(UnifyErr::Contradiction(format!("generics type has no member: {:?}", id))),
                        StructDefinitionInfo::Primitive => Err(UnifyErr::Contradiction(format!("primitive type has no member: {:?}", id))),
                    }
                }
                else if let Type::SolvedAssociatedType(_, _) = inner_ty {
                    Err(UnifyErr::Contradiction(format!("SolvedAssociatedType has no member: {:?}", inner_ty)))
                }
                else {
                    unreachable!()
                }
            }
            else if let Type::Generics(ref id, ref gens) = inner_ty {
                match trs.search_typeid(id).map_err(|st| UnifyErr::Contradiction(st))? {
                    StructDefinitionInfo::Def(def)  => {
                        match def.get_member_type(self, trs, gens, &mem_id) {
                            Ok(res) => self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed)),
                            Err(_) => Ok((Type::Member(Box::new(inner_ty), mem_id), inner_changed)),
                        }
                    }
                    StructDefinitionInfo::Generics  => Err(UnifyErr::Contradiction(format!("generics type has no member: {:?}", id))),
                    StructDefinitionInfo::Primitive => Err(UnifyErr::Contradiction(format!("primitive type has no member: {:?}", id))),
                }
            }
            else if let Type::SolvedAssociatedType(_, _) = inner_ty {
                Err(UnifyErr::Contradiction(format!("SolvedAssociatedType has no member: {:?}", inner_ty)))
            }
            else {
                Ok((Type::Member(Box::new(inner_ty), mem_id), inner_changed))
            }
        }
        else {
            Ok((ty, SolveChange::Not))
        }
    }

    fn solve_generics(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Generics(id, gens) = ty {
            let try_solve = gens.into_iter().map(|gen| { self.solve_relations(gen, trs) }).collect::<Result<Vec<_>, _>>()?;
            let inner_changed = try_solve.iter().map(|(_, changed)| *changed).fold(SolveChange::Not, |b, c| b & c);
            let gens = try_solve.into_iter().map(|(ty, _)| ty).collect();
            Ok((Type::Generics(id, gens), inner_changed))
        }
        else {
            Ok((ty, SolveChange::Not))
        }
    }

    pub fn unify(&mut self, trs: &TraitsInfo) -> Result<(), UnifyErr> {
        /* println!("unify");
        for (i, equ) in self.equs.iter().enumerate() {
            println!("{}. {:?}", i, equ);
        } */
        while let Some(equation) = self.equs.pop_front() {
            match equation {
                TypeEquation::HasTrait(left, tr, before_changed) => {
                    self.change_cnt -= before_changed.cnt();
                    let (left, left_changed) = self.solve_relations(left, trs)?;
                    if left.is_solved_type() {
                        if !self.solve_has_trait(&left, &tr, trs) {
                            Err(UnifyErr::Contradiction(format!("type {:?} is not implemented trait", tr)))?;
                        }
                    }
                    else {
                        self.equs.push_back(TypeEquation::HasTrait(left, tr, left_changed));
                        self.change_cnt += left_changed.cnt();
                    }
                }
                TypeEquation::Equal(left, right, before_changed) => {
                    self.change_cnt -= before_changed.cnt();
                    let (left, left_changed) = self.solve_relations(left, trs)?;
                    let (right, right_changed) = self.solve_relations(right, trs)?;
                    let changed = left_changed & right_changed;
                    match (left, right) {
                        (l, r) if l == r => {}
                        (Type::AssociatedType(b, a), right) => {
                            self.equs.push_back(TypeEquation::Equal(Type::AssociatedType(b, a), right, changed));
                            self.change_cnt += changed.cnt();
                        }
                        (left, Type::AssociatedType(b, a)) => {
                            self.equs.push_back(TypeEquation::Equal(left, Type::AssociatedType(b, a), changed));
                            self.change_cnt += changed.cnt();
                        }
                        (Type::TraitMethod(a, b, c), right) => {
                            self.equs.push_back(TypeEquation::Equal(Type::TraitMethod(a, b, c), right, changed));
                            self.change_cnt += changed.cnt();
                        }
                        (left, Type::TraitMethod(a, b, c)) => {
                            self.equs.push_back(TypeEquation::Equal(left, Type::TraitMethod(a, b, c), changed));
                            self.change_cnt += changed.cnt();
                        }
                        (Type::Member(b, a), right) => {
                            self.equs.push_back(TypeEquation::Equal(Type::Member(b, a), right, changed));
                            self.change_cnt += changed.cnt();
                        }
                        (left, Type::Member(b, a)) => {
                            self.equs.push_back(TypeEquation::Equal(left, Type::Member(b, a), changed));
                            self.change_cnt += changed.cnt();
                        }
                        (Type::CallEquation(call), right) => {
                            self.equs.push_back(TypeEquation::Equal(Type::CallEquation(call), right, changed));
                            self.change_cnt += changed.cnt();
                        }
                        (left, Type::CallEquation(call)) => {
                            self.equs.push_back(TypeEquation::Equal(left, Type::CallEquation(call), changed));
                            self.change_cnt += changed.cnt();
                        }
                        (Type::Func(l_args, l_return, _), Type::Func(r_args, r_return, _)) => {
                            if l_args.len() != r_args.len() {
                                Err(UnifyErr::Deficiency(format!("length of args is not equal. {:?}, {:?} vs {:?}, {:?}",
                                            l_args, l_return, r_args, r_return
                                            )))?;
                            }
                            for (l, r) in l_args.into_iter().zip(r_args.into_iter()) {
                                self.add_equation(l, r);
                            }
                            self.add_equation(*l_return, *r_return);
                        }
                        (Type::Generics(l_id, l_gens), Type::Generics(r_id, r_gens)) => {
                            if l_id != r_id {
                                Err(UnifyErr::Contradiction(format!("generics type id is not equal. {:?} != {:?}", l_id, r_id)))?;
                            }
                            else if l_gens.len() != r_gens.len() {
                                Err(UnifyErr::Contradiction(format!("unreachable, generics lengths are checked")))?;
                            }
                            else {
                                for (l, r) in l_gens.into_iter().zip(r_gens.into_iter()) {
                                    self.add_equation(l, r);
                                }
                            }
                        }
                        (Type::TypeVariable(lv), rt) if self.remove_want_solve(&lv) => {
                            if rt.occurs(&lv) {
                                Err(UnifyErr::Contradiction(format!("unification failed, occurs")))?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            for TypeSubst { t, .. } in self.substs.iter_mut() {
                                t.subst(&th);
                            }
                            self.substs.push(th);
                        }
                        (rt, Type::TypeVariable(lv)) if self.remove_want_solve(&lv) => {
                            if rt.occurs(&lv) {
                                Err(UnifyErr::Contradiction(format!("unification failed, occurs")))?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            for TypeSubst { t, .. } in self.substs.iter_mut() {
                                t.subst(&th);
                            }
                            self.substs.push(th);
                        }
                        (Type::TypeVariable(_), Type::TypeVariable(_)) => {
                            let all_not_want = self.equs.iter().map(|equ| match equ {
                                TypeEquation::Equal(Type::TypeVariable(l), Type::TypeVariable(r), _)
                                    if !self.want_solve.contains(l) && !self.want_solve.contains(r) => {
                                        true
                                    }
                                _ => false,
                            }).all(|f| f);
                            if all_not_want {
                                return Err(UnifyErr::Deficiency(format!("all not want solve variable {:?}", self.equs)))
                            }
                        }
                        (l, r) => {
                            Err(UnifyErr::Contradiction(format!("unfication failed, {:?} != {:?}", l, r)))?
                        }
                    }
                }
            }
            if self.change_cnt == 0 && self.equs.len() > 0 {
                return Err(UnifyErr::Deficiency(format!("change cnt = 0 {:?}", self.equs)));
            }
        }
        if self.want_solve.is_empty() {
            Ok(())
        }
        else {
            Err(UnifyErr::Deficiency(format!("want_solve {:?} cant solve now", self.want_solve)))
        }
    }
}


/*#[test]

fn unify_test1() {
    let mut trs = TraitsInfo::new();
    let mut equs = TypeEquations::new();
    let t = new_type_variable();
    let a = new_type_variable();
    trs.regist_structs_info(&StructDefinition {
        struct_id: TypeId::from_str("Hoge"),
        generics: vec![TypeId::from_str("T")],
        members_order: vec![Identifier::from_str("x")],
        members: vec![(Identifier::from_str("x"), TypeSpec::from_id(&TypeId::from_str("T")))].into_iter().collect(),
    }).unwrap();
    println!("trs: {:?}", trs);
    equs.add_equation(t.clone(), Type::Generics(TypeId::from_str("Hoge"), vec![Type::from_str("i64")]));
    equs.add_equation(a, Type::Member(Box::new(t), Identifier::from_str("x")));
    println!("{:?}", equs.unify(&trs));
}

#[test]
fn unify_test2() {
    let mut trs = TraitsInfo::new();
    let mut equs = TypeEquations::new();
    let t = new_type_variable();
    trs.regist_structs_info(&StructDefinition {
        struct_id: TypeId::from_str("Hoge"),
        generics: vec![TypeId::from_str("T")],
        members_order: vec![Identifier::from_str("x")],
        members: vec![(Identifier::from_str("x"), TypeSpec::from_id(&TypeId::from_str("T")))].into_iter().collect(),
    }).unwrap();
    println!("trs: {:?}", trs);
    equs.add_equation(t.clone(), Type::from_str("Hoge"));
    //equs.add_equation(Type::Type(TypeSpec::from_id(&TypeId::from_str("i64"))), Type::Member(Box::new(t), Identifier::from_str("x")));
    println!("{:?}", equs.unify(&trs));
}*/
