pub mod call_equation;
pub mod associated_type;
pub mod member;
pub mod tuple_member;
pub mod call_variable;

pub use call_equation::*;
pub use associated_type::*;
pub use member::*;
pub use tuple_member::*;
pub use call_variable::*;

use std::collections::{ HashMap, HashSet, VecDeque };

use crate::unary_expr::Variable;
use crate::func_definition::{ FuncDefinitionInfo, FuncDefinition };
use crate::trans::*;
use crate::traits::*;
use crate::unify::*;
use crate::type_spec::*;
use crate::type_id::*;
use crate::identifier::*;
use crate::error::*;
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
                    ta.annotation(self.tag.get_num(), "CppInlineInfoType", *i).transpile(ta)
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
    TraitFunc(TraitId, usize, Tag),
    SelfFunc(Tag),
    CppInline(CppInlineInfo, Vec<Identifier>),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AutoRefTag {
    Tag(Tag),
    Nothing,
    Ref,
    MutRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitGenerics {
    pub trait_id: TraitId,
    pub generics: Vec<Type>,
}

impl TraitGenerics {
    pub fn get_tag(&self) -> Tag {
        self.trait_id.id.tag.clone()
    }
    fn occurs(&self, t: &TypeVariable) -> bool {
        self.generics.iter().map(|g| g.occurs(t)).any(|b| b)
    }
    fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        self.generics.iter_mut().map(|g| g.subst(theta)).fold(SolveChange::not(), |a, b| a & b)
    }
    fn solve(self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Self, SolveChange), UnifyErr> {
        let generics = self.generics.into_iter().map(|g| equs.solve_relations(g, trs)).collect::<Result<Vec<_>, _>>()?;
        let (generics, changes): (Vec<_>, Vec<_>) = generics.into_iter().unzip();
        let solve_change = changes.into_iter().fold(SolveChange::not(), |a, b| a & b);
        Ok((TraitGenerics { trait_id: self.trait_id, generics, }, solve_change))
    }
    fn is_solved_type(&self) -> bool {
        self.generics.iter().map(|g| g.is_solved_type()).all(|b| b)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    SolvedAssociatedType(Box<Type>, TraitGenerics, AssociatedTypeIdentifier),
    Func(Vec<Type>, Box<Type>, FuncTypeInfo),
    TypeVariable(TypeVariable),
    Generics(TypeId, Vec<Type>),
    AssociatedType(AssociatedTypeEquation),
    TraitMethod(Box<Type>, Option<TraitGenerics>, Identifier),
    Member(MemberEquation),
    CallEquation(CallEquation),
    CallVariable(CallVariable),
    Ref(Box<Type>),
    MutRef(Box<Type>),
    Deref(Box<Type>),
    AutoRef(Box<Type>, AutoRefTag),
    Tuple(Vec<Type>),
    TupleMember(TupleMemberEquation),
    End,
}

impl Type {
    pub fn from_str(s: &str) -> Self {
        Type::Generics(TypeId::from_str(s), vec![])
    }

    fn is_solved_type(&self) -> bool {
        match self {
            Type::SolvedAssociatedType(_, _, _) => true,
            Type::Generics(_, gens) => gens.iter().map(|gen| gen.is_solved_type()).all(|t| t),
            Type::Ref(ref ty) => ty.as_ref().is_solved_type(),
            Type::MutRef(ref ty) => ty.as_ref().is_solved_type(),
            Type::Func(ref args, ref ret, _) => args.iter().map(|arg| arg.is_solved_type()).all(|t| t) && ret.is_solved_type(),
            Type::Tuple(ref params) => params.iter().map(|p| p.is_solved_type()).all(|b| b),
            _ => false,
        }
    }
    fn is_wait_solve(&self) -> bool {
        match self {
            Type::SolvedAssociatedType(..) => false,
            Type::TypeVariable(..) => false,
            Type::AutoRef(..) => false,
            Type::Generics(_, gens) => gens.iter().map(|gen| gen.is_wait_solve()).any(|t| t),
            Type::Ref(ref ty) => ty.as_ref().is_wait_solve(),
            Type::MutRef(ref ty) => ty.as_ref().is_wait_solve(),
            Type::Func(ref args, ref ret, _) => args.iter().map(|arg| arg.is_wait_solve()).any(|t| t) || ret.is_wait_solve(),
            Type::Tuple(ref params) => params.iter().map(|p| p.is_wait_solve()).any(|b| b),
            _ => true,
        }
    }
    fn occurs(&self, t: &TypeVariable) -> bool {
        match *self {
            Type::TypeVariable(ref s) => s == t,
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
            Type::AssociatedType(ref eq) => {
                eq.occurs(t)
            }
            Type::SolvedAssociatedType(ref ty, ref tr, _) => {
                ty.as_ref().occurs(t) || tr.occurs(t)
            }
            Type::TraitMethod(ref ty, _, _) => {
                ty.as_ref().occurs(t)
            }
            Type::Member(ref ty) => {
                ty.occurs(t)
            }
            Type::CallEquation(ref call_eq) => {
                call_eq.occurs(t)
            }
            Type::CallVariable(ref call_var) => {
                call_var.occurs(t)
            }
            Type::Ref(ref ty) => {
                ty.as_ref().occurs(t)
            }
            Type::MutRef(ref ty) => {
                ty.as_ref().occurs(t)
            }
            Type::Deref(ref ty) => {
                ty.as_ref().occurs(t)
            }
            Type::AutoRef(ref ty, _) => {
                ty.as_ref().occurs(t)
            }
            Type::Tuple(ref params) => {
                params.iter().map(|p| p.occurs(t)).any(|b| b)
            }
            Type::TupleMember(ref ty) => {
                ty.occurs(t)
            }
            Type::End => false,
        }
    }

    fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        match *self {
            Type::Func(ref mut args, ref mut ret, ref mut _info) => {
                let mut changed = SolveChange::not();
                for arg in args.iter_mut() {
                    changed &= arg.subst(theta);
                }
                changed &= ret.subst(theta);
                changed
            }
            Type::Generics(ref _ty, ref mut gens) => {
                gens.iter_mut().map(|gen| gen.subst(theta)).fold(SolveChange::not(), |a, b| a & b)
            }
            Type::SolvedAssociatedType(_, _, _) => { SolveChange::not() },
            Type::AssociatedType(ref mut eq) => {
                eq.subst(theta)
            }
            Type::TraitMethod(ref mut ty, _, _) => {
                ty.as_mut().subst(theta)
            }
            Type::Member(ref mut ty) => {
                ty.subst(theta)
            }
            Type::CallEquation(ref mut call_eq) => {
                call_eq.subst(theta)
            }
            Type::CallVariable(ref mut call_var) => {
                call_var.subst(theta)
            }
            Type::Ref(ref mut ty) => {
                ty.as_mut().subst(theta)
            }
            Type::MutRef(ref mut ty) => {
                ty.as_mut().subst(theta)
            }
            Type::Deref(ref mut ty) => {
                ty.as_mut().subst(theta)
            }
            Type::AutoRef(ref mut ty, _) => {
                ty.as_mut().subst(theta)
            }
            Type::Tuple(ref mut params) => {
                params.iter_mut().map(|p| p.subst(theta)).fold(SolveChange::not(), |a, b| a & b)
            }
            Type::TupleMember(ref mut ty) => {
                ty.subst(theta)
            }
            Type::End => { SolveChange::not() },
            // TypeVariable
            ref mut ty => {
                let TypeSubst { tv: y, t: into_t } = theta;
                if ty.clone() == Type::TypeVariable(y.clone()) {
                    *ty = into_t.clone();
                    SolveChange::Changed
                }
                else {
                    SolveChange::not()
                }
            }
        }
    }

    pub fn is_reference(&self) -> bool {
        match *self {
            Type::Ref(_) | Type::MutRef(_) => true,
            _ => false,
        }
    }
    fn double_reference_check(&self) -> Result<(), UnifyErr> {
        match *self {
            Type::Ref(ref ty) | Type::MutRef(ref ty) => {
                if ty.is_reference() {
                    Err(UnifyErr::Contradiction(ErrorComment::empty(format!("double reference is not allowed, {:?}", self))))
                }
                else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}

impl Transpile for Type {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Type::SolvedAssociatedType(ref ty, ref tr, ref asso_id) => {
                match find_operator(tr.trait_id.id.into_string().as_str()) {
                    Some(ResultFindOperator::Binary((_, ope))) => {
                        let left = ty.transpile(ta);
                        let right = tr.generics[0].transpile(ta);
                        format!("decltype(std::declval<{}>() {} std::declval<{}>())", left, ope, right)
                    }
                    Some(ResultFindOperator::Unary((_, ope))) => {
                        let left = ty.transpile(ta);
                        format!("decltype({}std::declval<{}>())", ope, left)
                    }
                    Some(ResultFindOperator::Eq) => {
                        unreachable!("trait Eq have no associated type");
                    }
                    Some(ResultFindOperator::Ord) => {
                        unreachable!("trait Eq have no associated type");
                    }
                    Some(ResultFindOperator::Clone) => {
                        unreachable!("trait Clone have no associated type");
                    }
                    Some(ResultFindOperator::Copy) => {
                        unreachable!("trait Copy have no associated type");
                    }
                    None => {

                        let generics = std::iter::once(ty.transpile(ta)).chain(tr.generics.iter().map(|g| g.transpile(ta))).collect::<Vec<_>>().join(", ");
                        format!("typename {}<{}>::{}", tr.trait_id.transpile(ta), generics, asso_id.transpile(ta))
                    }
                }
            }
            Type::Ref(ref ty) => {
                format!("{} const&", ty.as_ref().transpile(ta))
            }
            Type::MutRef(ref ty) => {
                format!("{}&", ty.as_ref().transpile(ta))
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
            Type::Tuple(ref params) => {
                format!("std::tuple<{}>", params.iter().map(|p| p.transpile(ta)).collect::<Vec<_>>().join(", "))
            }
            ref ty => unreachable!(format!("it is not Type {:?}", ty)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVariable {
    Counter(usize, &'static str, usize),
}

#[derive(Debug)]
pub enum SolveChange {
    Changed,
    Not(Vec<Error>),
}

impl SolveChange {
    pub fn not() -> Self {
        SolveChange::Not(Vec::new())
    }
    pub fn err(err: Error) -> Self {
        SolveChange::Not(vec![err])
    }
    pub fn cnt(&self) -> usize {
        match self {
            Self::Changed => 1,
            Self::Not(_) => 0,
        }
    }
    pub fn into_detail(self, comment: String, prev: Error) -> Error {
        let errs = match self {
            Self::Changed => Vec::new(),
            Self::Not(errs) => errs,
        };
        ErrorDetails::new(comment, errs, prev)
    }
}

impl std::ops::BitAnd for SolveChange {
    type Output = Self;
    fn bitand(self, right: Self) -> Self {
        match (self, right) {
            (Self::Not(mut e1), Self::Not(mut e2)) => {
                e1.append(&mut e2);
                Self::Not(e1)
            }
            _ => Self::Changed,
        }
    }
}

impl std::ops::BitAndAssign for SolveChange {
    fn bitand_assign(&mut self, right: Self) {
        match (self, right) {
            (Self::Not(ref mut e1), Self::Not(mut e2)) => {
                e1.append(&mut e2);
            }
            (s, _) => {
                *s = Self::Changed;
            }
        }
    }
}


#[derive(Debug)]
pub enum TypeEquation {
    CopyTrait(Tag, Type, SolveChange),
    HasTrait(Type, TraitGenerics, ErrorHint, SolveChange),
    TupleTrait(Type, TraitId, ErrorHint, SolveChange),
    Equal(Type, Type, Error, SolveChange),
}

impl TypeEquation {
    pub fn replace_solve_change(&mut self, new_change: SolveChange) -> SolveChange {
        match *self {
            TypeEquation::Equal(_, _, _, ref mut change) |
                TypeEquation::HasTrait(_, _, _, ref mut change) |
                TypeEquation::CopyTrait(_, _, ref mut change) |
                TypeEquation::TupleTrait(_, _, _, ref mut change) => {
                    std::mem::replace(change, new_change)
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
    not_void_vars: HashSet<TypeVariable>,
    substs: Vec<TypeSubst>,
    pub copyable: HashSet<Tag>,
    self_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct TypeSubst {
    pub tv: TypeVariable,
    pub t: Type,
}

#[derive(Debug, Clone)]
pub struct SubstsMap {
    mp: HashMap<(usize, &'static str, usize), Type>,
}

impl SubstsMap {
    pub fn new(vec: Vec<TypeSubst>) -> Self {
        SubstsMap {
            mp: vec.into_iter().map(|TypeSubst { tv: TypeVariable::Counter(i, label, n), t }| ((i, label, n), t)).collect()
        }
    }
    pub fn get(&self, id: &Identifier, label: &'static str, i: usize) -> TResult {
        match self.mp.get(&(id.get_tag_number(), label, i)) {
            Some(t) => Ok(t.clone()),
            None => Err(ErrorComment::empty(format!("undefined TypeVariable({:?}, {:?})", id, i))),
        }
    }
    pub fn get_from_tag(&self, tag: &Tag, label: &'static str, i: usize) -> TResult {
        match self.mp.get(&(tag.get_num(), label, i)) {
            Some(t) => Ok(t.clone()),
            None => Err(ErrorComment::empty(format!("undefined TypeVariable({:?}, {:?})", tag, i))),
        }
    }
}

pub type TResult = Result<Type, Error>;

pub trait GenType {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult;
}

#[derive(Debug)]
pub enum UnifyErr {
    Contradiction(Error),
    Deficiency(Error),
}

impl UnifyErr {
    /*
    pub fn to_string(self) -> String {
        match self {
            Self::Contradiction(st) => st,
            Self::Deficiency(st) => st,
        }
    }
    */
    pub fn into_err(self) -> Error {
        match self {
            Self::Contradiction(err) => err,
            Self::Deficiency(err) => err,
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
            not_void_vars: HashSet::new(),
            substs: Vec::new(),
            copyable: HashSet::new(),
            self_type: None,
        }
    }
    pub fn debug(&self){
        let mut debug_str = String::new();
        for equ in self.equs.iter() {
            debug_str.push_str(&format!("\n{:?}", equ))
        }
        /*
        for subst in self.substs.iter() {
            log::info!("     {:?}", subst);
        }
        log::info!("    {:?}", self.want_solve);
        */
        log::debug!("{}", debug_str);
    }
    pub fn try_get_substs(&self, tv: TypeVariable) -> Type {
        self.substs.iter().find(|TypeSubst { tv: tsv, t: _t }| *tsv == tv)
            .map_or(Type::TypeVariable(tv.clone()), |TypeSubst{ tv: _tv, t }| t.clone())
    }
    pub fn take_substs(&mut self) -> Vec<TypeSubst> {
        std::mem::replace(&mut self.substs, Vec::new())
    }
    pub fn take_over_equations(&mut self, mut gen_equs: Self) {
        for elem in std::mem::replace(&mut gen_equs.want_solve, HashSet::new()).into_iter() {
            self.want_solve.insert(elem);
        }
        for subst in gen_equs.substs.iter() {
            self.subst(subst);
        }
        for TypeSubst { tv, .. } in gen_equs.substs.iter() {
            self.want_solve.remove(tv);
        }
        for e in gen_equs.equs.iter_mut() {
            e.replace_solve_change(SolveChange::not());
        }
        self.equs.append(&mut gen_equs.equs);
        self.substs.append(&mut gen_equs.substs);
        self.change_cnt += gen_equs.change_cnt;
    }
    pub fn add_want_solve(&mut self, var: &TypeVariable, is_not_void: bool) {
        self.want_solve.insert(var.clone());
        if is_not_void {
            self.not_void_vars.insert(var.clone());
        }
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
            None => Err(ErrorComment::empty(format!("cant use Self"))),
        }
    }
    pub fn add_has_trait(&mut self, ty: Type, tr: TraitGenerics, hint: ErrorHint) {
        self.equs.push_back(TypeEquation::HasTrait(ty, tr, hint, SolveChange::Changed));
        self.change_cnt += 1;
    }
    pub fn add_tuple_trait(&mut self, ty: Type, tr: TraitId, hint: ErrorHint) {
        self.equs.push_back(TypeEquation::TupleTrait(ty, tr, hint, SolveChange::Changed));
        self.change_cnt += 1;
    }
    pub fn add_equation(&mut self, left: Type, right: Type, err: Error) {
        self.equs.push_back(TypeEquation::Equal(left, right, err, SolveChange::Changed));
        self.change_cnt += 1;
    }
    pub fn regist_check_copyable(&mut self, tag: Tag, ty: Type) {
        self.equs.push_back(TypeEquation::CopyTrait(tag, ty, SolveChange::Changed));
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
            return func.generate_type(&GenericsTypeMap::empty(), self, trs, &var.id, &ErrorHint::None);
        }
        for mp in self.variables.iter().rev() {
            if let Some(t) = mp.get(var) {
                return Ok(t.clone())
            }
        }
        Err(ErrorComment::empty(format!("Variable {:?} is not found", var)))
    }
    pub fn clear_equations(&mut self) {
        self.equs.clear();
    }
    fn subst(&mut self, theta: &TypeSubst) {
        self.change_cnt = 0;
        for TypeSubst { t, .. } in self.substs.iter_mut() {
            t.subst(theta);
        }
        for equation in self.equs.iter_mut() {
            match *equation {
                TypeEquation::Equal(ref mut left, ref mut right, _, ref mut changed) => {
                    *changed &= left.subst(theta);
                    *changed &= right.subst(theta);
                    self.change_cnt += changed.cnt();
                }
                TypeEquation::HasTrait(ref mut ty, ref mut tr, _, ref mut changed) => {
                    *changed &= ty.subst(theta);
                    *changed &= tr.subst(theta);
                    self.change_cnt += changed.cnt();
                }
                TypeEquation::CopyTrait(_, ref mut ty, ref mut changed) => {
                    *changed &= ty.subst(theta);
                    self.change_cnt += changed.cnt();
                }
                TypeEquation::TupleTrait(ref mut ty, _, _, ref mut changed) => {
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
        let (ty, b5) = self.solve_deref(ty, trs)?;
        let (ty, b6) = self.solve_autoref(ty, trs)?;
        let (ty, b7) = self.solve_func(ty, trs)?;
        let (ty, b8) = self.solve_tuple(ty, trs)?;
        let (ty, b9) = self.solve_tuple_member(ty, trs)?;
        let (ty, b10) = self.solve_call_variable(ty, trs)?;
        let (ty, b11) = self.solve_ref(ty, trs)?;
        Ok((ty, b0 & b1 & b2 & b3 & b4 & b5 & b6 & b7 & b8 & b9 & b10 & b11))
    }

    fn solve_call_equation(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        match ty {
            Type::CallEquation(call) => call.solve(self, trs),
            _ => Ok((ty, SolveChange::not())),
        }
    }
    fn solve_associated_type(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        match ty {
            Type::AssociatedType(eq) => eq.solve(self, trs),
            _ => Ok((ty, SolveChange::not())),
        }
    }

    fn solve_has_trait(&mut self, ty: &Type, trait_gen: &TraitGenerics, trs: &TraitsInfo) -> usize {
        let substs = trs.match_to_impls_for_type(trait_gen, ty);
        match substs {
            Ok(_) => 1,
            Err(i) => i,
        }
    }
    fn solve_copy_trait(&mut self, ty: &Type, trs: &TraitsInfo) -> usize {
        match *ty {
            Type::Ref(_) | Type::MutRef(_) => {
                1
            }
            ref ty => {
                self.solve_has_trait(&ty, &TraitGenerics { trait_id: TraitId { id: Identifier::from_str("Copy") }, generics: Vec::new() }, trs)
            }
        }
    }

    fn solve_trait_method(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        //if let Type::TraitMethod(inner_ty, tr_method) = ty {
        if let Type::TraitMethod(inner_ty, Some(trait_gen), method_id) = ty {
            let (inner_ty, inner_changed) = self.solve_relations(*inner_ty, trs)?;
            let (trait_gen, trait_changed) = trait_gen.solve(self, trs)?;
            //if inner_ty.is_solved_type() {
            {
                //let TraitMethod { trait_id, method_id } = tr_method;
                let substs = trs.match_to_impls_for_type(&trait_gen, &inner_ty);
                match substs {
                    Ok((subst, impl_trait)) => {
                        let before = self.set_self_type(Some(inner_ty.clone()));
                        let res = impl_trait.get_trait_method_from_id(self, trs, &TraitMethodIdentifier { id: method_id }, &subst, &inner_ty); self.set_self_type(before);
                        self.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                    }
                    Err(len) => {
                        if inner_ty.is_solved_type() {
                            if len == 0 { 
                                Err(UnifyErr::Contradiction(ErrorComment::empty(format!("type {:?} is not implemented trait {:?}", inner_ty, trait_gen))))
                            }
                            else if len > 1 {
                                Err(UnifyErr::Contradiction(ErrorComment::empty(format!("type {:?} is implemented too many trait {:?}", inner_ty, substs))))
                            }
                            else {
                                unreachable!();
                            }
                        }
                        else {
                            Ok((Type::TraitMethod(Box::new(inner_ty), Some(trait_gen), method_id), inner_changed & trait_changed))
                        }
                    }
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
                        Err(UnifyErr::Contradiction(ErrorComment::empty(format!("type {:?} is not implemented function {:?}", inner_ty, method_id))))
                    }
                    else if substs.len() > 1 {
                        Err(UnifyErr::Contradiction(ErrorComment::empty(format!("type {:?} is implemented too many trait {:?}", inner_ty, substs))))
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
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_member(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Member(mem) = ty {
            mem.solve(self, trs)
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_generics(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Generics(id, gens) = ty {
            let try_solve = gens.into_iter().map(|gen| { self.solve_relations(gen, trs) }).collect::<Result<Vec<_>, _>>()?;
            let (gens, changes): (Vec<_>, Vec<_>) = try_solve.into_iter().unzip();
            let inner_changed = changes.into_iter().fold(SolveChange::not(), |b, c| b & c);
            Ok((Type::Generics(id, gens), inner_changed))
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_deref(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Deref(ty) = ty {
            let (ty, inner_change) = self.solve_relations(*ty, trs)?;
            match ty {
                Type::Ref(ty) => Ok((*ty, SolveChange::Changed)),
                Type::MutRef(ty) => Ok((*ty, SolveChange::Changed)),
                ty => Ok((Type::Deref(Box::new(ty)), inner_change)),
            }
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_autoref(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::AutoRef(ty, tag) = ty {
            let (ty, change) = self.solve_relations(*ty, trs)?;
            Ok((Type::AutoRef(Box::new(ty), tag), change))
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_func(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Func(args, ret, info) = ty {
            let mut change = SolveChange::not();
            let args = args.into_iter().map(|ty| {
                let (ty, ch) = self.solve_relations(ty, trs)?;
                change &= ch;
                Ok(ty)
            }).collect::<Result<Vec<_>, _>>()?;
            let (ret, ret_change) = self.solve_relations(*ret, trs)?;
            Ok((Type::Func(args, Box::new(ret), info), change & ret_change))
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_tuple(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::Tuple(params) = ty {
            let mut change = SolveChange::not();
            let params = params.into_iter().map(|ty| {
                let (ty, ch) = self.solve_relations(ty, trs)?;
                change &= ch;
                Ok(ty)
            }).collect::<Result<Vec<_>, _>>()?;
            Ok((Type::Tuple(params), change))
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_tuple_member(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        if let Type::TupleMember(eq) = ty {
            eq.solve(self, trs)
        }
        else {
            Ok((ty, SolveChange::not()))
        }
    }

    fn solve_call_variable(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        match ty {
            Type::CallVariable(call) => call.solve(self, trs),
            _ => Ok((ty, SolveChange::not())),
        }
    }

    fn solve_ref(&mut self, ty: Type, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        match ty {
            Type::Ref(ty) => {
                let (ty, change) = self.solve_relations(*ty, trs)?;
                Ok((Type::Ref(Box::new(ty)), change))
            }
            Type::MutRef(ty) => {
                let (ty, change) = self.solve_relations(*ty, trs)?;
                Ok((Type::MutRef(Box::new(ty)), change))
            }
            _ => Ok((ty, SolveChange::not())),
        }
    }


    pub fn unify(&mut self, trs: &TraitsInfo) -> Result<(), UnifyErr> {
        /* log::debug!("unify");
        for (i, equ) in self.equs.iter().enumerate() {
            log::debug!("{}. {:?}", i, equ);
        } */
        while let Some(equation) = self.equs.pop_front() {
            match equation {
                TypeEquation::HasTrait(left, tr, hint, before_changed) => {
                    self.change_cnt -= before_changed.cnt();
                    let (left, left_changed) = self.solve_relations(left, trs)?;
                    let (tr, tr_changed) = tr.solve(self, trs)?;
                    let changed = left_changed & tr_changed;
                    log::debug!("{:?} {:?} {:?} {:?}", left, left.is_solved_type(), tr, tr.is_solved_type());
                    if left.is_solved_type() && tr.is_solved_type() {
                        let solve_cnt = self.solve_has_trait(&left, &tr, trs);
                        if solve_cnt == 0 {
                            Err(UnifyErr::Contradiction(
                                    ErrorComment::new(format!("type {:?} is not implemented trait {:?}", left, tr), hint.err())
                            ))?;
                        }
                        else if solve_cnt > 1{
                            Err(UnifyErr::Contradiction(
                                    ErrorComment::new(format!("type {:?} is too many implemented traits {:?}", left, tr), hint.err())
                            ))?;
                        }
                    }
                    else {
                        self.change_cnt += changed.cnt();
                        self.equs.push_back(TypeEquation::HasTrait(left, tr, hint, changed));
                    }
                }
                TypeEquation::TupleTrait(left, tr, hint, before_changed) => {
                    self.change_cnt -= before_changed.cnt();
                    let (left, left_changed) = self.solve_relations(left, trs)?;
                    if let Type::Tuple(ts) = left {
                        for t in ts {
                            self.equs.push_back(TypeEquation::HasTrait(t, TraitGenerics { trait_id: tr.clone(), generics: Vec::new() }, hint.clone(), SolveChange::Changed));
                            self.change_cnt += 1;
                        }
                    }
                    else {
                        self.change_cnt += left_changed.cnt();
                        self.equs.push_back(TypeEquation::TupleTrait(left, tr, hint, left_changed));
                    }
                }
                TypeEquation::CopyTrait(tag, ty, before_changed) => {
                    self.change_cnt -= before_changed.cnt();
                    let (ty, ty_changed) = self.solve_relations(ty, trs)?;
                    if ty.is_solved_type() {
                        let solve_cnt = self.solve_copy_trait(&ty, trs);
                        if solve_cnt == 1 {
                            log::debug!("{:?} is copyable", ty);
                            self.copyable.insert(tag);
                        }
                        else {
                            log::debug!("{:?} is not copyable", ty);
                        }
                    }
                    else {
                        self.change_cnt += ty_changed.cnt();
                        self.equs.push_back(TypeEquation::CopyTrait(tag, ty, ty_changed));
                    }
                }
                TypeEquation::Equal(left, right, err, before_changed) => {
                    left.double_reference_check()?;
                    right.double_reference_check()?;
                    //log::info!("\n{:?} = {:?}", left, right);
                    self.change_cnt -= before_changed.cnt();
                    let (left, left_changed) = self.solve_relations(left, trs)?;
                    let (right, right_changed) = self.solve_relations(right, trs)?;
                    let changed = left_changed & right_changed;
                    match (left, right) {
                        (l, r) if l == r => {}
                        (left, Type::AutoRef(ty, AutoRefTag::Tag(tag))) | (Type::AutoRef(ty, AutoRefTag::Tag(tag)), left) => {
                            let (ty, ty_changed) = self.solve_relations(*ty, trs)?;
                            let mut oks = vec![
                                    (AutoRefTag::Nothing, ty.clone()),
                                    (AutoRefTag::Ref, Type::Ref(Box::new(ty.clone()))),
                                    (AutoRefTag::MutRef, Type::MutRef(Box::new(ty.clone()))),
                            ].into_iter()
                                .map(|(ref_tag, right)| {
                                    let mut tmp_equs = TypeEquations::new();
                                    tmp_equs.add_equation(left.clone(), right, ErrorComment::new(format!("deref by {:?}", ref_tag), err.clone()));
                                    (ref_tag, tmp_equs)
                            }).chain(std::iter::once( {
                                let mut tmp_equs = TypeEquations::new();
                                let alpha = tag.generate_type_variable("MutrefToRef", 0, &mut tmp_equs);
                                tmp_equs.add_equation(ty.clone(), Type::MutRef(Box::new(alpha.clone())), Error::None);
                                tmp_equs.add_equation(Type::Ref(Box::new(alpha)), left.clone(), Error::None);
                                (AutoRefTag::Nothing, tmp_equs)
                            } )).filter_map(
                                |(ref_tag, mut tmp_equs)| match tmp_equs.unify(trs) {
                                    Err(UnifyErr::Contradiction(_)) => None,
                                    _ => Some((ref_tag, tmp_equs)),
                                }
                            ).collect::<Vec<_>>();
                            //log::debug!("--------------------");
                            //log::debug!("AUTOREF {:?} : {:?} {:?}", left, ty, tag);
                            //log::debug!("oks = {:?}", oks);
                            if oks.len() == 0 {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("not equal {:?}, auto ref {:?}", left, ty), err)))?;
                            }
                            else if oks.len() == 1 {
                                //log::debug!("OK");
                                //log::debug!("--------------------");
                                let (ref_tag, tmp_equs) = oks.pop().unwrap();
                                self.take_over_equations(tmp_equs);
                                let var = tag.generate_type_variable("AutoRefType", 0, self);
                                self.add_equation(var, Type::AutoRef(Box::new(ty), ref_tag), err);
                            }
                            else {
                                //log::debug!("NG");
                                //log::debug!("--------------------");
                                self.equs.push_back(TypeEquation::Equal(left, Type::AutoRef(Box::new(ty), AutoRefTag::Tag(tag)), err, changed & ty_changed));
                            }
                        }
                        (Type::Func(l_args, l_return, _), Type::Func(r_args, r_return, _)) => {
                            if l_args.len() != r_args.len() {
                                return Err(UnifyErr::Contradiction(ErrorComment::new(format!("length of args is not equal. {:?}, {:?} vs {:?}, {:?}",
                                            l_args, l_return, r_args, r_return
                                            ), err)));
                            }
                            else {
                                for (i, (l, r)) in l_args.into_iter().zip(r_args.into_iter()).enumerate() {
                                    self.add_equation(l, r, ErrorComment::new(format!("{}-th function arg equation", i), err.clone()));
                                }
                                self.add_equation(*l_return, *r_return, ErrorComment::new(format!("function return equation"), err));
                            }
                        }
                        (Type::Generics(l_id, l_gens), Type::Generics(r_id, r_gens)) => {
                            if l_id != r_id {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("generics type id is not equal. {:?} != {:?}", l_id, r_id), err)))?;
                            }
                            else if l_gens.len() != r_gens.len() {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("unreachable, generics lengths are checked"), err)))?;
                            }
                            else {
                                for (i, (l, r)) in l_gens.into_iter().zip(r_gens.into_iter()).enumerate() {
                                    self.add_equation(l, r, ErrorComment::new(format!("{}-th generics arg equation", i), err.clone()));
                                }
                            }
                        }
                        (Type::Ref(l_ty), Type::Ref(r_ty)) => {
                            self.add_equation(*l_ty, *r_ty, err);
                        }
                        (Type::MutRef(l_ty), Type::MutRef(r_ty)) => {
                            self.add_equation(*l_ty, *r_ty, err);
                        }
                        (Type::Tuple(lp), Type::Tuple(rp)) => {
                            if lp.len() != rp.len() {
                                return Err(UnifyErr::Contradiction(ErrorComment::new(format!("lengths of tuples are not match, {:?}, {:?}", lp, rp), err)));
                            }
                            for (i, (l, r)) in lp.into_iter().zip(rp.into_iter()).enumerate() {
                                self.add_equation(l, r, ErrorComment::new(format!("{}-th tuple element equation", i), err.clone()))
                            }
                        }
                        (left, right) if left.is_wait_solve() || right.is_wait_solve() => {
                            self.change_cnt += changed.cnt();
                            self.equs.push_back(TypeEquation::Equal(left, right, err, changed));
                        }
                        (Type::TypeVariable(lv), rt) if self.remove_want_solve(&lv) => {
                            if rt.occurs(&lv) {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("unification failed, occurs"), err)))?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            self.substs.push(th);
                        }
                        (rt, Type::TypeVariable(lv)) if self.remove_want_solve(&lv) => {
                            if rt.occurs(&lv) {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("unification failed, occurs"), err)))?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            self.substs.push(th);
                        }
                        (Type::TypeVariable(lv), rt) => {
                            if rt.occurs(&lv) {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("unification failed, occurs"), err)))?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            self.substs.push(th);
                        }
                        (rt, Type::TypeVariable(lv)) => {
                            if rt.occurs(&lv) {
                                Err(UnifyErr::Contradiction(ErrorComment::new(format!("unification failed, occurs"), err)))?;
                            }
                            let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                            self.subst(&th);
                            self.substs.push(th);
                        }
                        /*(Type::TypeVariable(_), Type::TypeVariable(_)) => {
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
                        }*/
                        (l, r) => {
                            Err(UnifyErr::Contradiction(ErrorComment::new(format!("unfication failed, {:?} != {:?}", l, r), err)))?
                        }
                    }
                }
            }
            if self.change_cnt == 0 && self.equs.len() > 0 {
                log::debug!("{:?}", self.equs);
                let change = self.equs.iter_mut().map(|e| {
                    e.replace_solve_change(SolveChange::not())
                }).fold(SolveChange::not(), |a, b| a & b);
                return Err(UnifyErr::Deficiency(change.into_detail(format!("solve cnt = 0"), Error::None)));
            }
        }

        let voids = self.substs.iter()
            .filter(|TypeSubst { tv, .. }| self.not_void_vars.contains(tv))
            .filter(|TypeSubst{ t, .. }| *t == Type::from_str("void"))
            .collect::<Vec<_>>();
        if !self.want_solve.is_empty() {
            Err(UnifyErr::Deficiency(ErrorComment::empty(format!("want_solve {:?} cant solve now", self.want_solve))))
        }
        else if voids.len() > 0 {
            Err(UnifyErr::Contradiction(ErrorComment::empty(format!("voids appear {:?}", voids))))
        }
        else {
            Ok(())
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
    log::debug!("trs: {:?}", trs);
    equs.add_equation(t.clone(), Type::Generics(TypeId::from_str("Hoge"), vec![Type::from_str("i64")]));
    equs.add_equation(a, Type::Member(Box::new(t), Identifier::from_str("x")));
    log::debug!("{:?}", equs.unify(&trs));
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
    log::debug!("trs: {:?}", trs);
    equs.add_equation(t.clone(), Type::from_str("Hoge"));
    //equs.add_equation(Type::Type(TypeSpec::from_id(&TypeId::from_str("i64"))), Type::Member(Box::new(t), Identifier::from_str("x")));
    log::debug!("{:?}", equs.unify(&trs));
}*/
