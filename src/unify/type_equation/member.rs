use crate::unify::*;
use crate::identifier::*;
use crate::error::*;
use crate::traits::*;

#[derive(Debug, Clone)]
pub struct MemberEquation {
    pub caller_type: Box<Type>,
    pub id: Identifier,
    pub caller_range: ErrorHint,
}

impl PartialEq for MemberEquation {
    fn eq(&self, _right: &Self) -> bool { false }
}
impl Eq for MemberEquation {}

impl MemberEquation {
    pub fn occurs(&self, tv: &TypeVariable) -> bool {
        self.caller_type.as_ref().occurs(tv)
    }
    pub fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        //log::info!("subst {:?}", theta);
        self.caller_type.as_mut().subst(theta)
    }

    pub fn solve(self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let (caller_type, caller_changed) = equs.solve_relations(*self.caller_type, trs)?;
        let substs = trs.match_to_member_for_type(&self.id, &caller_type);
        if substs.len() == 1 {
            let mut substs = substs;
            let (subst, impl_trait) = substs.pop().unwrap();
            let before = equs.set_self_type(Some(caller_type.clone()));
            let res = impl_trait.get_trait_method_from_id(equs, trs, &TraitMethodIdentifier { id: self.id.clone() } , &subst, &caller_type);
            equs.set_self_type(before);
            if let Type::Func(args, returns, info) = res {
                let mut iter = args.into_iter();
                let self_ty = iter.next().ok_or(UnifyErr::Contradiction(ErrorComment::empty(format!("trait method {:?} have no argument", self.id))))?;
                equs.add_equation(self_ty.clone(), caller_type.clone());
                let res = Type::Func(iter.collect(), returns, info);
                equs.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
            }
            else { unreachable!() }
        }
        else if let Type::Generics(ref id, ref gens) = caller_type {
            match trs.search_typeid(id).map_err(|st| UnifyErr::Contradiction(ErrorComment::empty(st)))? {
                StructDefinitionInfo::Def(def)  => {
                    let res = def.get_member_type(equs, trs, gens, &self.id).map_err(|st| UnifyErr::Contradiction(st))?;
                    equs.solve_relations(res, trs).map(|(ty, _)| (ty, SolveChange::Changed))
                }
                StructDefinitionInfo::Generics  => Err(UnifyErr::Contradiction(ErrorComment::empty(format!("generics type has no member: {:?}", id)))),
                StructDefinitionInfo::Primitive => Err(UnifyErr::Contradiction(ErrorComment::empty(format!("primitive type has no member: {:?}", id)))),
            }
        }
        else if let Type::Ref(caller_type) = caller_type {
            Ok((Type::Member( MemberEquation { caller_type, id: self.id, caller_range: self.caller_range }), SolveChange::Changed))
        }
        else if let Type::MutRef(caller_type) = caller_type {
            Ok((Type::Member( MemberEquation { caller_type, id: self.id, caller_range: self.caller_range }), SolveChange::Changed))
        }
        else if let Type::SolvedAssociatedType(_, _, _) = caller_type {
            Err(UnifyErr::Contradiction(ErrorComment::empty(format!("SolvedAssociatedType has no member: {:?}", caller_type))))
        }
        else {
            Ok((Type::Member( MemberEquation { caller_type: Box::new(caller_type), id: self.id, caller_range: self.caller_range, }), caller_changed))
        }


    }
}
