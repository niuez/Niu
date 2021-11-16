use crate::unify::*;
use crate::identifier::*;
use crate::error::*;
use crate::traits::*;

#[derive(Debug, Clone)]
pub struct AssociatedTypeEquation {
    pub caller_type: Box<Type>,
    pub trait_gen: Option<TraitGenerics>,
    pub associated_type_id: AssociatedTypeIdentifier,
    pub caller_range: ErrorHint,
    pub tag: Tag,
}

impl PartialEq for AssociatedTypeEquation {
    fn eq(&self, _right: &Self) -> bool { false }
}
impl Eq for AssociatedTypeEquation {}

impl AssociatedTypeEquation {
    pub fn occurs(&self, tv: &TypeVariable) -> bool {
        self.caller_type.as_ref().occurs(tv)
            || self.trait_gen.as_ref().map_or(false, |t| t.generics.iter().map(|g| g.occurs(tv)).any(|b| b))
    }
    pub fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        //log::info!("subst {:?}", theta);
        let mut changed = SolveChange::not();
        changed &= self.caller_type.as_mut().subst(theta);
        changed &= self.trait_gen.as_mut().map_or(SolveChange::not(), |t| t.subst(theta));
        changed
    }

    pub fn solve(mut self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let (caller, caller_changed) = equs.solve_relations(*self.caller_type, trs)?;
        self.caller_type = Box::new(caller);
        let (trait_gen, trait_changed) = match self.trait_gen {
            None => (None, SolveChange::not()),
            Some(t) => {
                let (t, changed) = t.solve(equs, trs)?;
                (Some(t), changed)
            }
        };
        self.trait_gen = trait_gen;

        let next_change = caller_changed & trait_changed;

        match trs.regist_for_associated_type_equation(equs, &self) {
            Ok(ret_ty) => {
                Ok((ret_ty, SolveChange::Changed))
            }
            Err(UnifyErr::Contradiction(err)) => {
                Err(UnifyErr::Contradiction(err))
                //Ok((Type::CallEquation(self), next_change))
            }
            Err(UnifyErr::Deficiency(err)) => {
                Ok((Type::AssociatedType(self), next_change & SolveChange::err(err)))
            }
        }
    }
}
