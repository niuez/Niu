use crate::unify::*;
use crate::identifier::*;
use crate::error::*;

#[derive(Debug, Clone)]
pub struct CallEquation {
    pub caller_type: Option<Box<Type>>,
    pub trait_gen: Option<TraitGenerics>,
    pub func_id: Identifier,
    pub args: Vec<Type>,
    pub caller_range: ErrorHint,
    pub tag: Tag,
}

impl PartialEq for CallEquation {
    fn eq(&self, _right: &Self) -> bool { false }
}
impl Eq for CallEquation {}

impl CallEquation {
    pub fn occurs(&self, tv: &TypeVariable) -> bool {
        self.caller_type.as_ref().map_or(false, |t| t.occurs(tv))
            || self.args.iter().map(|arg| arg.occurs(tv)).any(|f| f)
    }
    pub fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        //log::info!("subst {:?}", theta);
        let mut changed = SolveChange::not();
        changed &= self.caller_type.as_mut().map_or(SolveChange::not(), |t| t.subst(theta));
        changed &= self.trait_gen.as_mut().map_or(SolveChange::not(), |t| t.subst(theta));
        self.args.iter_mut().map(|arg| arg.subst(theta))
            .fold(changed, |a, b| a & b)
    }

    pub fn solve(mut self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let caller_changed = if let Some(caller_type) = self.caller_type {
            let (caller_type, caller_changed) = equs.solve_relations(*caller_type, trs)?;
            self.caller_type = Some(Box::new(caller_type));
            caller_changed
        }
        else {
            SolveChange::not()
        };
        let (trait_gen, trait_changed) = match self.trait_gen {
            None => (None, SolveChange::not()),
            Some(t) => {
                let (t, changed) = t.solve(equs, trs)?;
                (Some(t), changed)
            }
        };
        self.trait_gen = trait_gen;
        let args = self.args.into_iter().map(|arg| equs.solve_relations(arg, trs)).collect::<Result<Vec<_>, UnifyErr>>()?;
        let (args, changes): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let args_changed = changes.into_iter().fold(SolveChange::not(), |b, a| b & a);
        self.args = args;

        let next_change = caller_changed & trait_changed & args_changed;

        match trs.regist_for_call_equtions(equs, &self) {
            Ok(ret_ty) => {
                Ok((ret_ty, SolveChange::Changed))
            }
            Err(UnifyErr::Contradiction(err)) => {
                Err(UnifyErr::Contradiction(err))
                //Ok((Type::CallEquation(self), next_change))
            }
            Err(UnifyErr::Deficiency(err)) => {
                Ok((Type::CallEquation(self), next_change & SolveChange::err(err)))
            }
        }
    }
}
