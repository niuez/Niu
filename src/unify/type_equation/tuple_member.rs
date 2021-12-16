use crate::unify::*;
use crate::error::*;

#[derive(Debug, Clone)]
pub struct TupleMemberEquation {
    pub ty: Box<Type>,
    pub idx: usize,
    pub caller_range: ErrorHint,
}

impl PartialEq for TupleMemberEquation {
    fn eq(&self, _right: &Self) -> bool { false }
}
impl Eq for TupleMemberEquation {}

impl TupleMemberEquation {
    pub fn occurs(&self, tv: &TypeVariable) -> bool {
        self.ty.as_ref().occurs(tv)
    }
    pub fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        //log::info!("subst {:?}", theta);
        self.ty.as_mut().subst(theta)
    }

    pub fn solve(self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let (ty, change) = equs.solve_relations(*self.ty, trs)?;
        if let Type::Tuple(mut params) = ty {
            if self.idx < params.len() {
                Ok((params.swap_remove(self.idx), SolveChange::Changed))
            }
            else {
                Err(UnifyErr::Contradiction(ErrorUnify::new(
                            format!("tuple member solve"),
                            self.caller_range,
                            ErrorComment::empty(format!("tuple {:?} cannot index {}", params, self.idx))
                )))
            }
        }
        else if let Type::Ref(ty) = ty {
            Ok((Type::TupleMember( TupleMemberEquation {
                ty, 
                idx: self.idx,
                caller_range: self.caller_range
            }), SolveChange::Changed))
        }
        else if let Type::MutRef(ty) = ty {
            Ok((Type::TupleMember( TupleMemberEquation {
                ty, 
                idx: self.idx,
                caller_range: self.caller_range
            }), SolveChange::Changed))
        }
        else {
            let ty_str = format!("{:?} is not tuple", ty);
            Ok((Type::TupleMember( TupleMemberEquation {
                ty: Box::new(ty),
                idx: self.idx,
                caller_range: self.caller_range.clone()
            }), change & SolveChange::err(
                    ErrorUnify::new(
                        format!("cant solve tuple member"),
                        self.caller_range,
                        ErrorComment::empty(ty_str)
                ))))
        }
    }
}
