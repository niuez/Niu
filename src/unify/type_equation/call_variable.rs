use crate::unify::*;
use crate::identifier::*;
use crate::error::*;

#[derive(Debug, Clone)]
pub struct CallVariable {
    pub func_var: Box<Type>,
    pub args: Vec<Type>,
    pub caller_range: ErrorHint,
    pub tag: Tag,
}

impl PartialEq for CallVariable {
    fn eq(&self, _right: &Self) -> bool { false }
}
impl Eq for CallVariable {}

impl CallVariable {
    pub fn occurs(&self, tv: &TypeVariable) -> bool {
        self.func_var.as_ref().occurs(tv) ||
            self.args.iter().map(|a| a.occurs(tv)).any(|b| b)
    }
    pub fn subst(&mut self, theta: &TypeSubst) -> SolveChange {
        //log::info!("subst {:?}", theta);
        let change = self.func_var.as_mut().subst(theta);
        self.args.iter_mut().map(|a| a.subst(theta)).fold(change, |a, b| a & b)
    }

    pub fn solve(self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(Type, SolveChange), UnifyErr> {
        let (func_var, func_var_changed) = equs.solve_relations(*self.func_var, trs)?;
        let args = self.args.into_iter().map(|arg| equs.solve_relations(arg, trs)).collect::<Result<Vec<_>, UnifyErr>>()?;
        let (params, changes): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let args_changed = changes.into_iter().fold(SolveChange::not(), |b, a| b & a);
        let changed = func_var_changed & args_changed;
        match func_var {
            Type::Func(args, ret, info) => {
                let func_info_ty = self.tag.generate_type_variable("FuncTypeInfo", 0, equs);
                equs.add_equation(func_info_ty, Type::Func(args.clone(), ret.clone(), info.clone()), ErrorComment::new(format!("type variabel for func info"), self.caller_range.clone().err()));
                if params.len() == args.len() {
                    for (i, (p, a)) in params.into_iter().zip(args).enumerate() {
                        equs.add_equation(p, a, ErrorComment::new(format!("function {}-th arg equation", i),self.caller_range.clone().err()));
                    }
                }
                else {
                    return Err(UnifyErr::Contradiction(ErrorUnify::new(
                            format!("solve call variable"),
                            self.caller_range.clone(),
                            ErrorComment::empty(format!("length of args is not match, {:?} {:?}", args, params))
                            )))
                }
                Ok((*ret, SolveChange::Changed))
            }
            func_ty => {
                let changed = changed & SolveChange::err(ErrorUnify::new(
                        format!("solve call variable"),
                        self.caller_range.clone(),
                        ErrorComment::empty(format!("caller is not function type, {:?}", func_ty))
                        ));
                Ok((Type::CallVariable(CallVariable { func_var: Box::new(func_ty), args: params, caller_range: self.caller_range, tag: self.tag }), changed))
            }
        }
    }
}
