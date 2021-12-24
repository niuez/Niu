//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::bytes::complete::*;

use crate::expression::{ Expression, parse_expression };
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::error::*;
use crate::content_str::*;

#[derive(Debug)]
pub struct Substitute {
    pub into_expr: Expression,
    pub value: Expression,
}

impl GenType for Substitute {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let left = self.into_expr.gen_type(equs, trs)?;
        let right = self.value.gen_type(equs, trs)?;
        equs.add_equation(left, right, ErrorComment::empty(format!("substitute equation")));
        Ok(Type::End)
    }
}

impl Transpile for Substitute {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        format!("{} = {}",
                self.into_expr.transpile(ta),
                self.value.transpile(ta)
        )
    }
}

impl MutCheck for Substitute {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        let into_expr = self.into_expr.mut_check(ta, vars)?;
        self.value.mut_check(ta, vars)?;
        if let MutResult::Mut = into_expr {
            Ok(MutResult::NoType)
        }
        else {
            Err(format!("{:?} is not mutable", self.into_expr))
        }
    }
}

impl MoveCheck for Substitute {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        let left_res = self.into_expr.move_check(mc, ta)?;
        let right_res = self.value.move_check(mc, ta)?;
        mc.move_result(right_res)?;
        mc.live_result(left_res)?;
        Ok(MoveResult::Right)
    }
}

pub fn parse_substitute(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Substitute> {
    let (s, (into_expr, _, _e, _, value)) = tuple((parse_expression, multispace0, tag("="), multispace0, parse_expression))(s)?;
    Ok((s, Substitute { into_expr, value, }))
}
