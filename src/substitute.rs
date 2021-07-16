//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::bytes::complete::*;
use nom::combinator::*;

use crate::identifier::{ Identifier, parse_identifier };
use crate::expression::{ Expression, parse_expression };
use crate::unary_expr::Variable;
use crate::unify::*;
use crate::trans::*;
use crate::type_spec::*;

#[derive(Debug)]
pub struct Substitute {
    pub into_expr: Expression,
    pub value: Expression,
}

impl GenType for Substitute {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let left = self.into_expr.gen_type(equs, trs)?;
        let right = self.value.gen_type(equs, trs)?;
        equs.add_equation(left, right);
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

pub fn parse_let_declaration(s: &str) -> IResult<&str, Substitute> {
    let (s, (into_expr, _, _e, _, value)) = tuple((parse_expression, space0, tag("="), space0, parse_expression))(s)?;
    Ok((s, Substitute { into_expr, value, }))
}

#[test]
fn parse_decl_test() {
    println!("{:?}", parse_let_declaration("let x = 1 + 2"));
    println!("{:?}", parse_let_declaration("let x: i64 = 1 + 2"));
}
