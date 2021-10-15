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
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::type_spec::*;

#[derive(Debug)]
pub struct LetDeclaration {
    pub id: Identifier,
    pub is_mut: bool,
    pub type_info: Option<TypeSpec>,
    pub value: Expression,
}

impl GenType for LetDeclaration {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let alpha = self.id.generate_not_void_type_variable("LetType", 0, equs);
        equs.regist_variable(Variable::from_identifier(self.id.clone()), alpha.clone());
        let value_type = self.value.gen_type(equs, trs)?;
        equs.add_equation(alpha.clone(), value_type);
        if let Some(ref t) = self.type_info {
            let t_type = t.generics_to_type(&GenericsTypeMap::empty(), equs, trs)?;
            equs.add_equation(alpha.clone(), t_type);
        }
        Ok(Type::End)
    }
}

impl Transpile for LetDeclaration {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        format!("{}{} {} = {}",
                ta.annotation(self.id.get_tag_number(), "LetType", 0).transpile(ta),
                if self.is_mut { "" } else { " const" },
                self.id.into_string(),
                self.value.transpile(ta)
        )
    }
}

impl MutCheck for LetDeclaration {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        vars.regist_variable(&self.id, self.is_mut);
        if ta.annotation(self.id.get_tag_number(), "LetType", 0).is_reference() && self.is_mut {
            Err(format!("Reference cant be mutable, {:?}", self.id))
        }
        else {
            Ok(MutResult::NoType)
        }
    }
}

impl MoveCheck for LetDeclaration {
    fn move_check(&self, mc: &mut VariablesMoveChecker, trs: &TraitsInfo) -> Result<MoveResult, String> {
        let res = self.value.move_check(mc, trs)?;
        mc.move_result(&res)?;
        mc.regist_var(&self.id);
        Ok(MoveResult::Right)
    }
}


pub fn parse_let_declaration(s: &str) -> IResult<&str, LetDeclaration> {
    let (s, (_let, _, is_mut, id, _, tyinfo, _, _e, _, value)) = tuple((tag("let"), multispace1, opt(tuple((tag("mut"), multispace1))), parse_identifier, multispace0, opt(tuple((char(':'), multispace0, parse_type_spec))), multispace0, tag("="), multispace0, parse_expression))(s)?;
    Ok((s, (LetDeclaration { id, is_mut: is_mut.is_some(), type_info: tyinfo.map(|(_, _, type_info)| type_info ), value })))
}

#[test]
fn parse_decl_test() {
    log::debug!("{:?}", parse_let_declaration("let x = 1 + 2").ok());
    log::debug!("{:?}", parse_let_declaration("let x: i64 = 1 + 2").ok());
}
