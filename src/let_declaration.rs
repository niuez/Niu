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
pub struct LetDeclaration {
    pub id: Identifier,
    pub type_info: Option<TypeSpec>,
    pub value: Expression,
}

impl GenType for LetDeclaration {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        let alpha = self.id.generate_type_variable(0);
        equs.regist_variable(Variable::from_identifier(self.id.clone()), alpha.clone());
        let value_type = self.value.gen_type(equs)?;
        equs.add_equation(alpha.clone(), value_type);
        if let Some(ref t) = self.type_info {
            let t_type = t.gen_type(equs)?;
            equs.add_equation(alpha.clone(), t_type);
        }
        Ok(Type::End)
    }
}

impl Transpile for LetDeclaration {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        format!("{} {} = {}",
                ta.annotation(self.id.get_id_number(), 0).transpile(ta),
                self.id.into_string(),
                self.value.transpile(ta)
        )
    }
}

pub fn parse_let_declaration(s: &str) -> IResult<&str, LetDeclaration> {
    let (s, (_let, _, id, _, tyinfo, _, _e, _, value)) = tuple((tag("let"), space1, parse_identifier, space0, opt(tuple((char(':'), space0, parse_type_spec))), space0, tag("="), space0, parse_expression))(s)?;
    Ok((s, (LetDeclaration { id, type_info: tyinfo.map(|(_, _, type_info)| type_info ), value })))
}

#[test]
fn parse_decl_test() {
    println!("{:?}", parse_let_declaration("let x = 1 + 2"));
    println!("{:?}", parse_let_declaration("let x: i64 = 1 + 2"));
}
