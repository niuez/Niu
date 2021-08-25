//use nom::branch::*;
use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;

use crate::statement::{ Statement, parse_statement };
use crate::expression::{ Expression, parse_expression };
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_exp: Option<Expression>,
}

impl GenType for Block {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        equs.into_scope();
        for s in self.statements.iter() {
            let _exp = s.gen_type(equs, trs)?;
        }
        let res = self.return_exp.as_ref().map_or(Ok(Type::from_str("void")), |exp| exp.gen_type(equs, trs));
        equs.out_scope();
        res
    }
}

impl Transpile for Block {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let statements = self.statements.iter().map(|s| format!("{};\n", s.transpile(ta))).collect::<Vec<_>>().join("");
        let return_trans = match self.return_exp {
            Some(Expression::IfExpr(ref ifexpr)) => format!("{};\n", ifexpr.transpile_for_return(ta)),
            Some(ref return_exp) => format!("return {};\n", return_exp.transpile(ta)),
            None => format!(""),
        };
        format!("{}{}", statements, return_trans)
    }
}

impl MutCheck for Block {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        vars.into_scope();
        for st in self.statements.iter() {
            st.mut_check(ta, vars)?;
        }
        self.return_exp.as_ref().map_or(Ok(MutResult::NotMut), |exp| exp.mut_check(ta, vars))?;
        vars.out_scope();
        Ok(MutResult::NotMut)
    }
}


pub fn parse_block(s: &str) -> IResult<&str, Block> {
    let (s, (vec, _, return_exp, _)) = tuple((many0(tuple((multispace0, parse_statement, multispace0, tag(";")))), multispace0, opt(parse_expression), multispace0))(s)?;
    let mut statements = Vec::new();
    for (_, st, _, _) in vec {
        statements.push(st);
    }
    Ok((s, Block { statements, return_exp,}))
}

#[test]
fn parse_block_test() {
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z);"));
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z)"));
}
