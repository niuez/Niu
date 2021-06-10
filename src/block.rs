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

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_exp: Option<Expression>,
}

impl GenType for Block {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        for s in self.statements.iter() {
            s.gen_type(equs, trs)?;
        }
        self.return_exp.as_ref().unwrap().gen_type(equs, trs)
    }
}

impl Transpile for Block {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut vec = self.statements.iter().map(|s| s.transpile(ta)).collect::<Vec<_>>();
        if let Some(ref return_exp) = self.return_exp {
            vec.push(format!("return {};", return_exp.transpile(ta)));
        }
        vec.join(";\n")
    }
}

pub fn parse_block(s: &str) -> IResult<&str, Block> {
    let (s, (vec, _, return_exp, _)) = tuple((many0(tuple((space0, parse_statement, space0, tag(";")))), space0, opt(parse_expression), space0))(s)?;
    let mut statements = Vec::new();
    for (_, st, _, _) in vec {
        statements.push(st);
    }
    Ok((s, Block { statements, return_exp }))
}

#[test]
fn parse_block_test() {
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z);"));
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z)"));
}
