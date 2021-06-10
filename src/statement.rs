use nom::branch::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };
use crate::let_declaration::{ LetDeclaration, parse_let_declaration };
use crate::unify::*;
use crate::trans::*;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    LetDeclaration(LetDeclaration),
}

impl GenType for Statement {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            Statement::Expression(ref e) => e.gen_type(equs, trs)?,
            Statement::LetDeclaration(ref l) => l.gen_type(equs, trs)?,
        };
        Ok(Type::End)
    }
}

impl Transpile for Statement {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Statement::Expression(ref e) => e.transpile(ta),
            Statement::LetDeclaration(ref l) => l.transpile(ta),
        }
    }
}

fn parse_expression_to_statement(s: &str) -> IResult<&str, Statement> {
    let (s, expr) = parse_expression(s)?;
    Ok((s, Statement::Expression(expr)))
}

fn parse_let_declaration_to_statement(s: &str) -> IResult<&str, Statement> {
    let (s, decl) = parse_let_declaration(s)?;
    Ok((s, Statement::LetDeclaration(decl)))
}

pub fn parse_statement(s: &str) -> IResult<&str, Statement> {
    alt((parse_let_declaration_to_statement, parse_expression_to_statement))(s)
}

#[test]
fn parse_statement_test() {
    println!("{:?}", parse_statement("let x = 1 + 2;"));
    println!("{:?}", parse_statement("clamp(x, y, z);"));
}
