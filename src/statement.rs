use nom::branch::*;
use nom::IResult;

use crate::identifier::Tag;
use crate::expression::{ Expression, parse_expression };
use crate::let_declaration::{ LetDeclaration, parse_let_declaration };
use crate::mut_checker::MutCheck;
use crate::mut_checker::MutResult;
use crate::mut_checker::VariablesInfo;
use crate::substitute::*;
use crate::unify::*;
use crate::trans::*;

#[derive(Debug)]
pub enum Statement {
    Expression(Expression, Tag),
    LetDeclaration(LetDeclaration),
    Substitute(Substitute),
}

impl GenType for Statement {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            Statement::Expression(ref e, ref tag) => {
                let expr = e.gen_type(equs, trs)?;
                let alpha = tag.generate_type_variable("StatementType", 0, equs);
                equs.add_equation(alpha, expr);
            }
            Statement::LetDeclaration(ref l) => {
                l.gen_type(equs, trs)?;
            }
            Statement::Substitute(ref s) => {
                s.gen_type(equs, trs)?;
            }
        };
        Ok(Type::End)
    }
}

impl Transpile for Statement {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Statement::Expression(ref e, _) => e.transpile(ta),
            Statement::LetDeclaration(ref l) => l.transpile(ta),
            Statement::Substitute(ref s) => s.transpile(ta),
        }
    }
}

impl MutCheck for Statement {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        match *self {
            Statement::Expression(ref e, _) => e.mut_check(ta, vars),
            Statement::LetDeclaration(ref l) => l.mut_check(ta, vars),
            Statement::Substitute(ref s) => s.mut_check(ta, vars),
        }
    }
}

fn parse_expression_to_statement(s: &str) -> IResult<&str, Statement> {
    let (s, expr) = parse_expression(s)?;
    Ok((s, Statement::Expression(expr, Tag::new())))
}

fn parse_let_declaration_to_statement(s: &str) -> IResult<&str, Statement> {
    let (s, decl) = parse_let_declaration(s)?;
    Ok((s, Statement::LetDeclaration(decl)))
}

fn parse_substitute_to_statement(s: &str) -> IResult<&str, Statement> {
    let (s, subst) = parse_substitute(s)?;
    Ok((s, Statement::Substitute(subst)))
}

pub fn parse_statement(s: &str) -> IResult<&str, Statement> {
    alt((parse_let_declaration_to_statement, parse_substitute_to_statement, parse_expression_to_statement))(s)
}

#[test]
fn parse_statement_test() {
    println!("{:?}", parse_statement("let x = 1 + 2;"));
    println!("{:?}", parse_statement("clamp(x, y, z);"));
}
