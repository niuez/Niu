use nom::branch::*;
use nom::bytes::complete::*;
use nom::IResult;

use crate::identifier::Tag;
use crate::expression::{ Expression, parse_expression };
use crate::let_declaration::{ LetDeclaration, parse_let_declaration };
use crate::mut_checker::MutCheck;
use crate::mut_checker::MutResult;
use crate::mut_checker::VariablesInfo;
use crate::move_checker::*;
use crate::substitute::*;
use crate::unify::*;
use crate::trans::*;
use crate::error::*;
use crate::content_str::*;


#[derive(Debug)]
pub enum Statement {
    Expression(Expression, Tag),
    LetDeclaration(LetDeclaration),
    Substitute(Substitute),
    Break,
    Continue,
}

impl GenType for Statement {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            Statement::Expression(ref e, ref tag) => {
                let expr = e.gen_type(equs, trs)?;
                let alpha = tag.generate_type_variable("StatementType", 0, equs);
                equs.add_equation(alpha, expr, ErrorComment::empty(format!("type variable for statement type")));
            }
            Statement::LetDeclaration(ref l) => {
                l.gen_type(equs, trs)?;
            }
            Statement::Substitute(ref s) => {
                s.gen_type(equs, trs)?;
            }
            Statement::Break => {}
            Statement::Continue => {}
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
            Statement::Break => format!("break"),
            Statement::Continue => format!("continue"),
        }
    }
}

impl MutCheck for Statement {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        match *self {
            Statement::Expression(ref e, _) => e.mut_check(ta, vars),
            Statement::LetDeclaration(ref l) => l.mut_check(ta, vars),
            Statement::Substitute(ref s) => s.mut_check(ta, vars),
            Statement::Break => Ok(MutResult::NoType),
            Statement::Continue => Ok(MutResult::NoType),
        }
    }
}

impl MoveCheck for Statement {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        match *self {
            Statement::Expression(ref e, _) => e.move_check(mc, ta),
            Statement::LetDeclaration(ref l) => l.move_check(mc, ta),
            Statement::Substitute(ref s) => s.move_check(mc, ta),
            Statement::Break => Ok(MoveResult::Right),
            Statement::Continue => Ok(MoveResult::Right),
        }
    }
}

pub fn parse_expression_to_statement(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    let (s, expr) = parse_expression(s)?;
    Ok((s, Statement::Expression(expr, Tag::new())))
}

pub fn parse_let_declaration_to_statement(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    let (s, decl) = parse_let_declaration(s)?;
    Ok((s, Statement::LetDeclaration(decl)))
}

pub fn parse_substitute_to_statement(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    let (s, subst) = parse_substitute(s)?;
    Ok((s, Statement::Substitute(subst)))
}

pub fn parse_break_to_statement(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    let (s, _) = tag("break")(s)?;
    Ok((s, Statement::Break))
}
pub fn parse_continue_to_statement(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    let (s, _) = tag("continue")(s)?;
    Ok((s, Statement::Continue))
}

pub fn parse_statement(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    alt((parse_break_to_statement, parse_continue_to_statement, parse_let_declaration_to_statement, parse_substitute_to_statement, parse_expression_to_statement))(s)
}

#[test]
fn parse_statement_test() {
    log::debug!("{:?}", parse_statement(ContentStr { s: "let x = 1 + 2;", name: 0 }).ok());
    log::debug!("{:?}", parse_statement("clamp(x, y, z);".into_content(0)).ok());
}
