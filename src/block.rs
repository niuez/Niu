//use nom::branch::*;
use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;
use nom::branch::*;

use crate::statement::{ Statement, parse_statement };
use crate::expression::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::identifier::Tag;

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

fn parse_null_statement(s: &str) -> IResult<&str, ()> {
    let (s, _) = many0(tuple((multispace0, tag(";"))))(s)?;
    Ok((s, ()))
}

fn parse_if_statement_specialize(s: &str) -> IResult<&str, Statement> {
    let (s, (_, if_expr)) = tuple((multispace0, parse_if_expr))(s)?;
    Ok((s, Statement::Expression(if_expr, Tag::new())))
}

fn parse_for_statement_specialize(s: &str) -> IResult<&str, Statement> {
    let (s, (_, for_expr)) = tuple((multispace0, parse_for_expr))(s)?;
    Ok((s, Statement::Expression(for_expr, Tag::new())))
}

fn parse_normal_statement(s: &str) -> IResult<&str, Statement> {
    let (s, (_, statement, _, _)) = tuple((multispace0, parse_statement, multispace0, tag(";")))(s)?;
    Ok((s, statement))
}


pub fn parse_block(s: &str) -> IResult<&str, Block> {
    let (s, (statements, _, _, return_exp, _)) = tuple((
            many0(tuple((parse_null_statement, alt((parse_if_statement_specialize, parse_for_statement_specialize, parse_normal_statement))))),
            parse_null_statement, multispace0, opt(parse_expression), multispace0))(s)?;
    let statements = statements.into_iter().map(|(_, s)| s).collect();
    Ok((s, Block { statements, return_exp,}))
}

#[test]
fn parse_block_test() {
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z);").ok());
    println!("{:?}", parse_block("let x = 0; let y = 91; let z = 1333; func(x * x, y, z)").ok());
}
