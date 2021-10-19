//use nom::branch::*;
use nom::IResult;

use nom::character::complete::*;

use nom::sequence::*; 
use nom::combinator::*;
use nom::branch::*;

use crate::statement::{ Statement, parse_statement };
use crate::expression::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
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
            Some(Expression::ForExpr(ref forexpr)) => format!("{};\n", forexpr.transpile(ta)),
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

impl MoveCheck for Block {
    fn move_check(&self, top_mc: &mut VariablesMoveChecker, trs: &TraitsInfo) -> Result<MoveResult, String> {
        let mut mc = VariablesMoveChecker::new();
        for statement in self.statements.iter() {
            statement.move_check(&mut mc, trs)?;
        }
        if let Some(ref expr) = self.return_exp {
            let res = expr.move_check(&mut mc, trs)?;
            mc.move_result(res)?;
        }
        top_mc.solve_lazys(mc)?;
        Ok(MoveResult::Right)
    }
}


enum BlockElement {
    End(Option<Expression>),
    Statement(Statement),
}

fn parse_block_end(s: &str) -> IResult<&str, BlockElement> {
    let (s, (_, expr, _, _)) = tuple((multispace0, opt(parse_expression), multispace0, char('}')))(s)?;
    Ok((s, BlockElement::End(expr)))
}

fn parse_block_statement_with_block(s: &str) -> IResult<&str, BlockElement> {
    let (s, (_, expr)) = tuple((multispace0, alt((parse_if_expr, parse_for_expr))))(s)?;
    Ok((s, BlockElement::Statement(Statement::Expression(expr, Tag::new()))))
}

fn parse_block_statement_without_block(s: &str) -> IResult<&str, BlockElement> {
    let (s, (_, stmt, _, _)) = tuple((multispace0, parse_statement, multispace0, char(';')))(s)?;
    Ok((s, BlockElement::Statement(stmt)))
}

pub fn parse_block(s: &str) -> IResult<&str, Block> {
    let (mut s, _) = tuple((char('{'), multispace0))(s)?;
    let mut statements = Vec::new();
    loop {
        match alt((parse_block_end, parse_block_statement_with_block, parse_block_statement_without_block))(s)? {
            (s, BlockElement::End(return_exp)) => {
                break Ok((s, Block { statements, return_exp }))
            }
            (ss, BlockElement::Statement(stmt)) => {
                s = ss;
                statements.push(stmt);
            }
        }
    }
}

#[test]
fn parse_block_test() {
    println!("{:?}", parse_block("{ let x = 0; let y = 91; let z = 1333; func(x * x, y, z); }").unwrap());
    println!("{:?}", parse_block("{ let x = 0; let y = 91; let z = 1333; func(x * x, y, z) }").unwrap());
    println!("{:?}", parse_block("{ let x = 0; let y = 91; if x == y { x; } else { y; } func(x * x, y, z); }").unwrap());
    println!("{:?}", parse_block("{ let x = 0; let y = 91; if x == y { x } else { y } }").unwrap());
}
