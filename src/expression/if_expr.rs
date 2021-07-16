//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;

use crate::expression::{ Expression, parse_expression };
use crate::block::{ Block, parse_block };
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;

#[derive(Debug)]
struct IfPair {
    cond: Expression,
    block: Block,
}

#[derive(Debug)]
pub struct IfExpr {
    ifp: IfPair,
    elifp: Vec<IfPair>,
    el_block: Block,
}

impl GenType for IfExpr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let cond_type = self.ifp.cond.gen_type(equs, trs)?;
        let bl_type = self.ifp.block.gen_type(equs, trs)?;
        equs.add_equation(cond_type, Type::from_str("bool"));
        for IfPair { cond, block } in self.elifp.iter() {
            let cond_type = cond.gen_type(equs, trs)?;
            let bl2_type = block.gen_type(equs, trs)?;
            equs.add_equation(cond_type, Type::from_str("bool"));
            equs.add_equation(bl_type.clone(), bl2_type);
        }
        let el_bl_type = self.el_block.gen_type(equs, trs)?;
        equs.add_equation(bl_type.clone(), el_bl_type);
        Ok(bl_type)
    }
}

impl Transpile for IfExpr {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let if_trans = format!("if({}) {{\n {} \n}}\n", self.ifp.cond.transpile(ta), self.ifp.block.transpile(ta));
        let elif_trans = self.elifp.iter().map(|ifp| format!("\nelse if({}) {{\n {} \n}}\n", ifp.cond.transpile(ta), ifp.block.transpile(ta))).collect::<Vec<_>>().join("");
        let else_trans = format!("else {{\n {} \n}}\n", self.el_block.transpile(ta));
        format!("[&](){{ {}{}{} \n}}()", if_trans, elif_trans, else_trans)
    }
}

impl MutCheck for IfExpr {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        self.ifp.cond.mut_check(ta, vars)?;
        self.ifp.block.mut_check(ta, vars)?;
        for IfPair { cond, block } in self.elifp.iter() {
            cond.mut_check(ta, vars)?;
            block.mut_check(ta, vars)?;
        }
        self.el_block.mut_check(ta, vars)?;
        Ok(MutResult::NotMut)
    }
}

pub fn parse_if_expr(s: &str) -> IResult<&str, Expression> {
    let (s, (_, _, if_cond, _, _, if_block, _, _, many, _, _, _, el_block, _, _)) = tuple((tag("if"), space1, parse_expression, space0, char('{'), parse_block, char('}'), space0,
                        many0(tuple((tag("else"), space1, tag("if"), space1, parse_expression, space0, char('{'), parse_block, char('}'), space0))),
                        tag("else"), space1, char('{'), parse_block, char('}'), space0))(s)?;
    let ifp = IfPair { cond: if_cond, block: if_block };
    let elifp = many.into_iter().map(|(_, _, _, _, cond, _, _, block, _, _)| IfPair { cond, block }).collect::<Vec<_>>();
    Ok((s, Expression::IfExpr(Box::new(IfExpr { ifp, elifp, el_block }))))
}

#[test]
fn parse_if_expr_test() {
    println!("{:?}", parse_if_expr("if a == b { c } else { d }"));
}
