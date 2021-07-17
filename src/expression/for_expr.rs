//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;

use crate::statement::*;
use crate::expression::*;
use crate::block::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;

#[derive(Debug)]
pub struct ForExpr {
    init: Statement,
    cond: Expression,
    update: Statement,
    block: Block,
}

impl GenType for ForExpr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        equs.into_scope();
        self.init.gen_type(equs, trs)?;
        let cond_type = self.cond.gen_type(equs, trs)?;
        self.update.gen_type(equs, trs)?;
        let bl_type = self.block.gen_type(equs, trs)?;
        equs.add_equation(cond_type, Type::from_str("bool"));
        equs.add_equation(bl_type, Type::from_str("void"));
        equs.out_scope();
        Ok(Type::from_str("void"))
    }
}

impl Transpile for ForExpr {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let init_trans = self.init.transpile(ta);
        let cond_trans = self.cond.transpile(ta);
        let update_trans = self.update.transpile(ta);
        let block_trans = self.block.transpile(ta);
        format!("for({}; {}; {}){{\n{}\n}}", init_trans, cond_trans, update_trans, block_trans)
    }
}

impl MutCheck for ForExpr {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        vars.into_scope();
        self.init.mut_check(ta, vars)?;
        self.cond.mut_check(ta, vars)?;
        self.update.mut_check(ta, vars)?;
        self.block.mut_check(ta, vars)?;
        vars.out_scope();
        Ok(MutResult::NotMut)
    }
}

pub fn parse_for_expr(s: &str) -> IResult<&str, Expression> {
    let (s, (_, _, _, _, init, _, _, _, cond, _, _, _, update, _, _, _, _, _, block, _, _)) =
        tuple((tag("for"), space0, char('('), space0,
            parse_statement, space0, char(';'), space0,
            parse_expression, space0, char(';'), space0,
            alt((parse_substitute_to_statement, parse_expression_to_statement)), space0, char(')'), space0, char('{'), space0,
            parse_block, space0, char('}')))(s)?;
    Ok((s, Expression::ForExpr(Box::new(ForExpr { init, cond, update, block }))))
}

#[test]
fn parse_if_expr_test() {
    println!("{:?}", parse_if_expr("if a == b { c } else { d }"));
}
