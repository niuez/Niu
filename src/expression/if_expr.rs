//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;
use nom::combinator::*;

use crate::identifier::*;
use crate::expression::{ Expression, parse_expression };
use crate::block::{ Block, parse_block };
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;

#[derive(Debug)]
struct IfPair {
    cond: Expression,
    block: Block,
}

#[derive(Debug)]
pub struct IfExpr {
    ifp: IfPair,
    elifp: Vec<IfPair>,
    el_block: Option<Block>,
    tag: Tag,
}

impl IfExpr {
    pub fn transpile_for_return(&self, ta: &TypeAnnotation) -> String {
        let if_trans = format!("if({}) {{\n{}}}\n", self.ifp.cond.transpile(ta), self.ifp.block.transpile(ta));
        let elif_trans = self.elifp.iter().map(|ifp| format!("else if({}) {{\n{}}}\n", ifp.cond.transpile(ta), ifp.block.transpile(ta))).collect::<Vec<_>>().join("");
        let else_trans = match self.el_block.as_ref() {
            Some(el_block) => format!("else {{\n{}}}\n", el_block.transpile(ta)),
            None => format!(""),
        };
        format!("{}{}{}", if_trans, elif_trans, else_trans)
    }
}

impl GenType for IfExpr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let cond_type = self.ifp.cond.gen_type(equs, trs)?;
        let return_type = self.tag.generate_type_variable("ReturnType", 0, equs);
        let bl_type = self.ifp.block.gen_type(equs, trs)?;
        equs.add_equation(cond_type, Type::from_str("bool"));
        equs.add_equation(return_type.clone(), bl_type);
        for IfPair { cond, block } in self.elifp.iter() {
            let cond_type = cond.gen_type(equs, trs)?;
            let bl2_type = block.gen_type(equs, trs)?;
            equs.add_equation(cond_type, Type::from_str("bool"));
            equs.add_equation(return_type.clone(), bl2_type);
        }
        let el_bl_type = match self.el_block.as_ref() {
            Some(el_block) => el_block.gen_type(equs, trs)?,
            None => Type::from_str("void"),
        };
        equs.add_equation(return_type.clone(), el_bl_type);
        Ok(return_type)
    }
}

impl Transpile for IfExpr {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let if_trans = format!("if({}) {{\n {}}}\n", self.ifp.cond.transpile(ta), self.ifp.block.transpile(ta));
        let elif_trans = self.elifp.iter().map(|ifp| format!("else if({}) {{\n {}}}\n", ifp.cond.transpile(ta), ifp.block.transpile(ta))).collect::<Vec<_>>().join("");
        let else_trans = match self.el_block.as_ref() {
            Some(el_block) => format!("else {{\n{}}}\n", el_block.transpile(ta)),
            None => format!(""),
        };
        if Type::from_str("void") == ta.annotation(self.tag.get_num(), "ReturnType", 0) {
            format!("{}{}{}", if_trans, elif_trans, else_trans)
        }
        else {
            format!("[&](){{ {}{}{} \n}}()", if_trans, elif_trans, else_trans)
        }
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
        if let Some(el_block) = self.el_block.as_ref() {
            el_block.mut_check(ta, vars)?;
        }
        Ok(MutResult::NotMut)
    }
}

impl MoveCheck for IfExpr {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        self.ifp.cond.move_check(mc, ta)?;
        let mut base_mc = VariablesMoveChecker::new();
        {
            let mut ifp_mc = VariablesMoveChecker::new();
            self.ifp.block.move_check(&mut ifp_mc, ta)?;
            base_mc.parallel_merge(ifp_mc);
        }
        for IfPair { cond, block } in self.elifp.iter() {
            cond.move_check(mc, ta)?;
            let mut ifp_mc = VariablesMoveChecker::new();
            block.move_check(&mut ifp_mc, ta)?;
            base_mc.parallel_merge(ifp_mc);
        }
        if let Some(el_block) = self.el_block.as_ref() {
            let mut ifp_mc = VariablesMoveChecker::new();
            el_block.move_check(&mut ifp_mc, ta)?;
            base_mc.parallel_merge(ifp_mc);
        }
        mc.solve_lazys(base_mc)?;
        Ok(MoveResult::Right)
    }
}


pub fn parse_if_expr(s: &str) -> IResult<&str, Expression> {
    let (s, (_, _, if_cond, _, if_block, _, many, el_block, _)) = tuple((tag("if"), multispace1, parse_expression, multispace0, parse_block, multispace0,
                        many0(tuple((tag("else"), multispace1, tag("if"), multispace1, parse_expression, multispace0, parse_block, multispace0))),
                        opt(tuple((tag("else"), multispace0, parse_block))), multispace0))(s)?;
    let ifp = IfPair { cond: if_cond, block: if_block };
    let elifp = many.into_iter().map(|(_, _, _, _, cond, _, block, _)| IfPair { cond, block }).collect::<Vec<_>>();
    let el_block = el_block.map(|(_, _, block)| block);
    Ok((s, Expression::IfExpr(Box::new(IfExpr { ifp, elifp, el_block, tag: Tag::new(), }))))
}

#[test]
fn parse_if_expr_test() {
    println!("{:?}", parse_if_expr("if a == b { c } else { d }").ok());
}
