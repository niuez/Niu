//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*; 
use nom::bytes::complete::*;
use nom::combinator::*;

use crate::statement::*;
use crate::expression::*;
use crate::block::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::type_spec::*;
use crate::let_declaration::*;
use crate::error::*;

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
        equs.add_equation(cond_type, Type::from_str("bool"), ErrorComment::empty(format!("for condition expression must be return bool")));
        equs.add_equation(bl_type, Type::from_str("void"), ErrorComment::empty(format!("for block must be return void")));
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
        format!("for({}; {}; {}){{\n{}}}", init_trans, cond_trans, update_trans, block_trans)
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

impl MoveCheck for ForExpr {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        let mut for_init_mc = VariablesMoveChecker::new();
        self.init.move_check(&mut for_init_mc, ta)?;
        let mut for_loop_mc = VariablesMoveChecker::new();
        self.cond.move_check(&mut for_loop_mc, ta)?;
        self.block.move_check(&mut for_loop_mc, ta)?;
        self.update.move_check(&mut for_loop_mc, ta)?;
        if for_loop_mc.is_lazy_empty() {
            for_init_mc.solve_lazys(for_loop_mc)?;
            mc.solve_lazys(for_init_mc)?;
            Ok(MoveResult::Right)
        }
        else {
            Err(format!("for loop move error, {:?}", for_loop_mc.print_lazy()))
        }
    }
}

pub fn parse_for_expr_paren(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Expression> {
    let (s, (_, _, _, _, init, _, _, _, cond, _, _, _, update, _, _, _, block)) =
        tuple((tag("for"), multispace0, char('('), multispace0,
            parse_statement, multispace0, char(';'), multispace0,
            parse_expression, multispace0, char(';'), multispace0,
            alt((parse_substitute_to_statement, parse_expression_to_statement)), multispace0, char(')'), multispace0, parse_block))(s)?;
    Ok((s, Expression::ForExpr(Box::new(ForExpr { init, cond, update, block }))))
}

fn parse_initial_variable(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Statement> {
    let (s, (id, _, tyinfo, _, _e, _, value)) = tuple((parse_identifier, multispace0, opt(tuple((char(':'), multispace0, parse_type_spec))), multispace0, tag("="), multispace0, parse_expression))(s)?;
    Ok((s, Statement::LetDeclaration(LetDeclaration { vars: VariableDeclaration::Leaf(id, true), type_info: tyinfo.map(|(_, _, type_info)| type_info ), value })))
}

pub fn parse_for_expr_no_parentheses(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Expression> {
    let (s, (_, _, init, _, _, _, cond, _, _, _, update, _, block)) =
        tuple((tag("for"), multispace1,
            parse_initial_variable, multispace0, char(';'), multispace0,
            parse_expression, multispace0, char(';'), multispace0,
            alt((parse_substitute_to_statement, parse_expression_to_statement)), multispace0, parse_block))(s)?;
    Ok((s, Expression::ForExpr(Box::new(ForExpr { init, cond, update, block }))))
}


pub fn parse_for_expr(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Expression> {
    alt((parse_for_expr_paren, parse_for_expr_no_parentheses))(s)
}

#[test]
fn parse_for_expr_test() {
    println!("{:?}", parse_for_expr("for(let mut i = 0; i < 5; i = i + 1) {}".into_content(0)).unwrap());
    println!("{:?}", parse_for_expr("for i = 0; i < 5; i = i + 1 {}".into_content(0)).unwrap());
}
