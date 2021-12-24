//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*; 
use nom::bytes::complete::*;

use crate::unary_expr::Variable;
use crate::expression::*;
use crate::block::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::error::*;

#[derive(Debug)]
pub struct ForEachExpr {
    id: Identifier,
    iter: Expression,
    block: Block,
}

impl GenType for ForEachExpr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        equs.into_scope();
        let alpha = self.id.generate_not_void_type_variable("LetType", 0, equs);
        equs.regist_variable(Variable::from_identifier(self.id.clone()), alpha.clone());
        let iter_ty = self.iter.gen_type(equs, trs)?;
        equs.add_equation(alpha.clone(), Type::AssociatedType(AssociatedTypeEquation {
            caller_type: Box::new(iter_ty),
            trait_gen: Some(TraitGenerics { trait_id: TraitId { id: Identifier::from_str("Iterator") } , generics: Vec::new()}),
            associated_type_id: AssociatedTypeIdentifier { id: Identifier::from_str("Item") },
            caller_range: ErrorHint::None,
            tag: Tag::new(),
        }), ErrorComment::empty(format!("type variable for variable of foreach")));

        let bl_type = self.block.gen_type(equs, trs)?;
        equs.add_equation(bl_type, Type::from_str("void"), ErrorComment::empty(format!("foreach block must be return void")));
        equs.out_scope();
        Ok(Type::from_str("void"))
    }
}

impl Transpile for ForEachExpr {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let range_var = format!("__range{}", self.id.get_tag_number());
        let begin = format!("__begin{}", self.id.get_tag_number());
        let end = format!("__end{}", self.id.get_tag_number());
        let (begin_func, end_func) = match ta.annotation(self.id.get_tag_number(), "LetType", 0) {
            Type::Ref(_) => (format!("std::cbegin({})", range_var), format!("std::cend({})", range_var)),
            Type::MutRef(_) => (format!("std::begin({})", range_var), format!("std::end({})", range_var)),
            _ => (format!("std::make_move_iterator(std::begin({}))", range_var), format!("std::make_move_iterator(std::end({}))", range_var))
        };
        let iter_trans = self.iter.transpile(ta);
        let block_trans = self.block.transpile(ta);
        let elem_ty = ta.annotation(self.id.get_tag_number(), "LetType", 0).transpile(ta);
        format!("{{\nauto&& {} = {};\nfor(auto {} = {}, {} = {}; {} != {}; ++{}){{\n{} {} = *{};\n{}}}\n}}", range_var, iter_trans, begin, begin_func, end, end_func, begin, end, begin, elem_ty, self.id.into_string(), begin, block_trans)
    }
}

impl MutCheck for ForEachExpr {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        vars.into_scope();
        self.iter.mut_check(ta, vars)?;
        vars.regist_variable(&self.id, false);
        self.block.mut_check(ta, vars)?;
        vars.out_scope();
        Ok(MutResult::NotMut)
    }
}

impl MoveCheck for ForEachExpr {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        let iter_res = self.iter.move_check(mc, ta)?;
        mc.move_result(iter_res)?;
        let mut for_loop_mc = VariablesMoveChecker::new();
        for_loop_mc.regist_var(&self.id);
        self.block.move_check(&mut for_loop_mc, ta)?;
        if for_loop_mc.is_lazy_empty() {
            mc.solve_lazys(for_loop_mc)?;
            Ok(MoveResult::Right)
        }
        else {
            Err(format!("for loop move error, {:?}", for_loop_mc.print_lazy()))
        }
    }
}

pub fn parse_for_each_expr(s: ContentStr<'_>) -> IResult<ContentStr<'_>, Expression> {
    let (s, (_, _, id, _, _, _, iter, _, block)) = tuple((tag("for"), multispace1, parse_identifier,
        multispace1, tag("in"), multispace1, parse_expression,
        multispace0, parse_block
    ))(s)?;
    Ok((s, Expression::ForEachExpr(Box::new(ForEachExpr { id, iter, block }))))
}

#[test]
fn parse_for_expr_test() {
    println!("{:?}", parse_for_expr("for(let mut i = 0; i < 5; i = i + 1) {}".into_content(0)).unwrap());
    println!("{:?}", parse_for_expr("for i = 0; i < 5; i = i + 1 {}".into_content(0)).unwrap());
}
