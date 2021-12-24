use std::collections::HashMap;

use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;

use crate::identifier::{ Identifier, parse_identifier, Tag };
use crate::type_id::*;
use crate::type_spec::*;
use crate::expression::*;
use crate::unary_expr::*;
//use crate::unary_expr::Variable;
use crate::trans::*;
use crate::unify::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::error::*;
use crate::unify::MemberEquation;
use crate::content_str::*;

#[derive(Debug)]
pub struct StructInstantiation {
    pub struct_id: TypeId,
    pub members: HashMap<Identifier, (Expression, SourceRange)>,
    tag: Tag,
    range: SourceRange,
}

impl GenType for StructInstantiation {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let inst_ty = self.tag.generate_type_variable("InstantiationType", 0, equs);
        for (id, (expr, range)) in self.members.iter() {
            let st = Box::new(inst_ty.clone());
            let right = expr.gen_type(equs, trs)?;
            equs.add_equation(Type::Member( MemberEquation {
                caller_type: st,
                id: id.clone(),
                caller_range: range.hint("a element of struct instantiation", self.range.hint("struct instantiation here", ErrorHint::None)),
            }), right, ErrorComment::new(format!("member instantiation euqation for member"), range.hint("member here", self.range.hint("struct instantiation here", ErrorHint::None)).err()));
        }
        let struct_ty = TypeSpec::from_id(&self.struct_id).generics_to_type(&GenericsTypeMap::empty(), equs, trs)?;
        equs.add_equation(inst_ty.clone(), struct_ty, ErrorComment::new(format!("type variable for instantiation type"), self.range.hint("struct instantiation here", ErrorHint::None).err()));
        Ok(inst_ty)
    }
}

fn parse_member(s: ContentStr<'_>) -> IResult<ContentStr<'_>, (Identifier, (Expression, SourceRange))> {
    let (s, ((id, _, _, _, ty), range)) = with_range(tuple((parse_identifier, multispace0, char(':'), multispace0, parse_expression)))(s)?;
    Ok((s, (id, (ty, range))))
}

pub fn parse_struct_instantiation(s: ContentStr<'_>) -> IResult<ContentStr<'_>, UnaryExpr> {
    let (s, ((struct_id, _, _, _, opts, _), range)) = with_range(tuple((parse_type_id, multispace0, char('{'), multispace0,
                         opt(tuple((parse_member, many0(tuple((multispace0, char(','), multispace0, parse_member))), opt(tuple((multispace0, char(',')))), multispace0))),
                         char('}'))))(s)?;
    let members = match opts {
        None => HashMap::new(),
        Some((mem, mems, _, _)) => {
            let mut vec = vec![mem];
            for (_, _, _, mem) in mems {
                vec.push(mem);
            }
            vec.into_iter().collect()
        }
    };
    Ok((s, UnaryExpr::StructInst(StructInstantiation { struct_id, members, tag: Tag::new(), range })))
}

impl Transpile for StructInstantiation {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let args = ta.get_struct_members_order(&self.struct_id).iter()
            .map(|mem| self.members.get(mem).unwrap())
            .map(|(exp, _)| exp.transpile(ta))
            .collect::<Vec<_>>().join(", ");
        format!("{}({})", ta.annotation(self.tag.get_num(), "InstantiationType", 0).transpile(ta), args)
    }
}

impl MutCheck for StructInstantiation {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        for (_, (expr, _range)) in self.members.iter() {
            expr.mut_check(ta, vars)?;
        }
        Ok(MutResult::NotMut)
    }
}

impl MoveCheck for StructInstantiation {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        for (_, (expr, _range)) in self.members.iter() {
            let res = expr.move_check(mc, ta)?;
            mc.move_result(res)?;
        }
        Ok(MoveResult::Right)
    }
}

#[test]
fn parse_struct_instantiation_test() {
    log::debug!("{:?}", parse_struct_instantiation("MyStruct { a: 1i64 + 2i64, b: val, }".into_content(0)).ok());
}

