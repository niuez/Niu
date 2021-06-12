use std::collections::HashMap;

use nom::IResult;
use nom::bytes::complete::*;
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

#[derive(Debug)]
pub struct StructInstantiation {
    pub struct_id: TypeId,
    pub members: HashMap<Identifier, Expression>,
    tag: Tag
}

impl GenType for StructInstantiation {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let inst_ty = self.tag.generate_type_variable(0);
        for (id, expr) in self.members.iter() {
            let st = Box::new(inst_ty.clone());
            let right = expr.gen_type(equs, trs)?;
            equs.add_equation(Type::Member(st, id.clone()), right);
        }
        let struct_ty = TypeSpec::from_id(&self.struct_id).generics_to_type(&GenericsTypeMap::empty(), equs, trs)?;
        equs.add_equation(inst_ty.clone(), struct_ty);
        Ok(inst_ty)
    }
}

fn parse_member(s: &str) -> IResult<&str, (Identifier, Expression)> {
    let (s, (id, _, _, _, ty)) = tuple((parse_identifier, space0, char(':'), space0, parse_expression))(s)?;
    Ok((s, (id, ty)))
}

pub fn parse_struct_instantiation(s: &str) -> IResult<&str, UnaryExpr> {
    let (s, (struct_id, _, _, _, opts, _)) = tuple((parse_type_id, space0, char('{'), space0,
                         opt(tuple((parse_member, many0(tuple((space0, char(','), space0, parse_member))), opt(tuple((space0, char(',')))), space0))),
                         char('}')))(s)?;
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
    Ok((s, UnaryExpr::StructInst(StructInstantiation { struct_id, members, tag: Tag::new() })))
}

impl Transpile for StructInstantiation {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let args = ta.get_struct_members_order(&self.struct_id).iter()
            .map(|mem| self.members.get(mem).unwrap())
            .map(|exp| exp.transpile(ta))
            .collect::<Vec<_>>().join(", ");
        format!("{}({})", ta.annotation(self.tag.get_num(), 0).transpile(ta), args)
    }
}

#[test]
fn parse_struct_instantiation_test() {
    println!("{:?}", parse_struct_instantiation("MyStruct { a: 1i64 + 2i64, b: val, }"));
}

