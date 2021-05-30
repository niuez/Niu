use std::collections::HashMap;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;

use crate::identifier::{ Identifier, parse_identifier };
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
}

impl GenType for StructInstantiation {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        for (id, expr) in self.members.iter() {
            let st = Box::new(TypeSpec::from_id(&self.struct_id).gen_type(equs)?);
            let right = expr.gen_type(equs)?;
            equs.add_equation(Type::Member(st, id.clone()), right);
        }
        TypeSpec::from_id(&self.struct_id).gen_type(equs)
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
    Ok((s, UnaryExpr::StructInst(StructInstantiation { struct_id, members })))
}

#[test]
fn parse_struct_instantiation_test() {
    println!("{:?}", parse_struct_instantiation("MyStruct { a: 1i64 + 2i64, b: val, }"));
}

