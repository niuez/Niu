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
//use crate::unary_expr::Variable;
use crate::trans::*;
use crate::func_definition::*;

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub struct_id: TypeId,
    pub members: HashMap<Identifier, TypeSpec>,
}

fn parse_member(s: &str) -> IResult<&str, (Identifier, TypeSpec)> {
    let (s, (id, _, _, _, ty)) = tuple((parse_identifier, space0, char(':'), space0, parse_type_spec))(s)?;
    Ok((s, (id, ty)))
}


pub fn parse_struct_definition(s: &str) -> IResult<&str, StructDefinition> {
    let (s, (_, _, struct_id, _, _, _, opts, _)) = tuple((tag("struct"), space1, parse_type_id, space0, char('{'), space0,
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
    Ok((s, StructDefinition { struct_id, members }))
}



#[test]
fn parse_struct_definition_test() {
    println!("{:?}", parse_struct_definition("struct MyStruct { a: i64, b: u64, }"));
}
