use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_id::{ TypeId, parse_type_id };
use crate::block::{ Block, parse_block };

#[derive(Debug)]
pub struct FuncDefinition<'a> {
    pub func_id: Identifier<'a>,
    pub args: Vec<(Identifier<'a>, TypeId<'a>)>,
    pub return_type: TypeId<'a>,
    pub block: Block<'a>,
}

pub fn parse_func_definition(s: &str) -> IResult<&str, FuncDefinition> {
    let (s, (_, _, func_id, _, _, _, op, _, _, _, _, return_type, _, _, block, _)) = 
        tuple((tag("fn"), space1, parse_identifier, space0, char('('), space0,
            opt(tuple((parse_identifier, space0, char(':'), space0, parse_type_id, space0, many0(tuple((char(','), space0, parse_identifier, space0, char(':'), space0, parse_type_id, space0))), opt(char(',')), space0))),
            char(')'), space0, tag("->"), space0, parse_type_id, space0, char('{'), parse_block, char('}')))(s)?;
    let args = match op {
        Some((arg0, _, _, _, ty0, _, many, _, _)) => {
            let mut args = vec![(arg0, ty0)];
            for (_, _, arg, _, _, _, ty, _) in many {
                args.push((arg, ty));
            }
            args
        }
        None => Vec::new(),
    };
    Ok((s, FuncDefinition { func_id, args, return_type, block }))
}


#[test]
fn parse_func_definition_test() {
    println!("{:?}", parse_func_definition("fn func(x: i64) -> i64 { let y = x * x; y + x }"))
}
