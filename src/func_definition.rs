use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_id::{ TypeId, parse_type_id };
use crate::block::{ Block, parse_block };
use crate::unify::*;

#[derive(Debug)]
pub struct FuncDefinition {
    pub func_id: Identifier,
    pub generics: Vec<TypeId>,
    pub args: Vec<(Identifier, TypeId)>,
    pub return_type: TypeId,
    pub block: Block,
}

impl GenType for FuncDefinition {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        let func_type = Type::Func(
            self.args.iter().map(|(_, t)| t.gen_type(equs)).collect::<Result<Vec<_>, String>>()?,
            Box::new(self.return_type.gen_type(equs)?)
            );
        equs.add_equation(Type::TypeVariable(TypeVariable::Identifier(self.func_id.clone())), func_type);
        for (i, t) in self.args.iter() {
            let t_type = t.gen_type(equs)?; 
            equs.add_equation(Type::TypeVariable(TypeVariable::Identifier(i.clone())), t_type);
        }
        let result_type = self.block.gen_type(equs)?;
        let return_t = self.return_type.gen_type(equs)?;
        equs.add_equation(result_type, return_t);
        Ok(Type::End)
    }
}

pub fn parse_func_definition(s: &str) -> IResult<&str, FuncDefinition> {
    let (s, (_, _, func_id, _, generics_opt, _, _, op, _, _, _, _, return_type, _, _, block, _)) = 
        tuple((tag("fn"), space1, parse_identifier, space0, opt(tuple((char('<'), space0, opt(tuple((parse_type_id, space0, many0(tuple((char(','), space0, parse_type_id, space0))), opt(char(',')), space0))), char('>'), space0))),
               char('('), space0,
            opt(tuple((parse_identifier, space0, char(':'), space0, parse_type_id, space0, many0(tuple((char(','), space0, parse_identifier, space0, char(':'), space0, parse_type_id, space0))), opt(char(',')), space0))),
            char(')'), space0, tag("->"), space0, parse_type_id, space0, char('{'), parse_block, char('}')))(s)?;
    let generics = match generics_opt {
        Some((_, _, generics_opt, _, _)) => {
            match generics_opt {
                Some((arg0, _, many, _, _)) => {
                    let mut vec = vec![arg0];
                    for (_, _, arg, _) in many {
                        vec.push(arg);
                    }
                    vec
                }
                None => Vec::new(),
            }
        }
        None => Vec::new(),
    };
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
    Ok((s, FuncDefinition { func_id, generics, args, return_type, block }))
}


#[test]
fn parse_func_definition_test() {
    println!("{:?}", parse_func_definition("fn func(x: i64) -> i64 { let y = x * x; y + x }"));
    println!("{:?}", parse_func_definition("fn func2<T>(x: T) -> T { x }"));
    println!("{:?}", parse_func_definition("fn func3<X, Y, Z>(x: X) -> Z { x }"));
}
#[test]
fn gentype_func_definition_test() {
    let (_, t) = parse_func_definition("fn func(x: i64) -> i64 { let y = x * x; y + x }").unwrap();
    println!("{:?}", t);
    let mut equs = TypeEquations::new();
    t.gen_type(&mut equs).unwrap();
    println!("{:#?}", equs);
}
