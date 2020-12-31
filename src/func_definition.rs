use std::collections::HashMap;

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
use crate::unary_expr::Variable;

#[derive(Debug)]
pub struct FuncDefinition {
    pub func_id: Identifier,
    pub generics: Vec<TypeId>,
    pub args: Vec<(Identifier, TypeId)>,
    pub return_type: TypeId,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct FuncDefinitionInfo {
    pub generics: Vec<TypeId>,
    pub args: Vec<(Identifier, TypeId)>,
    pub return_type: TypeId,
}

impl FuncDefinitionInfo {
    pub fn generate_type(&self, equs: &mut TypeEquations) -> TResult {
        let mut mp = HashMap::new();
        for gt in self.generics.iter() {
            mp.insert(gt.clone(), equs.get_type_variable());
        }
        let mut generics_to_type = |t: &TypeId| {
            match mp.get(t).cloned() {
                Some(t) => Ok(t),
                None => t.gen_type(equs)
            }
        };
        let args = self.args.iter().map(|(_, t)| generics_to_type(t)).collect::<Result<Vec<Type>, String>>()?;
        let return_type = generics_to_type(&self.return_type)?;
        Ok(Type::Func(args, Box::new(return_type)))
    }
}

impl FuncDefinition {
    pub fn get_func_info(&self) -> (Variable, FuncDefinitionInfo) {
        (Variable { name: self.func_id.clone() },
         FuncDefinitionInfo { generics: self.generics.clone(), args: self.args.clone(), return_type: self.return_type.clone() }
         )
    }
}

impl GenType for FuncDefinition {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult {
        equs.into_scope();

        for (i, t) in self.args.iter() {
            let alpha = equs.get_type_variable();
            let t_type = t.gen_type(equs)?; 
            equs.regist_variable(Variable::from_identifier(i.clone()), alpha.clone());
            equs.add_equation(alpha, t_type);
        }
        let result_type = self.block.gen_type(equs)?;
        let return_t = self.return_type.gen_type(equs)?;
        equs.add_equation(result_type, return_t);

        equs.out_scope();
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
