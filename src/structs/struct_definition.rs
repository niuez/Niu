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
use crate::unify::*;

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub struct_id: TypeId,
    pub generics: Vec<TypeId>,
    pub members: HashMap<Identifier, TypeSpec>,
}

impl StructDefinition {
    pub fn get_id(&self) -> TypeId {
        self.struct_id.clone()
    }
    pub fn get_generics_len(&self) -> usize {
        self.generics.len()
    }
    pub fn get_member_type(&self, equs: &mut TypeEquations, gens: &Vec<Type>, id: &Identifier) -> TResult {
        match self.members.get(id) {
            Some(spec) => {
                let mp = self.generics.iter().cloned().zip(gens.iter().cloned()).collect();
                spec.generics_to_type(&mp, equs)
            }
            None => Err(format!("{:?} doesnt have member {:?}", self.struct_id, id)),
        }
    }
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
    Ok((s, StructDefinition { struct_id, generics: Vec::new(), members }))
}



#[test]
fn parse_struct_definition_test() {
    println!("{:?}", parse_struct_definition("struct MyStruct { a: i64, b: u64, }"));
}

#[test]
fn get_member_type_test() {
    let def = StructDefinition {
        struct_id: parse_type_id("Hoge").unwrap().1,
        generics: vec![parse_type_id("S").unwrap().1, parse_type_id("T").unwrap().1],
        members: vec![parse_member("s: S").unwrap().1, parse_member("t: T").unwrap().1].into_iter().collect()
    };
    let gens = vec![Type::Generics(TypeId::from_str("i64"), Vec::new()), Type::Generics(TypeId::from_str("u64"), Vec::new())];
    let res = def.get_member_type(&mut TypeEquations::new(), &gens, &Identifier::from_str("s"));
    println!("{:?}", res);
}
