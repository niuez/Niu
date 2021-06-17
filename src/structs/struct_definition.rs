use std::collections::HashMap;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;
use nom::branch::*;

use crate::identifier::{ Identifier, parse_identifier };
use crate::type_id::*;
use crate::type_spec::*;
use crate::cpp_inline::*;
//use crate::unary_expr::Variable;
use crate::unify::*;

use crate::trans::*;

#[derive(Debug, Clone)]
pub struct MemberInfo {
    pub members_order: Vec<Identifier>,
    pub members: HashMap<Identifier, TypeSpec>,
}

#[derive(Debug, Clone)]
pub enum StructMember {
    MemberInfo(MemberInfo),
    CppInline(CppInline),
}

#[derive(Debug, Clone)]
pub struct StructDefinition {
    pub struct_id: TypeId,
    pub generics: Vec<TypeId>,
    pub member: StructMember,
}

impl StructDefinition {
    pub fn get_id(&self) -> TypeId {
        self.struct_id.clone()
    }
    pub fn get_generics_len(&self) -> usize {
        self.generics.len()
    }
    pub fn get_member_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo, gens: &Vec<Type>, id: &Identifier) -> TResult {
        match self.member {
            StructMember::MemberInfo(MemberInfo { ref members_order, ref members }) => {
                match members.get(id) {
                    Some(spec) => {
                        let mp = self.generics.iter().cloned().zip(gens.iter().cloned()).collect();
                        spec.generics_to_type(&GenericsTypeMap::empty().next(mp), equs, trs)
                    }
                    None => Err(format!("{:?} < {:?} >doesnt have member {:?}", self.struct_id, gens, id)),
                }
            }
            StructMember::CppInline(_) => {
                Err(format!("{:?} is inline struct", self.struct_id))
            }
        }
    }
}

fn parse_member(s: &str) -> IResult<&str, (Identifier, TypeSpec)> {
    let (s, (id, _, _, _, ty)) = tuple((parse_identifier, space0, char(':'), space0, parse_type_spec))(s)?;
    Ok((s, (id, ty)))
}

fn parse_generics_annotation(s: &str) -> IResult<&str, Vec<TypeId>> {
    let (s, op) = opt(tuple((char('<'), space0, parse_type_id, space0, many0(tuple((char(','), space0, parse_type_id, space0))), opt(tuple((space0, char(',')))), space0, char('>'))))(s)?;
    let v = match op {
        None => Vec::new(),
        Some((_, _, ty, _, m0, _, _, _)) => {
            let mut v = vec![ty];
            for (_, _, ty, _) in m0 {
                v.push(ty);
            }
            v
        }
    };
    Ok((s, v))
}

fn parse_struct_members(s: &str) -> IResult<&str, StructMember> {
    let (s, (_, _, opts, _)) = tuple((char('{'), space0,
                         opt(tuple((parse_member, many0(tuple((space0, char(','), space0, parse_member))), opt(tuple((space0, char(',')))), space0))),
                         char('}')))(s)?;
    let (members_order, members) = match opts {
        None => (Vec::new(), HashMap::new()),
        Some((mem, mems, _, _)) => {
            let mut vec = vec![mem];
            for (_, _, _, mem) in mems {
                vec.push(mem);
            }
            let members_order = vec.iter().map(|(id, _)| id.clone()).collect();
            (members_order, vec.into_iter().collect())
        }
    };
    Ok((s, StructMember::MemberInfo(MemberInfo { members_order, members })))
}

fn parse_struct_cpp_inline(s: &str) -> IResult<&str, StructMember> {
    let (s, cppinline) = parse_cpp_inline(s)?;
    Ok((s, StructMember::CppInline(cppinline)))
}

pub fn parse_struct_definition(s: &str) -> IResult<&str, StructDefinition> {
    let (s, (_, _, struct_id, _, generics, _, member)) =
        tuple((tag("struct"), space1, parse_type_id, space0, parse_generics_annotation, space0, alt((parse_struct_members, parse_struct_cpp_inline))))(s)?;
    Ok((s, StructDefinition { struct_id, generics, member }))
}

impl Transpile for StructDefinition {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match self.member {
            StructMember::MemberInfo(MemberInfo { ref members_order, ref members }) => {
                let template = if self.generics.len() > 0 {
                    format!("template <{}> ", self.generics.iter().map(|gen| format!("class {}", gen.transpile(ta))).collect::<Vec<_>>().join(", "))
                }
                else {
                    format!("")
                };
                let members_str = members_order.iter().map(|mem| members.get_key_value(mem).unwrap()).map(|(mem, ty)| format!("{} {};", ty.transpile(ta), mem.into_string())).collect::<Vec<_>>().join("\n");
                let constructor = format!("{}({}):{} {{ }}",
                self.struct_id.transpile(ta),
                members_order.iter().map(|mem| members.get_key_value(mem).unwrap())
                .map(|(mem, ty)| format!("{} {}", ty.transpile(ta), mem.into_string())).collect::<Vec<_>>().join(", "),
                members_order.iter().map(|mem| members.get_key_value(mem).unwrap())
                .map(|(mem, _)| format!("{}({})", mem.into_string(), mem.into_string())).collect::<Vec<_>>().join(", ")
                );

                format!("{}struct {} {{\n{}\n{}\n}} ;\n", template, self.struct_id.transpile(ta), members_str, constructor)
            }
            _ => format!(""),
        }
    }
}

#[test]
fn parse_struct_definition_test() {
    println!("{:?}", parse_struct_definition("struct MyStruct { a: i64, b: u64, }"));
}

#[test]
fn parse_struct_definition2_test() {
    println!("{:?}", parse_struct_definition("struct MyStruct<S, T> { a: S, b: T }"));
}

/*#[test]
fn get_member_type_test() {
    let def = StructDefinition {
        struct_id: parse_type_id("Hoge").unwrap().1,
        generics: vec![parse_type_id("S").unwrap().1, parse_type_id("T").unwrap().1],
        members_order: vec![Identifier::from_str("s"), Identifier::from_str("t")],
        members: vec![parse_member("s: S").unwrap().1, parse_member("t: T").unwrap().1].into_iter().collect()
    };
    let gens = vec![Type::Generics(TypeId::from_str("i64"), Vec::new()), Type::Generics(TypeId::from_str("u64"), Vec::new())];
    // let res = def.get_member_type(&mut TypeEquations::new(), &gens, &Identifier::from_str("s"));
    // println!("{:?}", res);
}*/
