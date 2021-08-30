use std::collections::HashMap;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::combinator::*;
use nom::branch::*;

use crate::identifier::{ Identifier, parse_identifier, Tag };
use crate::type_id::*;
use crate::type_spec::*;
use crate::cpp_inline::*;
use crate::traits::*;
use crate::structs::*;
use crate::func_definition::*;
//use crate::unary_expr::Variable;
use crate::unify::*;

use crate::trans::*;
use crate::mut_checker::*;

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
pub struct StructMemberDefinition {
    pub struct_id: TypeId,
    pub generics: Vec<TypeId>,
    pub member: StructMember,
    pub where_sec: WhereSection,
}

#[derive(Debug)]
pub struct StructDefinition {
    pub member_def: StructMemberDefinition,
    pub impl_self: ImplSelfDefinition,
}


impl StructDefinition {
    pub fn get_id(&self) -> TypeId {
        self.member_def.get_id()
    }
    pub fn get_member_def(&self) -> &StructMemberDefinition {
        &self.member_def
    }
    pub fn unify_require_methods(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> Result<(), String> {
        self.impl_self.unify_require_methods(equs, trs)
    }
    pub fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<(), String> {
        self.impl_self.mut_check(ta, vars)
    }

    pub fn get_impl_self_def(&self) -> &ImplSelfDefinition {
        &self.impl_self
    }
    pub fn transpile_self_type(&self) -> String {
        let generics = if self.member_def.generics.is_empty() {
            format!("")
        }
        else {
            format!("<{}>", self.member_def.generics.iter().map(|g| g.id.into_string()).collect::<Vec<_>>().join(", "))
        };
        format!("{}{}", self.member_def.struct_id.id.into_string(), generics)
    }
    pub fn transpile_definition(&self, ta: &TypeAnnotation) -> String {
        match self.member_def.member {
            StructMember::MemberInfo(MemberInfo { .. }) => {
                let generics = self.member_def.generics.iter().map(|gen| format!("class {}", gen.transpile(ta)))
                    .chain(if self.member_def.where_sec.is_empty() { None } else { Some(format!("class = void"))})
                    .collect::<Vec<_>>();
                let template = if generics.len() > 0 {
                    format!("template <{}> ", generics.join(", "))
                }
                else {
                    format!("")
                };
                format!("{}struct {};\n", template, self.member_def.struct_id.transpile(ta))
            }
            _ => format!(""),
        }
    }
    pub fn transpile(&self, ta: &TypeAnnotation, opes: Vec<String>) -> String {
        let binary_operators = BINARY_OPERATOR_TRAITS.iter().cloned().collect::<HashMap<_, _>>();
        match self.member_def.member {
            StructMember::MemberInfo(MemberInfo { ref members_order, ref members }) => {
                let template = if self.member_def.generics.len() > 0 {
                    format!("template <{}> ", self.member_def.generics.iter().map(|gen| format!("class {}", gen.transpile(ta))).collect::<Vec<_>>().join(", "))
                }
                else {
                    format!("")
                };
                let generics = self.member_def.generics.iter().map(|gen| format!("{}", gen.transpile(ta)))
                    .chain(if self.member_def.where_sec.is_empty() {
                        None
                    } else {
                        Some(self.member_def.where_sec.transpile(ta))
                    }).collect::<Vec<_>>();
                let impl_type = if generics.len() > 0 {
                    format!("{}<{}>", self.member_def.struct_id.transpile(ta),generics.join(", "))
                }
                else {
                    format!("{}", self.member_def.struct_id.transpile(ta))
                };
                let self_type_generics = if self.member_def.generics.len() > 0 {
                    format!("<{}>", self.member_def.generics.iter().map(|gen| format!("{}", gen.transpile(ta))).collect::<Vec<_>>().join(", "))
                }
                else {
                    format!("")
                };
                let self_type = format!("{}{}", self.member_def.struct_id.transpile(ta), self_type_generics);
                let members_str = members_order.iter().map(|mem| members.get_key_value(mem).unwrap()).map(|(mem, ty)| format!("{} {};", ty.transpile(ta), mem.into_string())).collect::<Vec<_>>().join("\n");
                let constructor_member_init = 
                    members_order.iter().map(|mem| members.get_key_value(mem).unwrap())
                        .map(|(mem, _)| format!("{}({})", mem.into_string(), mem.into_string())).collect::<Vec<_>>().join(", ");
                let constructor_member_init = if constructor_member_init.is_empty() {
                    format!("")
                }
                else {
                    format!(":{}", constructor_member_init)
                };
                let constructor = format!("{}({}){} {{ }}",
                    self.member_def.struct_id.transpile(ta),
                    members_order.iter().map(|mem| members.get_key_value(mem).unwrap())
                        .map(|(mem, ty)| format!("{} {}", ty.transpile(ta), mem.into_string())).collect::<Vec<_>>().join(", "),
                    constructor_member_init
                );
                let methods = self.impl_self.require_methods.iter().map(|(_, func)| format!("{}", func.transpile(ta, true))).collect::<Vec<_>>().join("\n");
                let operators = opes.into_iter().map(|ope| match ope.as_str() {
                    "Index" => {
                        format!("typename std::enable_if<Index<{0}>::value, const typename Index<{0}>::Output&>::type operator[](typename Index<{0}>::Arg k) const {{ return *Index<{0}>::index(this, k); }}\n", self_type)
                    }
                    "IndexMut" => {
                        format!("typename std::enable_if<IndexMut<{0}>::value, typename Index<{0}>::Output&>::type operator[](typename Index<{0}>::Arg k) {{ return *IndexMut<{0}>::index_mut(this, k); }}\n", self_type)
                    }
                    /* bin_ope if binary_operators.contains_key(bin_ope) => {
                        let method = binary_operators[&bin_ope];
                        format!("template<class Arg> typename std::enable_if<{0}<Self, Arg>::value, typename {0}<Self, Arg>::Output>::type operator{2}(Arg k) {{ return {0}<Self, Arg>::{1}(*this, k); }}\n", bin_ope, method.0, method.1)
                    } */
                    _ => "".to_string(),
                }).collect::<Vec<_>>().join("");

                format!("{}struct {} {{\n{}\n{}\n{}{}}} ;\n", template, impl_type, members_str, constructor, methods, operators)
            }
            _ => format!(""),
        }
    }
}

impl StructMemberDefinition {
    pub fn get_id(&self) -> TypeId {
        self.struct_id.clone()
    }
    pub fn get_generics_len(&self) -> usize {
        self.generics.len()
    }
    pub fn get_member_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo, gens: &Vec<Type>, id: &Identifier) -> TResult {
        match self.member {
            StructMember::MemberInfo(MemberInfo { ref members, .. }) => {
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
    let (s, (id, _, _, _, ty)) = tuple((parse_identifier, multispace0, char(':'), multispace0, parse_type_spec))(s)?;
    Ok((s, (id, ty)))
}

fn parse_generics_annotation(s: &str) -> IResult<&str, Vec<TypeId>> {
    let (s, op) = opt(tuple((char('<'), multispace0, parse_type_id, multispace0, many0(tuple((char(','), multispace0, parse_type_id, multispace0))), opt(tuple((multispace0, char(',')))), multispace0, char('>'))))(s)?;
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
    let (s, (_, _, opts, _)) = tuple((char('{'), multispace0,
                         opt(tuple((parse_member, many0(tuple((multispace0, char(','), multispace0, parse_member))), opt(tuple((multispace0, char(',')))), multispace0))),
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

pub fn parse_struct_member_definition(s: &str) -> IResult<&str, StructMemberDefinition> {
    let (s, (_, _, struct_id, _, generics, _, where_sec, _, member)) =
        tuple((tag("struct"), space1, parse_type_id, multispace0, parse_generics_annotation, multispace0, parse_where_section, multispace0, alt((parse_struct_members, parse_struct_cpp_inline))))(s)?;
    Ok((s, StructMemberDefinition { struct_id, generics, member, where_sec }))
}

pub fn parse_struct_definition(s: &str) -> IResult<&str, StructDefinition> {
    let (s, (member_def, _, _, _, funcs, _)) = tuple((parse_struct_member_definition, multispace0, char('{'), multispace0,
            many0(tuple((parse_func_definition, multispace0))), char('}')))(s)?;
    let require_methods = funcs.into_iter().map(|(func, _)| (func.func_id.clone(), func)).collect();
    let impl_self = ImplSelfDefinition {
        generics: member_def.generics.clone(),
        impl_ty: TypeSpec::TypeSign(TypeSign {
            id: member_def.struct_id.clone(),
            gens: member_def.generics.iter().map(|id| TypeSpec::from_id(id)).collect(),
        }),
        where_sec: member_def.where_sec.clone(),
        require_methods,
        tag: Tag::new(),
    };
    Ok((s, StructDefinition {
        member_def,
        impl_self,
    }))
}

#[test]
fn parse_struct_definition_test() {
    log::debug!("{:?}", parse_struct_definition("struct MyStruct { a: i64, b: u64, }").ok());
}

#[test]
fn parse_struct_definition2_test() {
    log::debug!("{:?}", parse_struct_definition("struct MyStruct<S, T> { a: S, b: T }").ok());
}

/*#[test]
fn get_member_type_test() {
    let def = StructMemberDefinition {
        struct_id: parse_type_id("Hoge").unwrap().1,
        generics: vec![parse_type_id("S").unwrap().1, parse_type_id("T").unwrap().1],
        members_order: vec![Identifier::from_str("s"), Identifier::from_str("t")],
        members: vec![parse_member("s: S").unwrap().1, parse_member("t: T").unwrap().1].into_iter().collect()
    };
    let gens = vec![Type::Generics(TypeId::from_str("i64"), Vec::new()), Type::Generics(TypeId::from_str("u64"), Vec::new())];
    // let res = def.get_member_type(&mut TypeEquations::new(), &gens, &Identifier::from_str("s"));
    // log::debug!("{:?}", res);
}*/
