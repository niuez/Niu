use std::collections::HashMap;

use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::branch::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };
use crate::unary_expr::UnaryExpr;
use crate::unify::*;
use crate::type_spec::*;
use crate::trans::*;
use crate::identifier::*;

#[derive(Debug)]
pub enum Subseq {
    Call(Call),
    Member(Member),
}

pub fn subseq_gen_type(uexpr: &UnaryExpr, subseq: &Subseq, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
    match *subseq {
        Subseq::Call(ref call) => {
            match uexpr {
                UnaryExpr::Subseq(mem_caller, Subseq::Member(mem)) => {
                    let caller = mem_caller.gen_type(equs, trs)?;
                    let args =
                            std::iter::once(Ok(Type::AutoRef(Box::new(caller.clone()), AutoRefTag::Tag(call.tag.clone()))))
                            //std::iter::once(Ok(caller.clone()))
                            .chain(call.args.iter().map(|arg| arg.gen_type(equs, trs))).collect::<Result<Vec<_>, String>>()?;
                    Ok(Type::CallEquation(CallEquation {
                        caller_type: None,
                        trait_id: None,
                        func_id: mem.mem_id.clone(),
                        args,
                        tag: call.tag.clone(),
                    }))
                }
                UnaryExpr::TraitMethod(spec, trait_op, func_id) => {
                    let caller = spec.generics_to_type(&GenericsTypeMap::empty(), equs, trs)?;
                    let args = call.args.iter().map(|arg| arg.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
                    Ok(Type::CallEquation(CallEquation {
                        caller_type: Some(Box::new(caller)),
                        trait_id: trait_op.clone(),
                        func_id: func_id.clone(),
                        args,
                        tag: call.tag.clone(),
                    }))
                }
                uexpr => {
                    let caller = uexpr.gen_type(equs, trs)?;
                    let args = call.args.iter().map(|arg| arg.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
                    let return_type = call.tag.generate_type_variable("ReturnType", 0, equs);
                    equs.add_equation(caller, Type::Func(args, Box::new(return_type.clone()), FuncTypeInfo::None));
                    Ok(return_type)
                }
            }
        }
        Subseq::Member(ref mem) => {
            let st = uexpr.gen_type(equs, trs)?;
            let alpha = mem.mem_id.generate_type_variable("MemberType", 0, equs);
            equs.add_equation(alpha.clone(), Type::Member(Box::new(st.clone()), mem.mem_id.clone()));
            Ok(Type::Member(Box::new(st), mem.mem_id.clone()))
        }
    }

}

pub fn subseq_transpile(uexpr: &UnaryExpr, subseq: &Subseq, ta: &TypeAnnotation) -> String {
    match *subseq {
        Subseq::Call(ref call) => {
            if let UnaryExpr::Subseq(mem_caller, Subseq::Member(mem)) = uexpr {
                let caller_trans = match ta.annotation(call.tag.get_num(), "AutoRefType", 0) {
                    Type::AutoRef(_, AutoRefTag::Nothing) => format!("{}", mem_caller.transpile(ta)),
                    Type::AutoRef(_, AutoRefTag::Ref) => format!("(&{})", mem_caller.transpile(ta)),
                    Type::AutoRef(_, AutoRefTag::MutRef) => format!("(&{})", mem_caller.transpile(ta)),
                    _ => unreachable!("it is not AutoRef"),
                };
                let ty = ta.annotation(call.tag.get_num(), "FuncTypeInfo", 0);
                //if let Type::Func(_, _, Some((trait_id, ty))) = ty {
                if let Type::Func(_, _, info) = ty {
                    match info {
                        FuncTypeInfo::TraitFunc(trait_id, tag) => {
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = std::iter::once(caller_trans).chain(args).collect::<Vec<_>>().join(", ");
                            let ty = ta.annotation(tag.get_num(), "SelfType", 0);
                            format!("{}<{}>::{}({})", trait_id.transpile(ta), ty.transpile(ta), mem.mem_id.into_string(), args)
                        }
                        FuncTypeInfo::SelfFunc(tag) => {
                            let ty = ta.annotation(tag.get_num(), "SelfType", 0).transpile(ta);
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = std::iter::once(caller_trans).chain(args).collect::<Vec<_>>().join(", ");
                            format!("{}::{}({})", ty, mem.mem_id.into_string(), args)
                        }
                        FuncTypeInfo::CppInline(cppinline, ids) => {
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = std::iter::once(caller_trans).chain(args);
                            let mp = ids.into_iter().zip(args.into_iter()).collect::<HashMap<_, _>>();
                            cppinline.transpile(ta, &mp)
                        }
                        FuncTypeInfo::None => {
                            unimplemented!("Func type member?")
                        }
                    }
                }
                else {
                    unreachable!(format!("Member Call\nuexpr = {:?}\nsubseq = {:?}\nty = {:?}", uexpr, subseq, ty))
                }
            }
            else if let UnaryExpr::TraitMethod(_, _, method_id) = uexpr {
                let ty = ta.annotation(call.tag.get_num(), "FuncTypeInfo", 0);
                //if let Type::Func(_, _, Some((trait_id, ty))) = ty {
                if let Type::Func(_, _, info) = ty {
                    match info {
                        FuncTypeInfo::TraitFunc(trait_id, tag) => {
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = args.collect::<Vec<_>>().join(", ");
                            let ty = ta.annotation(tag.get_num(), "SelfType", 0);
                            format!("{}<{}>::{}({})", trait_id.transpile(ta), ty.transpile(ta), method_id.into_string(), args)
                        }
                        FuncTypeInfo::SelfFunc(tag) => {
                            let ty = ta.annotation(tag.get_num(), "SelfType", 0).transpile(ta);
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = args.collect::<Vec<_>>().join(", ");
                            format!("{}::{}({})", ty, method_id.into_string(), args)
                        }
                        FuncTypeInfo::CppInline(cppinline, ids) => {
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let mp = ids.into_iter().zip(args.into_iter()).collect::<HashMap<_, _>>();
                            cppinline.transpile(ta, &mp)
                        }
                        FuncTypeInfo::None => {
                            unimplemented!("Func type member?")
                        }
                    }
                }
                else {
                    unreachable!(format!("Member Call\nuexpr = {:?}\nsubseq = {:?}\nty = {:?}", uexpr, subseq, ty))
                }
            }
            else {
                let caller = uexpr.transpile(ta);
                let args = call.args.iter().map(|arg| arg.transpile(ta)).collect::<Vec<_>>().join(", ");
                format!("{}({})", caller, args)
            }
        }
        Subseq::Member(ref mem) => {
            let caller = uexpr.transpile(ta);
            format!("{}.{}", caller, mem.mem_id.into_string())
        }
    }
}

pub fn parse_subseq(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, x)) = tuple((space0, alt((parse_call, parse_member))))(s)?;
    Ok((s, x))
}

#[derive(Debug)]
pub struct Call {
    pub args: Vec<Expression>,
    tag: Tag,
}

pub fn parse_call(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, _, op, _)) = tuple((
        char('('), space0, opt(tuple((
                    parse_expression, space0,
                    many0(tuple((char(','), space0, parse_expression, space0))), opt(char(',')), space0))),
                    char(')')))(s)?;
    let args = match op {
        Some((arg0, _, many, _, _)) => {
            let mut args = vec![arg0];
            for (_, _, arg, _) in many {
                args.push(arg);
            }
            args
        }
        None => Vec::new(),
    };
    Ok((s, (Subseq::Call(Call{ args, tag: Tag::new(), }))))
}

#[derive(Debug)]
pub struct Member {
    pub mem_id: Identifier,
}

fn parse_member(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, _, mem_id)) = tuple((char('.'), space0, parse_identifier))(s)?;
    Ok((s, Subseq::Member(Member { mem_id })))
}

#[test]
fn parse_call_test() {
    println!("{:?}", parse_call("()"));
    println!("{:?}", parse_call("(1, 2, 3)"));
}
