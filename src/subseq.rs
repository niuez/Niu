use std::collections::HashMap;

use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::branch::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };
use crate::unary_expr::UnaryExpr;
use crate::traits::*;
use crate::unify::*;
use crate::type_spec::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::identifier::*;
use crate::error::*;

#[derive(Debug)]
pub enum Subseq {
    Call(Call),
    Member(Member),
    TupleMember(TupleMember),
    Index(IndexCall),
}

pub fn subseq_gen_type(uexpr: &UnaryExpr, subseq: &Subseq, range: &SourceRange, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
    match *subseq {
        Subseq::Call(ref call) => {
            match uexpr {
                UnaryExpr::Subseq(mem_caller, Subseq::Member(mem), _) => {
                    let caller = mem_caller.gen_type(equs, trs)?;
                    let args =
                            std::iter::once(Ok(Type::AutoRef(Box::new(caller.clone()), AutoRefTag::Tag(call.tag.clone()))))
                            //std::iter::once(Ok(caller.clone()))
                            .chain(call.args.iter().map(|arg| arg.gen_type(equs, trs))).collect::<Result<Vec<_>, _>>()?;
                    Ok(Type::CallEquation(CallEquation {
                        caller_type: None,
                        trait_gen: None,
                        func_id: mem.mem_id.clone(),
                        args,
                        caller_range: range.hint("call here", ErrorHint::None),
                        tag: call.tag.clone(),
                    }))
                }
                UnaryExpr::TraitMethod(spec, trait_op, func_id, _range) => {
                    let caller = spec.generics_to_type(&GenericsTypeMap::empty(), equs, trs)?;
                    let args = call.args.iter().map(|arg| arg.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
                    Ok(Type::CallEquation(CallEquation {
                        caller_type: Some(Box::new(caller)),
                        trait_gen: match trait_op {
                            Some(trait_spec) => Some(trait_spec.generate_trait_generics_with_no_map(equs, trs)?),
                            None => None,
                        },
                        func_id: func_id.clone(),
                        args,
                        caller_range: range.hint("call here", ErrorHint::None),
                        tag: call.tag.clone(),
                    }))
                }
                uexpr => {
                    let caller = uexpr.gen_type(equs, trs)?;
                    let args = call.args.iter().map(|arg| arg.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
                    /*
                    let return_type = call.tag.generate_type_variable("ReturnType", 0, equs);
                    let func_type = call.tag.generate_type_variable("FuncTypeInfo", 0, equs);
                    equs.add_equation(caller, func_type.clone());
                    equs.add_equation(func_type, Type::Func(args, Box::new(return_type.clone()), FuncTypeInfo::None));
                    Ok(return_type)
                    */
                    Ok(Type::CallVariable(CallVariable {
                        func_var: Box::new(caller),
                        args,
                        caller_range: range.hint("call here", ErrorHint::None),
                        tag: call.tag.clone()
                    }))
                }
            }
        }
        Subseq::Member(ref mem) => {
            let st = uexpr.gen_type(equs, trs)?;
            let st_type = mem.mem_id.generate_type_variable("StructType", 0, equs);
            equs.add_equation(st_type.clone(), st, ErrorComment::new(format!("type variable for member struct type"), range.merge(&mem.range).hint("member call here", ErrorHint::None).err()));
            let alpha = mem.mem_id.generate_type_variable("MemberType", 0, equs);
            let member_eq = Type::Member( MemberEquation {
                caller_type: Box::new(st_type.clone()),
                id: mem.mem_id.clone(),
                caller_range: range.merge(&mem.range).hint("member call here", ErrorHint::None),
            });
            equs.add_equation(alpha.clone(), member_eq.clone(), ErrorComment::new(format!("type variable for member type"), range.merge(&mem.range).hint("member call here", ErrorHint::None).err()));
            equs.regist_check_copyable(mem.mem_id.tag.clone(), alpha);
            Ok(member_eq)
        }
        Subseq::TupleMember(ref mem) => {
            let st = uexpr.gen_type(equs, trs)?;
            let st_type = mem.id.tag.generate_type_variable("StructType", 0, equs);
            equs.add_equation(st_type.clone(), st, ErrorComment::new(format!("type variable for tuple member struct type"), range.merge(&mem.range).hint("member call here", ErrorHint::None).err()));
            let alpha = mem.id.tag.generate_type_variable("MemberType", 0, equs);
            equs.add_equation(alpha.clone(), Type::TupleMember( TupleMemberEquation {
                ty: Box::new(st_type.clone()),
                idx: mem.idx,
                caller_range: range.merge(&mem.range).hint("tuple member call here", ErrorHint::None)
            }), ErrorComment::new(format!("type variable for tuple member type"), range.merge(&mem.range).hint("member call here", ErrorHint::None).err()));
            equs.regist_check_copyable(mem.id.tag.clone(), alpha.clone());
            Ok(alpha)
        }
        Subseq::Index(ref index) => {
            let caller = uexpr.gen_type(equs, trs)?;
            let caller_type = index.tag.generate_type_variable("IndexCallerType", 0, equs);
            equs.add_equation(caller.clone(), caller_type, ErrorComment::new(format!("type variable for index caller type"), range.merge(&index.range).hint("index call here", ErrorHint::None).err()));
            let arg0 = Type::AutoRef(Box::new(caller.clone()), AutoRefTag::Tag(index.tag.clone()));
            let arg1 = index.arg.as_ref().gen_type(equs, trs)?;
            let alpha = index.tag.generate_type_variable("IndexResult", 0, equs);
            equs.add_equation(alpha.clone(), Type::Deref(Box::new(
                        Type::CallEquation(CallEquation {
                            caller_type: None,
                            trait_gen: Some(TraitGenerics { trait_id: TraitId { id: Identifier::from_str("Index") }, generics: Vec::new() }),
                            func_id: Identifier::from_str("index"),
                            args: vec![arg0, arg1],
                            caller_range: range.merge(&index.range).hint("call here", ErrorHint::None),
                            tag: index.tag.clone(),
                        }
            ))), ErrorComment::new(format!("type variable for index result type"), range.merge(&index.range).hint("index call here", ErrorHint::None).err()));
            equs.regist_check_copyable(index.tag.clone(), alpha.clone());
            Ok(alpha)
        }
    }

}

pub fn subseq_transpile(uexpr: &UnaryExpr, subseq: &Subseq, ta: &TypeAnnotation) -> String {
    match *subseq {
        Subseq::Call(ref call) => {
            if let UnaryExpr::Subseq(mem_caller, Subseq::Member(mem), _) = uexpr {
                if mem.mem_id.into_string() == "clone" {
                    format!("{}", mem_caller.transpile(ta))
                }
                else {
                    let caller_trans = match ta.annotation(call.tag.get_num(), "AutoRefType", 0) {
                        Type::AutoRef(_, AutoRefTag::Nothing) => format!("{}", mem_caller.transpile(ta)),
                        Type::AutoRef(_, AutoRefTag::Ref) => format!("{}", mem_caller.transpile(ta)),
                        Type::AutoRef(_, AutoRefTag::MutRef) => format!("{}", mem_caller.transpile(ta)),
                        _ => unreachable!("it is not AutoRef"),
                    };
                    let ty = ta.annotation(call.tag.get_num(), "FuncTypeInfo", 0);
                    //if let Type::Func(_, _, Some((trait_id, ty))) = ty {
                    if let Type::Func(_, _, info) = ty {
                        match info {
                            FuncTypeInfo::TraitFunc(trait_id, generics_cnt, tag) => {
                                let args = call.args.iter().map(|arg| arg.transpile(ta));
                                let args = std::iter::once(caller_trans).chain(args).collect::<Vec<_>>().join(", ");
                                let ty = std::iter::once(ta.annotation(tag.get_num(), "SelfType", 0)).chain(
                                    (0..generics_cnt).map(|i| ta.annotation(tag.get_num(), "TraitGenerics", i)))
                                    .map(|t| t.transpile(ta)).collect::<Vec<_>>().join(", ");
                                format!("{}<{}>::{}({})", trait_id.transpile(ta), ty, mem.mem_id.into_string(), args)
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
            }
            else if let UnaryExpr::TraitMethod(_, _, method_id, _range) = uexpr {
                let ty = ta.annotation(call.tag.get_num(), "FuncTypeInfo", 0);
                //if let Type::Func(_, _, Some((trait_id, ty))) = ty {
                if let Type::Func(_, _, info) = ty {
                    match info {
                        FuncTypeInfo::TraitFunc(trait_id, generics_cnt, tag) => {
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = args.collect::<Vec<_>>().join(", ");
                            let ty = std::iter::once(ta.annotation(tag.get_num(), "SelfType", 0)).chain(
                                (0..generics_cnt).map(|i| ta.annotation(tag.get_num(), "TraitGenerics", i)))
                                .map(|t| t.transpile(ta)).collect::<Vec<_>>().join(", ");
                            format!("{}<{}>::{}({})", trait_id.transpile(ta), ty, method_id.into_string(), args)
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
                match ta.annotation(call.tag.get_num(), "FuncTypeInfo", 0) {
                    Type::Func(_, _, FuncTypeInfo::CppInline(cppinline, ids)) => {
                        let args = call.args.iter().map(|arg| arg.transpile(ta));
                        let mp = ids.into_iter().zip(args.into_iter()).collect::<HashMap<_, _>>();
                        cppinline.transpile(ta, &mp)
                    }
                    _ => {
                        let caller = uexpr.transpile(ta);
                        let args = call.args.iter().map(|arg| arg.transpile(ta)).collect::<Vec<_>>().join(", ");
                        format!("{}({})", caller, args)
                    }
                }
            }
        }
        Subseq::Member(ref mem) => {
            let caller = uexpr.transpile(ta);
            let trans = match ta.annotation(mem.mem_id.get_tag_number(), "StructType", 0) {
                Type::Ref(_) => format!("{}.{}", caller, mem.mem_id.into_string()),
                Type::MutRef(_) => format!("{}.{}", caller, mem.mem_id.into_string()),
                _ => format!("{}.{}", caller, mem.mem_id.into_string())
            };
            if ta.is_copyable(&mem.mem_id.tag) {
                trans
            }
            else if ta.is_moved(&mem.mem_id.tag) {
                format!("std::move({})", trans)
            }
            else {
                trans
            }
        }
        Subseq::TupleMember(ref mem) => {
            let caller = uexpr.transpile(ta);
            let trans = match ta.annotation(mem.id.tag.get_num(), "StructType", 0) {
                _ => format!("std::get<{}>({})", mem.idx, caller)
            };
            if ta.is_copyable(&mem.id.tag) {
                trans
            }
            else if ta.is_moved(&mem.id.tag) {
                format!("std::move({})", trans)
            }
            else {
                trans
            }
        }
        Subseq::Index(ref index) => { 
            let caller = uexpr.transpile(ta);
            let arg = index.arg.as_ref().transpile(ta);
            match ta.annotation(index.tag.get_num(), "IndexCallerType", 0) {
                Type::Ref(_) => format!("{}[{}]", caller, arg),
                Type::MutRef(_) => format!("{}[{}]", caller, arg),
                _ => format!("{}[{}]", caller, arg)
            }
        }
    }
}

pub fn subseq_mut_check(uexpr: &UnaryExpr, subseq: &Subseq, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
    match *subseq {
        Subseq::Call(ref call) => {
            match uexpr {
                UnaryExpr::Subseq(mem_caller, Subseq::Member(_mem), _) => {
                    for arg in call.args.iter() {
                        arg.mut_check(ta, vars)?;
                    }
                    match ta.annotation(call.tag.get_num(), "AutoRefType", 0) {
                        Type::AutoRef(_, AutoRefTag::MutRef) => {
                            if let MutResult::Mut = mem_caller.mut_check(ta, vars)? {
                                Ok(MutResult::NotMut)
                            }
                            else {
                                Err(format!("caller {:?} is needed mutable but not", mem_caller))
                            }
                        }
                        Type::AutoRef(_, AutoRefTag::Nothing) => Ok(MutResult::NotMut),
                        Type::AutoRef(_, AutoRefTag::Ref) => Ok(MutResult::NotMut),
                        _ => unreachable!("it is not AutoRef"),
                    }
                }
                UnaryExpr::TraitMethod(_spec, _trait_op, _func_id, _range) => {
                    for arg in call.args.iter() {
                        arg.mut_check(ta, vars)?;
                    }
                    Ok(MutResult::NotMut)
                }
                uexpr => {
                    uexpr.mut_check(ta, vars)?;
                    for arg in call.args.iter() {
                        arg.mut_check(ta, vars)?;
                    }
                    Ok(MutResult::NotMut)
                }
            }
        }
        Subseq::Member(ref mem) => {
            let uexpr = uexpr.mut_check(ta, vars)?;
            match (uexpr, ta.annotation(mem.mem_id.get_tag_number(), "StructType", 0)) {
                (MutResult::Mut, _) => Ok(MutResult::Mut),
                (_, Type::MutRef(_)) => Ok(MutResult::Mut),
                _ => Ok(MutResult::NotMut),
            }
        }
        Subseq::TupleMember(ref mem) => {
            let uexpr = uexpr.mut_check(ta, vars)?;
            match (uexpr, ta.annotation(mem.id.tag.get_num(), "StructType", 0)) {
                (MutResult::Mut, _) => Ok(MutResult::Mut),
                (_, Type::MutRef(_)) => Ok(MutResult::Mut),
                _ => Ok(MutResult::NotMut),
            }
        }
        Subseq::Index(ref index) => {
            let uexpr = uexpr.mut_check(ta, vars)?;
            let _arg = index.arg.as_ref().mut_check(ta, vars)?;
            Ok(uexpr)
        }
    }
}

pub fn subseq_move_check(uexpr: &UnaryExpr, subseq: &Subseq, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
    /*match *subseq {
        Subseq::Call(ref call) => {
            for arg in call.args.iter() {
                let res = arg.move_check(mc, trs)?;
            }
            Ok(MutResult::Right)
        }
        Subseq::Member(ref mem) => {
            let uexpr = uexpr.move_check(mc, trs)?;
            match (uexpr, ta.annotation(mem.mem_id.get_tag_number(), "StructType", 0)) {
                (MoveResult::Movable(id), _) => MoveResult::Movable(id),
                (MoveResult::Right, Type::Ref(_)) | (MoveResult::Right, Type::MutRef(_)) => MoveResult::Deref,
                (MoveResult::Right, _) => MoveResult::Right,
                (MoveResult::Deref, _) => MoveResult::Deref,
                (MoveResult::Dead(id), _) => Err(format!("{:?} {:?} is dead", uexpr, id))
            }
        }
        Subseq::Index(ref index) => {
            let uexpr = uexpr.mut_check(ta, vars)?;
            let _arg = index.arg.as_ref().mut_check(ta, vars)?;
            Ok(uexpr)
        }
    }*/
    match *subseq {
        Subseq::Call(ref call) => {
            match uexpr {
                UnaryExpr::Subseq(mem_caller, Subseq::Member(_mem), _) => {
                    let mem_res = mem_caller.move_check(mc, ta)?;
                    match ta.annotation(call.tag.get_num(), "AutoRefType", 0) {
                        Type::AutoRef(_, AutoRefTag::MutRef) | Type::AutoRef(_, AutoRefTag::Ref) => {}
                        Type::AutoRef(_, AutoRefTag::Nothing) => {
                            mc.move_result(mem_res)?;
                        }
                        _ => return Err(format!("it is not AutoRef"))
                    }
                    for arg in call.args.iter() {
                        let res = arg.move_check(mc, ta)?;
                        mc.move_result(res)?;
                    }
                    Ok(MoveResult::Right)
                }
                _uexpr => {
                    for arg in call.args.iter() {
                        let res = arg.move_check(mc, ta)?;
                        mc.move_result(res)?;
                    }
                    Ok(MoveResult::Right)
                }
            }
        }
        Subseq::Member(ref mem) => {
            let uexpr = uexpr.move_check(mc, ta)?;
            if ta.is_copyable(&mem.mem_id.tag) {
                Ok(MoveResult::Right)
            }
            else {
                match (uexpr, ta.annotation(mem.mem_id.get_tag_number(), "StructType", 0)) {
                    (_, Type::Ref(_)) | (MoveResult::Right, Type::MutRef(_)) => Ok(MoveResult::Deref),
                    (MoveResult::Deref, _) => Ok(MoveResult::Deref),
                    (uexpr, _) => Ok(MoveResult::Member(Box::new(uexpr), mem.mem_id.clone())),
                }
            }
        }
        Subseq::TupleMember(ref mem) => {
            let uexpr = uexpr.move_check(mc, ta)?;
            if ta.is_copyable(&mem.id.tag) {
                Ok(MoveResult::Right)
            }
            else {
                match (uexpr, ta.annotation(mem.id.get_tag_number(), "StructType", 0)) {
                    (_, Type::Ref(_)) | (MoveResult::Right, Type::MutRef(_)) => Ok(MoveResult::Deref),
                    (MoveResult::Deref, _) => Ok(MoveResult::Deref),
                    (uexpr, _) => Ok(MoveResult::Member(Box::new(uexpr), mem.id.clone())),
                }
            }
        }
        Subseq::Index(ref index) => {
            let _uexpr = uexpr.move_check(mc, ta)?;
            let arg = index.arg.as_ref().move_check(mc, ta)?;
            mc.move_result(arg)?;
            if ta.is_copyable(&index.tag) {
                Ok(MoveResult::Right)
            }
            else {
                Ok(MoveResult::Deref)
            }
        }
    }
}


pub fn parse_subseq(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, x)) = tuple((multispace0, alt((parse_call, parse_member, parse_tuple_member, parse_index_call))))(s)?;
    Ok((s, x))
}

#[derive(Debug)]
pub struct Call {
    pub args: Vec<Expression>,
    tag: Tag,
}

pub fn parse_call(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, _, op, _)) = tuple((
        char('('), multispace0, opt(tuple((
                    parse_expression, multispace0,
                    many0(tuple((char(','), multispace0, parse_expression, multispace0))), opt(char(',')), multispace0))),
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
pub struct IndexCall {
    arg: Box<Expression>,
    tag: Tag,
    range: SourceRange,
}

pub fn parse_index_call(s: &str) -> IResult<&str, Subseq> {
    let (s, ((_, _, arg, _, _), range)) = with_range(tuple((
            char('['), multispace0, parse_expression, multispace0, char(']')
            )))(s)?;
    Ok((s, Subseq::Index(IndexCall { arg: Box::new(arg), tag: Tag::new(), range })))
}

#[derive(Debug)]
pub struct Member {
    pub mem_id: Identifier,
    pub range: SourceRange,
}

fn parse_member(s: &str) -> IResult<&str, Subseq> {
    let (s, ((_, _, mem_id), range)) = with_range(tuple((char('.'), multispace0, parse_identifier)))(s)?;
    Ok((s, Subseq::Member(Member { mem_id, range })))
}

#[derive(Debug)]
pub struct TupleMember {
    pub idx: usize,
    pub id: Identifier,
    pub range: SourceRange,
}

fn parse_tuple_member(s: &str) -> IResult<&str, Subseq> {
    let (s, ((_, _, idx), range)) = with_range(tuple((char('.'), multispace0, nom::character::complete::u64)))(s)?;
    Ok((s, Subseq::TupleMember(TupleMember { idx: idx as usize, id: Identifier::from_str(&idx.to_string()), range, })))
}

#[test]
fn parse_call_test() {
    log::debug!("{:?}", parse_call("()").ok());
    log::debug!("{:?}", parse_call("(1, 2, 3)").ok());
}
