use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::branch::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };
use crate::unary_expr::UnaryExpr;
use crate::unify::*;
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
            let caller = uexpr.gen_type(equs, trs)?;
            let args = call.args.iter().map(|arg| arg.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
            let return_type = new_type_variable();
            equs.add_equation(caller, Type::Func(args, Box::new(return_type.clone()), FuncTypeInfo::None));
            Ok(return_type)
        }
        Subseq::Member(ref mem) => {
            let st = uexpr.gen_type(equs, trs)?;
            let alpha = mem.mem_id.generate_type_variable(1);
            equs.add_equation(alpha.clone(), Type::Member(Box::new(st.clone()), mem.mem_id.clone()));
            Ok(Type::Member(Box::new(st), mem.mem_id.clone()))
        }
    }

}

pub fn subseq_transpile(uexpr: &UnaryExpr, subseq: &Subseq, ta: &TypeAnnotation) -> String {
    match *subseq {
        Subseq::Call(ref call) => {
            if let UnaryExpr::Subseq(mem_caller, Subseq::Member(mem)) = uexpr {
                let ty = ta.annotation(mem.mem_id.get_tag_number(), 1);
                //if let Type::Func(_, _, Some((trait_id, ty))) = ty {
                if let Type::Func(_, _, info) = ty {
                    match info {
                        FuncTypeInfo::TraitFunc(trait_id, ty) => {
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = std::iter::once(mem_caller.transpile(ta)).chain(args).collect::<Vec<_>>().join(", ");
                            format!("{}<{}>::{}({})", trait_id.transpile(ta), (*ty).transpile(ta), mem.mem_id.into_string(), args)
                        }
                        FuncTypeInfo::SelfFunc(typeid, impl_generics) => {
                            let gen_args = impl_generics.into_iter().map(|(_, ty)| ty.transpile(ta)).collect::<Vec<_>>().join(", ");
                            let args = call.args.iter().map(|arg| arg.transpile(ta));
                            let args = std::iter::once(mem_caller.transpile(ta)).chain(args).collect::<Vec<_>>().join(", ");
                            format!("{}<{}>::{}({})", typeid.transpile(ta), gen_args, mem.mem_id.into_string(), args)
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
    Ok((s, (Subseq::Call(Call{ args }))))
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
