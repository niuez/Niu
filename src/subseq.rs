use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::expression::{ Expression, parse_expression };
use crate::unary_expr::UnaryExpr;
use crate::unify::*;

#[derive(Debug)]
pub enum Subseq {
    Call(Call),
}

pub fn subseq_gen_type(uexpr: &UnaryExpr, subseq: &Subseq, equs: &mut TypeEquations) -> TResult {
    match *subseq {
        Subseq::Call(ref call) => {
            let caller = uexpr.gen_type(equs)?;
            let args = call.args.iter().map(|arg| arg.gen_type(equs)).collect::<Result<Vec<_>, String>>()?;
            let return_type = equs.get_type_variable();
            equs.add_equation(caller, Type::Func(args, Box::new(return_type.clone())));
            Ok(return_type)
            /* if let Type::Func(ref def_result, ref def_args) = uexpr_type {
                if def_args.len() == call_args.len() {
                    def_args.iter().zip(call_args.iter().map(|arg| arg.gen_type(equs)).collect::<Result<Vec<_>, String>>()?.into_iter())
                                   .for_each(|(d, c)| equs.add_equation(d.clone(), c));
                    Ok(def_result.as_ref().clone())
                }
                else {
                    Err("length of args is not match".to_string())
                }
            }
            else {
                Err("caller is not function".to_string())
            } */
        }
    }

}

#[derive(Debug)]
pub struct Call {
    pub args: Vec<Expression>,
}

pub fn parse_subseq(s: &str) -> IResult<&str, Subseq> {
    let (s, (_, x)) = tuple((space0, parse_call))(s)?;
    Ok((s, x))
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

#[test]
fn parse_call_test() {
    println!("{:?}", parse_call("()"));
    println!("{:?}", parse_call("(1, 2, 3)"));
}
