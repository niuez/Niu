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
use crate::trans::*;
use crate::type_spec::*;
use crate::traits::*;

#[derive(Debug)]
pub struct FuncDefinition {
    pub func_id: Identifier,
    pub generics: Vec<(TypeId, Option<TraitId>)>,
    pub args: Vec<(Identifier, TypeSpec)>,
    pub return_type: TypeSpec,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct FuncDefinitionInfo {
    pub generics: Vec<(TypeId, Option<TraitId>)>,
    pub args: Vec<(Identifier, TypeSpec)>,
    pub return_type: TypeSpec,
}

impl FuncDefinitionInfo {
    pub fn generate_type(&self, equs: &mut TypeEquations) -> TResult {
        let mut mp = HashMap::new();
        for gt in self.generics.iter() {
            let (id, tr) = gt.clone();
            let ty_var = equs.get_type_variable();
            mp.insert(id, ty_var.clone());
            if let Some(tr) = tr {
                equs.add_has_trait(ty_var, tr);
            }
        }
        let args = self.args.iter().map(|(_, t)| t.generics_to_type(&mp, equs)).collect::<Result<Vec<Type>, String>>()?;
        let return_type = self.return_type.generics_to_type(&mp, equs)?;
        Ok(Type::Func(args, Box::new(return_type)))
    }

    pub fn get_generics_annotation(&self, ta: &mut TypeAnnotation) -> String {
        if self.generics.len() > 0 {
            let gen = self.generics.iter().map(|_| { let res = ta.annotation(); ta.count(); res.transpile(ta) })
                          .collect::<Vec<_>>().join(", ");
            format!("<{}>", gen)
        }
        else {
            "".to_string()
        }
    }
}

impl FuncDefinition {
    pub fn get_func_info(&self) -> (Variable, FuncDefinitionInfo) {
        (Variable { name: self.func_id.clone() },
         FuncDefinitionInfo { generics: self.generics.clone(), args: self.args.clone(), return_type: self.return_type.clone() }
         )
    }
    pub fn unify_definition(&self, equs: &mut TypeEquations, trs: &mut TraitsInfo) -> Result<Vec<TypeSubst>, String> {
        equs.into_scope();
        trs.into_scope();

        for (ty_id, trait_id) in self.generics.iter() {
            if let Some(trait_id) = trait_id {
                trs.regist_param_candidate(equs, &TypeSpec::TypeId(ty_id.clone()), trait_id)?;
            }
        }

        println!("trs = {:?}", trs);

        for (i, t) in self.args.iter() {
            let alpha = equs.get_type_variable();
            let t_type = t.gen_type(equs)?; 
            equs.regist_variable(Variable::from_identifier(i.clone()), alpha.clone());
            equs.add_equation(alpha, t_type);
        }
        let result_type = self.block.gen_type(equs)?;
        let return_t = self.return_type.gen_type(equs)?;
        equs.add_equation(result_type, return_t);

        let result = equs.unify(trs);
        
        trs.out_scope();
        equs.out_scope();
        result
    }
}


impl Transpile for FuncDefinition {
    fn transpile(&self, ta: &mut TypeAnnotation) -> String {
        let template_str =
            if self.generics.len() > 0 {
                let gen = self.generics.iter().map(|g| format!("class {}", g.0.transpile(ta))).collect::<Vec<_>>().join(", ");
                format!("template<{}> ", gen)
            } 
            else {
                "".to_string()
            };

        let return_str = self.return_type.transpile(ta);
        let func_str = self.func_id.into_string();
        let arg_str = self.args.iter().map(|(id, ty)| {
            ta.count();
            format!("{} {}", ty.transpile(ta), id.into_string())
        }).collect::<Vec<_>>().join(", ");

        let block_str = self.block.transpile(ta);

        format!("{}{} {}({}) {{\n{}\n}}\n", template_str, return_str, func_str, arg_str, block_str)
    }
}

fn parse_generics_arg(s: &str) -> IResult<&str, (TypeId, Option<TraitId>)> {
    let (s, (id, _, opt)) = tuple((parse_type_id, space0, opt(tuple((char(':'), space0, parse_trait_id)))))(s)?;
    Ok((s, (id, opt.map(|(_, _, tr)| tr))))
}

pub fn parse_func_definition(s: &str) -> IResult<&str, FuncDefinition> {
    let (s, (_, _, func_id, _, generics_opt, _, _, op, _, _, _, _, return_type, _, _, block, _)) = 
        tuple((tag("fn"), space1, parse_identifier, space0, opt(tuple((char('<'), space0, opt(tuple((parse_generics_arg, space0, many0(tuple((char(','), space0, parse_generics_arg, space0))), opt(char(',')), space0))), char('>'), space0))),
               char('('), space0,
            opt(tuple((parse_identifier, space0, char(':'), space0, parse_type_spec, space0, many0(tuple((char(','), space0, parse_identifier, space0, char(':'), space0, parse_type_spec, space0))), opt(char(',')), space0))),
            char(')'), space0, tag("->"), space0, parse_type_spec, space0, char('{'), parse_block, char('}')))(s)?;
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
    println!("{:?}", parse_func_definition("fn func2<t>(x: t) -> t { x }"));
    println!("{:?}", parse_func_definition("fn func3<x, y, z>(x: x) -> z { x }"));
}
#[test]
fn parse_func_definition2_test() {
    println!("{:?}", parse_func_definition("fn func2<t: MyTrait>(x: t) -> t { x }"));
}
