//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::sequence::*;
use nom::bytes::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::branch::*;

use crate::identifier::*;
use crate::expression::{ Expression, parse_expression };
use crate::unary_expr::Variable;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::type_spec::*;

#[derive(Debug)]
pub enum VariableDeclaration {
    Leaf(Identifier, bool),
    Tuple(Vec<VariableDeclaration>, Tag),
}

impl GenType for VariableDeclaration {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match self {
            VariableDeclaration::Leaf(id, _is_mut) => {
                let alpha = id.generate_not_void_type_variable("LetType", 0, equs);
                equs.regist_variable(Variable::from_identifier(id.clone()), alpha.clone());
                Ok(alpha)
            }
            VariableDeclaration::Tuple(vars, _) => {
                let params = vars.iter().map(|v| v.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Tuple(params))
            }
        }
    }
}

impl MutCheck for VariableDeclaration {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        match self {
            VariableDeclaration::Leaf(id, is_mut) => {
                vars.regist_variable(id, *is_mut);
                if ta.annotation(id.get_tag_number(), "LetType", 0).is_reference() && *is_mut {
                    Err(format!("Reference cant be mutable, {:?}", id))
                }
                else {
                    Ok(MutResult::NoType)
                }
            }
            VariableDeclaration::Tuple(params, _) => {
                for v in params.iter() {
                    v.mut_check(ta, vars)?;
                }
                Ok(MutResult::NoType)
            }
        }
    }
}

impl VariableDeclaration {
    fn regist_vars(&self, mc: &mut VariablesMoveChecker) {
        match self {
            VariableDeclaration::Leaf(id, _is_mut) => {
                mc.regist_var(id);
            }
            VariableDeclaration::Tuple(params, _) => {
                for v in params.iter() {
                    v.regist_vars(mc);
                }
            }
        }
    }
    fn transpile_var(&self) -> String {
        match self {
            VariableDeclaration::Leaf(ref id, _) => id.into_string(),
            VariableDeclaration::Tuple(_, ref tag) => format!("__tuple{}", tag.get_num()),
        }
    }
    fn is_tuple(&self) -> bool {
        match self {
            VariableDeclaration::Tuple(_, _) => true,
            VariableDeclaration::Leaf(_, _) => false,
        }
    }
    fn transpile(&self, ta: &TypeAnnotation, right: String) -> String {
        match self {
            VariableDeclaration::Leaf(ref id, ref is_mut) => {
                format!("{}{} {} = {}",
                        ta.annotation(id.get_tag_number(), "LetType", 0).transpile(ta),
                        if *is_mut { "" } else { "" },
                        id.into_string(),
                        right,
                        )
            }
            VariableDeclaration::Tuple(ref params, _) => {
                let params_trans = params.iter().map(|p| p.transpile_var()).collect::<Vec<_>>().join(", ");
                let mut decl = format!("auto [{}] = {}", params_trans, right);
                for p in params.iter() {
                    if p.is_tuple() {
                        let s = p.transpile(ta, format!("std::move({})", p.transpile_var()));
                        decl.push_str(";\n");
                        decl.push_str(&s);
                    }
                }
                decl
            }
        }
    }
}

fn parse_declaration_tuple(s: &str) -> IResult<&str, VariableDeclaration> {
    let (s, (_, _, tuples, _, _, _, _)) = tuple((char('('), multispace0, separated_list1(tuple((multispace0, char(','), multispace0)), parse_variable_declaration), multispace0, opt(char(',')), multispace0, char(')')))(s)?;
    Ok((s, VariableDeclaration::Tuple(tuples, Tag::new())))
}

fn parse_declration_leaf(s: &str) -> IResult<&str, VariableDeclaration> {
    let (s, (is_mut, id)) = tuple((opt(tuple((tag("mut"), multispace1))), parse_identifier))(s)?;
    Ok((s, VariableDeclaration::Leaf(id, is_mut.is_some())))
}

pub fn parse_variable_declaration(s: &str) -> IResult<&str, VariableDeclaration> {
    alt((parse_declration_leaf, parse_declaration_tuple))(s)
}

#[derive(Debug)]
pub struct LetDeclaration {
    pub vars: VariableDeclaration,
    pub type_info: Option<TypeSpec>,
    pub value: Expression,
}


impl GenType for LetDeclaration {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let vars_type = self.vars.gen_type(equs, trs)?;
        let value_type = self.value.gen_type(equs, trs)?;
        equs.add_equation(vars_type.clone(), value_type);
        if let Some(ref t) = self.type_info {
            let t_type = t.generics_to_type(&GenericsTypeMap::empty(), equs, trs)?;
            equs.add_equation(vars_type.clone(), t_type);
        }
        Ok(Type::End)
    }
}

impl Transpile for LetDeclaration {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        self.vars.transpile(ta, self.value.transpile(ta))
    }
}

impl MutCheck for LetDeclaration {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        self.vars.mut_check(ta, vars)
    }
}

impl MoveCheck for LetDeclaration {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        let res = self.value.move_check(mc, ta)?;
        mc.move_result(res)?;
        self.vars.regist_vars(mc);
        Ok(MoveResult::Right)
    }
}


pub fn parse_let_declaration(s: &str) -> IResult<&str, LetDeclaration> {
    let (s, (_let, _, vars, _, tyinfo, _, _e, _, value)) = tuple((tag("let"), multispace1, parse_variable_declaration, multispace0, opt(tuple((char(':'), multispace0, parse_type_spec))), multispace0, tag("="), multispace0, parse_expression))(s)?;
    Ok((s, (LetDeclaration { vars, type_info: tyinfo.map(|(_, _, type_info)| type_info ), value })))
}

#[test]
fn parse_decl_test() {
    log::debug!("{:?}", parse_let_declaration("let x = 1 + 2").ok());
    log::debug!("{:?}", parse_let_declaration("let x: i64 = 1 + 2").ok());
}
