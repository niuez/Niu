use std::collections::HashMap;

use nom::bytes::complete::*;
use nom::branch::*;
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
use crate::mut_checker::*;
use crate::type_spec::*;
use crate::cpp_inline::*;
use crate::move_checker::*;
use crate::error::*;
use crate::content_str::*;


#[derive(Debug)]
pub enum FuncBlock {
    Block(Block),
    CppInline(CppInline),
}

#[derive(Debug)]
pub struct FuncDefinition {
    pub func_id: Identifier,
    pub generics: Vec<TypeId>,
    pub where_sec: WhereSection,
    pub args: Vec<(Identifier, TypeSpec, bool)>,
    pub return_type: TypeSpec,
    pub block: FuncBlock,
    def_range: SourceRange,
}

#[derive(Debug, Clone)]
pub struct FuncDefinitionInfo {
    pub func_id: Identifier,
    pub generics: Vec<TypeId>,
    pub where_sec: WhereSection,
    pub args: Vec<(Identifier, TypeSpec, bool)>,
    pub return_type: TypeSpec,
    pub inline: Option<CppInline>,
    range: SourceRange,
}

impl FuncDefinitionInfo {
    pub fn hint(&self, define_hint: &ErrorHint) -> ErrorHint {
        self.range.hint("function define", define_hint.clone())
    }
    pub fn generate_type(&self, before_mp: &GenericsTypeMap, equs: &mut TypeEquations, trs: &TraitsInfo, call_id: &Identifier, define_hint: &ErrorHint) -> TResult {
        let mut gen_mp = HashMap::new();
        for (i, g_id) in self.generics.iter().enumerate() {
            let ty_var = call_id.generate_type_variable("Generics", i, equs);
            gen_mp.insert(g_id.clone(), ty_var.clone());
        }
        let mp = before_mp.next(gen_mp);
        self.where_sec.regist_equations(&mp, equs, trs, &self.hint(define_hint))?;
        let args = self.args.iter().map(|(_, t, _)| t.generics_to_type(&mp, equs, trs)).collect::<Result<Vec<Type>, _>>()?;
        let return_type = self.return_type.generics_to_type(&mp, equs, trs)?;

        let type_info = match self.inline {
            None => FuncTypeInfo::None,
            Some(ref inline) => {
                let args = self.args.iter().map(|(id, _, _)| id.clone()).collect();
                FuncTypeInfo::CppInline(inline.generate_cpp_inline_info(equs, trs, &mp)?, args)
            }
        };
        Ok(Type::Func(args, Box::new(return_type), type_info))
    }

    pub fn check_equal(&self, right: &Self, equs: &mut TypeEquations, trs: &TraitsInfo, self_gen_map: &GenericsTypeMap, right_gen_map: &GenericsTypeMap) -> Result<(), Error> {
        if self.generics != right.generics {
            Err(ErrorComment::empty(format!("generics of method {:?} is not matched", self.func_id)))?;
        }
        if !self.where_sec.check_equal(&right.where_sec) {
            Err(ErrorComment::empty(format!("where_section of method {:?} is not matched", self.func_id)))?;
        }
        let mut trs = trs.into_scope();
        for g_id in self.generics.iter() {
            trs.regist_generics_type(g_id)?;
        }
        let self_args  =  self.args.iter().map(|(_, t, _)| t.generics_to_type(self_gen_map, equs, &trs)).collect::<Result<Vec<Type>, _>>()?;
        let right_args = right.args.iter().map(|(_, t, _)| t.generics_to_type(right_gen_map, equs, &trs)).collect::<Result<Vec<Type>, _>>()?;
        let self_return_type = self.return_type.generics_to_type(self_gen_map, equs, &trs)?;
        let right_return_type = right.return_type.generics_to_type(right_gen_map, equs, &trs)?;
        equs.add_equation(
            Type::Func(self_args, Box::new(self_return_type), FuncTypeInfo::None),
            Type::Func(right_args, Box::new(right_return_type), FuncTypeInfo::None),
            ErrorComment::new(format!("check equal function definitions"), self.range.hint("left definition", right.range.hint("right definition", ErrorHint::None)).err())
            );
        log::info!("function {:?} and {:?} are equal unify", self.func_id, right.func_id);
        equs.unify(&mut trs).map_err(|e| e.into_err())
    }

    pub fn get_generics_annotation(&self, ta: &TypeAnnotation, call_id: &Identifier) -> String {
        if self.generics.len() > 0 {
            let gen = self.generics.iter().enumerate().map(|(i, _)| { let res = ta.annotation(call_id.get_tag_number(), "Generics", i); res.transpile(ta) })
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

        let inline = if let FuncBlock::CppInline(ref inline) = self.block {
            Some(inline.clone())
        }
        else {
            None
        };
        (Variable { id: self.func_id.clone() },
         FuncDefinitionInfo {
             func_id: self.func_id.clone(),
             generics: self.generics.clone(),
             where_sec: self.where_sec.clone(),
             args: self.args.clone(),
             return_type: self.return_type.clone(),
             inline,
             range: self.def_range.clone(),
         }
         )
    }
    pub fn unify_definition(&self, equs: &mut TypeEquations, trs: &TraitsInfo, define_hint: &ErrorHint) -> Result<(), Error> {
        if let FuncBlock::Block(ref block) = self.block {
            if self.func_id == Identifier::from_str("main") {
                if self.generics.len() > 0 {
                    Err(format!("main function must not have generics arguments"))
                }
                else if !self.where_sec.is_empty() {
                    Err(format!("main function must not have where sections"))
                }
                else if self.return_type != TypeSpec::from_str("void") {
                    Err(format!("main function must return void"))
                }
                else {
                    Ok(())
                }
            }
            else {
                Ok(())
            }.map_err(|s| ErrorComment::empty(s))?;

            equs.into_scope();

            let mut trs = trs.into_scope();

            for ty_id in self.generics.iter() {
                trs.regist_generics_type(ty_id)?;
                /* if let Some(trait_id) = trait_id {
                   trs.regist_param_candidate(equs, ty_id, trait_id)?;
                   }*/
            }

            self.where_sec.regist_candidate(equs, &mut trs, &self.def_range.hint("function define", define_hint.clone()))?;

            for (n, (i, t, _)) in self.args.iter().enumerate() {
                let alpha = i.generate_not_void_type_variable("ForRegist", 0, equs);
                let t_type = t.generics_to_type(&GenericsTypeMap::empty(), equs, &trs)?; 
                equs.regist_variable(Variable::from_identifier(i.clone()), alpha.clone());
                equs.add_equation(alpha, t_type, ErrorComment::new(format!("type variable for {}-th arg", n), self.def_range.hint("function definition", ErrorHint::None).err()));
            }
            let result_type = block.gen_type(equs, &trs)?;
            let return_t = self.return_type.generics_to_type(&GenericsTypeMap::empty(), equs, &trs)?;
            equs.add_equation(result_type, return_t, ErrorComment::new(format!("type variable for return"), self.def_range.hint("function definition", ErrorHint::None).err()));

            log::info!("function {:?} unify", self.func_id);
            equs.debug();
            let result = equs.unify(&mut trs).map_err(|e| e.into_err());

            equs.out_scope();
            result
        }
        else {
            Ok(())
        }
    }

    pub fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<(), String> {
        vars.into_scope();
        for (id, ty, is_mutable) in self.args.iter() {
            vars.regist_variable(id, *is_mutable);
            if ty.is_reference() && *is_mutable {
                return Err(format!("Reference cant be mutable, {:?}", id));
            }
        }
        if let FuncBlock::Block(ref block) = self.block {
            block.mut_check(ta, vars)?;
        }
        vars.out_scope();
        Ok(())
    }
    pub fn transpile_definition_only(&self, ta: &TypeAnnotation, class_str: &str, is_static: bool) -> String {
        let where_empty = self.where_sec.is_empty();
        let where_str = self.where_sec.transpile(ta);
        let template_str =
            if self.generics.len() > 0 {
                let gen = self.generics.iter().map(|g| format!("class {}", g.transpile(ta))).collect::<Vec<_>>().join(", ");
                if where_empty  {
                    format!("template<{}> ", gen)
                }
                else {
                    format!("template<{}, class = {}> ", gen, where_str)
                }
            } 
            else if !where_empty {
                format!("template<class = {}> ", where_str)
            }
            else {
                "".to_string()
            };

        let return_str = if self.func_id == Identifier::from_str("main") {
            format!("int")
        }
        else {
            self.return_type.transpile(ta)
        };
        let static_str = if is_static { "static " } else { "" };
        let func_str = self.func_id.into_string();
        let arg_str = self.args.iter().map(|(id, ty, is_mutable)| {
            if *is_mutable || ty.is_reference() {
                format!("{} {}", ty.transpile(ta), id.into_string())
            }
            else {
                //format!("{} const {}", ty.transpile(ta), id.into_string())
                format!("{} {}", ty.transpile(ta), id.into_string())
            }
        }).collect::<Vec<_>>().join(", ");

        format!("{}{}{} {}{}({})", template_str, static_str, return_str, class_str, func_str, arg_str)
    }

    fn transpile_definition(&self, ta: &TypeAnnotation, class_str: &str, is_static: bool) -> String {
        let _where_str = self.where_sec.transpile(ta);
        let template_str =
            if self.generics.len() > 0 {
                let gen = self.generics.iter().map(|g| format!("class {}", g.transpile(ta))).collect::<Vec<_>>().join(", ");
                if self.where_sec.is_empty() {
                    format!("template<{}> ", gen)
                }
                else {
                    format!("template<{}, class> ", gen)
                }
            } 
            else if !self.where_sec.is_empty() {
                format!("template<class> ")
            }
            else {
                "".to_string()
            };

        let return_str = if self.func_id == Identifier::from_str("main") {
            format!("int")
        }
        else {
            self.return_type.transpile(ta)
        };
        let static_str = if is_static { "static " } else { "" };
        let func_str = self.func_id.into_string();
        let arg_str = self.args.iter().map(|(id, ty, is_mutable)| {
            if *is_mutable | ty.is_reference() {
                format!("{} {}", ty.transpile(ta), id.into_string())
            }
            else {
                //format!("{} const {}", ty.transpile(ta), id.into_string())
                format!("{} {}", ty.transpile(ta), id.into_string())
            }
        }).collect::<Vec<_>>().join(", ");

        format!("{}{}{} {}{}({})", template_str, static_str, return_str, class_str, func_str, arg_str)
    }
    pub fn transpile_for_impl(&self, ta: &TypeAnnotation, class_str: &str, is_static: bool) -> String {
        match self.block {
            FuncBlock::Block(ref block) => {
                let func_def = self.transpile_definition(ta, class_str, is_static);
                let block_str = block.transpile(ta);
                format!("{} {{\n{}}}\n", func_def, block_str)
            }
            FuncBlock::CppInline(ref block) => {
                let func_def = self.transpile_definition(ta, class_str, is_static);
                let block_str = block.transpile_implement(ta);
                format!("{} {{\nreturn {};\n}}\n", func_def, block_str)
            }
        }
    }
    pub fn transpile(&self, ta: &TypeAnnotation, is_static: bool) -> String {
        if let FuncBlock::Block(ref block) = self.block {
            let func_def = self.transpile_definition(ta, "", is_static);
            let block_str = block.transpile(ta);
            format!("{} {{\n{}}}\n", func_def, block_str)
        }
        else {
            format!("")
        }
    }
    pub fn move_check(&self, top_mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<(), String> {
        log::info!("move check {:?}", self.func_id);
        let mut mc = VariablesMoveChecker::new();
        for (id, _, _) in self.args.iter() {
            mc.regist_var(id);
        }
        if let FuncBlock::Block(ref block) = self.block {
            block.move_check(&mut mc, ta)?;
        }
        top_mc.solve_lazys(mc)?;
        Ok(())
    }
}


/*fn parse_generics_arg(s: &str) -> IResult<&str, (TypeId, Option<TraitId>)> {
    let (s, (id, _, opt)) = tuple((parse_type_id, multispace0, opt(tuple((char(':'), multispace0, parse_trait_id)))))(s)?;
    Ok((s, (id, opt.map(|(_, _, tr)| tr))))
}*/

fn parse_mutable(s: ContentStr<'_>) -> IResult<ContentStr<'_>, bool> {
    let (s, op) = opt(tuple((tag("mut"), multispace1)))(s)?;
    let is_mutable = match op {
        Some(_) => true,
        None => false,
    };
    Ok((s, is_mutable))
}

pub fn parse_func_definition_info(s: ContentStr<'_>) -> IResult<ContentStr<'_>, FuncDefinitionInfo> {
    let (s, ((_, _, func_id, _, generics_opt, _, _, _, op, _, _, _, _, return_type, _, where_sec), range)) = 
        with_range(tuple((tag("fn"), multispace1, parse_identifier, multispace0, opt(tuple((char('<'), multispace0, opt(tuple((parse_type_id, multispace0, many0(tuple((char(','), multispace0, parse_type_id, multispace0))), opt(char(',')), multispace0))), char('>'), multispace0))), multispace0,
               char('('), multispace0,
            opt(tuple((parse_mutable, parse_identifier, multispace0, char(':'), multispace0, parse_type_spec, multispace0, many0(tuple((char(','), multispace0, parse_mutable, parse_identifier, multispace0, char(':'), multispace0, parse_type_spec, multispace0))), opt(char(',')), multispace0))),
            char(')'), multispace0, tag("->"), multispace0, parse_type_spec, multispace0, parse_where_section)))(s)?;
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
        Some((is_mut0, arg0, _, _, _, ty0, _, many, _, _)) => {
            let mut args = vec![(arg0, ty0, is_mut0)];
            for (_, _, is_mut, arg, _, _, _, ty, _) in many {
                args.push((arg, ty, is_mut));
            }
            args
        }
        None => Vec::new(),
    };
    Ok((s, FuncDefinitionInfo { func_id, generics, where_sec, args, return_type, inline: None, range }))
}

fn parse_func_block_block(s: ContentStr<'_>) -> IResult<ContentStr<'_>, FuncBlock> {
    let (s, block) = parse_block(s)?;
    Ok((s, FuncBlock::Block(block)))
}

fn parse_func_block_cppinline(s: ContentStr<'_>) -> IResult<ContentStr<'_>, FuncBlock> {
    let (s, inline) = parse_cpp_inline(s)?;
    Ok((s, FuncBlock::CppInline(inline)))
}


fn parse_func_block(s: ContentStr<'_>) -> IResult<ContentStr<'_>, FuncBlock> {
    alt((parse_func_block_block, parse_func_block_cppinline))(s)
}

pub fn parse_func_definition(s: ContentStr<'_>) -> IResult<ContentStr<'_>, FuncDefinition> {
    let (s, (info, _, block)) = tuple((parse_func_definition_info, multispace0, parse_func_block))(s)?;
    Ok((s, FuncDefinition { func_id: info.func_id, generics: info.generics, where_sec: info.where_sec, args: info.args, return_type: info.return_type, block, def_range: info.range.clone() }))
}


#[test]
fn parse_func_definition_test() {
    log::debug!("{:?}", parse_func_definition("fn func(x: i64) -> i64 { let y = x * x; y + x }".into_content(0)).ok());
    log::debug!("{:?}", parse_func_definition("fn func2<t>(x: t) -> t { x }".into_content(0)).ok());
    log::debug!("{:?}", parse_func_definition("fn func3<x, y, z>(x: x) -> z { x }".into_content(0)).ok());
}
#[test]
fn parse_func_definition2_test() {
    log::debug!("{:?}", parse_func_definition("fn func2<t>(x: t) -> t where t: MyTraits{ x }".into_content(0)).ok());
    log::debug!("{:?}", parse_func_definition_info("fn nest_out<T>(t: T) -> T#MyTrait::Output#MyTrait::Output where T: MyTrait, T#MyTrait::Output: MyTrait".into_content(0)).ok());
}

#[test]
fn parse_func_mutable_argument_test() {
    log::debug!("{:?}", parse_func_definition("fn func(mut x: i64, mut y: u64) -> bool { false }".into_content(0)).unwrap());
}

#[test]
fn parse_func_cppinline_test() {
    log::debug!("{:?}", parse_func_definition("fn push_back(self: Self, t: T) -> bool $${ $arg(self).push_back($arg(t)) }$$".into_content(0)).ok());
}
