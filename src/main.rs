extern crate nom;
extern crate log;

pub mod literal;
pub mod expression;
pub mod identifier;
pub mod subseq;

pub mod unary_expr;

pub mod statement;
pub mod substitute;
pub mod let_declaration;

pub mod block;

pub mod type_id;
pub mod type_spec;
pub mod func_definition;

pub mod full_content;

pub mod unify;

pub mod trans;

pub mod traits;

pub mod structs;

pub mod cpp_inline;

pub mod mut_checker;

use std::path::*;

use crate::trans::Transpile;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;

fn get_import_path() -> Result<Vec<PathBuf>, String> {
    let paths = std::env::var("NIU_IMPORT_PATH").unwrap_or(format!(""));
    let parse: IResult<&str, Vec<_>> = many0(tuple((is_not(";"), char(';'))))(paths.as_str());
    let (s, paths) = parse.map_err(|e| format!("{:?}", e))?;
    if s != "" {
        Err(format!("get import path remaining {}", s))
    }
    else {
        Ok(paths.into_iter().map(|(path, _)| Path::new(path).to_path_buf()).collect())
    }
}

fn type_check() -> Result<String, String> {
    let args = std::env::args().collect::<Vec<_>>();
    let filename = args.get(1).ok_or("no filepath")?;
    let import_path = get_import_path()?;
    let mut t = crate::full_content::parse_full_content_from_file(&filename, &import_path).map_err(|e| format!("{:?}", e))?;
    //log::debug!("{:?}", t);
    let ta = t.type_check()?;
    t.mut_check(&ta)?;
    Ok(t.transpile(&ta))
}

fn main() {
    env_logger::init();
    match type_check() {
        Ok(prog) => println!("{}", prog),
        Err(err) => log::error!("{}", err),
    }
}
