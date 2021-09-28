extern crate nom;
extern crate log;
extern crate clap;

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
pub mod unit_test;

pub mod subcommand;

use std::path::*;

//use crate::trans::Transpile;
use crate::unit_test::*;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::sequence::*;
use nom::multi::*;

use clap::{ Arg, App, SubCommand };

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

fn type_check(filename: &str) -> Result<String, String> {
    let import_path = get_import_path()?;
    let mut t = crate::full_content::parse_full_content_from_file(&filename, &import_path).map_err(|e| format!("{:?}", e))?;
    //log::debug!("{:?}", t);
    let mut ta = t.type_check()?;
    t.mut_check(&ta)?;
    Ok(t.transpile(&mut ta))
}

pub fn type_check_with_tests(filename: &str) -> Result<(String, Vec<UnitTestTranspiled>), String> {
    let import_path = get_import_path()?;
    let mut t = crate::full_content::parse_full_content_from_file(&filename, &import_path).map_err(|e| format!("{:?}", e))?;
    //log::debug!("{:?}", t);
    let mut ta = t.type_check()?;
    t.mut_check(&ta)?;
    let main_prog = t.transpile(&mut ta);
    let unit_tests = t.transpile_tests(&ta);
    Ok((main_prog, unit_tests))
}

fn main() {
    env_logger::init();

    let matches = App::new("Niu transpiler")
        .version("0.6.1")
        .author("niuez")
        .subcommand(SubCommand::with_name("trans")
                    .about("transpile .niu program")
                    .arg(Arg::with_name("FILE")
                         .help("input .niu file")
                         .required(true)
                         .index(1)
                    ))
        .subcommand(SubCommand::with_name("test")
                    .about("test .niu programs")
                    .arg(
                        Arg::with_name("nogen")
                        .long("nogen")
                        .help("no generate before test")
                    ))
        .subcommand(SubCommand::with_name("gen")
                    .about("generate headers and tests")
                    )
        .get_matches();
    if let Some(matches) = matches.subcommand_matches("trans") {
        match type_check(matches.value_of("FILE").unwrap()) {
            Ok(prog) => println!("{}", prog),
            Err(err) => log::error!("{}", err),
        }
    }
    else if let Some(_matches) = matches.subcommand_matches("gen") {
        match subcommand::create_test_directory(Path::new(".")) {
            Ok(_) => log::info!("created .test directory"),
            Err(err) => {
                log::error!("{}", err);
                return;
            }
        }
        match subcommand::generate::generate_headers(Path::new(".")) {
            Ok(_) => log::info!("generate finished"),
            Err(err) => log::error!("{}", err),
        }
    }
    else if let Some(matches) = matches.subcommand_matches("test") {
        match subcommand::create_test_directory(Path::new(".")) {
            Ok(_) => log::info!("created .test directory"),
            Err(err) => {
                log::error!("{}", err);
                return;
            }
        }
        if matches.occurrences_of("nogen") == 0 {
            match subcommand::generate::generate_headers(Path::new(".")) {
                Ok(_) => log::info!("generate finished"),
                Err(err) => log::error!("{}", err),
            }
        }
        match subcommand::tester::download_testers(Path::new(".")) {
            Ok(_) => log::info!("download finished"),
            Err(err) => {
                log::error!("{}", err);
                return;
            }
        }
        match subcommand::unit_test::test_cppfiles(Path::new(".")) {
            Ok(_) => log::info!("test finished"),
            Err(err) => log::error!("{}", err),
        }
    }
}
