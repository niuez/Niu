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
pub mod move_checker;

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
    t.move_check(&ta)?;
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

fn app() -> Result<(), String> {
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
            Ok(prog) => {
                println!("{}", prog);
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
    else if let Some(_matches) = matches.subcommand_matches("gen") {
        let library_dir = subcommand::unit_test::get_library_dir(Path::new("."))?;
        subcommand::create_test_directory(&library_dir)?;
        log::info!("created .test directory");
        subcommand::generate::generate_headers(&library_dir)?;
        log::info!("generate finished");
        Ok(())
    }
    else if let Some(matches) = matches.subcommand_matches("test") {
        let library_dir = subcommand::unit_test::get_library_dir(Path::new("."))?;
        subcommand::create_test_directory(&library_dir)?;
        log::info!("created .test directory");
        if matches.occurrences_of("nogen") == 0 {
            subcommand::generate::generate_headers(&library_dir)?;
            log::info!("generate finished");
        }
        subcommand::tester::download_testers(&library_dir)?;
        log::info!("download finished");
        subcommand::unit_test::test_cppfiles(&library_dir)?;
        log::info!("test finished");
        Ok(())
    }
    else {
        Ok(())
    }
}

fn main() {
    std::process::exit( match app() {
        Ok(_) => 0,
        Err(err) => {
            log::error!("{}", err);
            1
        }
    });
}
