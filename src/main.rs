extern crate nom;

pub mod literal;
pub mod expression;
pub mod identifier;
pub mod subseq;

pub mod unary_expr;

pub mod statement;
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

use crate::trans::Transpile;

fn type_check() -> Result<String, String> {
    let args = std::env::args().collect::<Vec<_>>();
    let filename = args.get(1).ok_or("no filepath")?;
    let program = std::fs::read_to_string(filename).map_err(|_| format!("cant open {}", filename))?;
    let program = program.chars().map(|c| match c { '\n' => ' ', c => c }).collect::<String>();
    let (s, mut t) = crate::full_content::parse_full_content(&program).map_err(|e| format!("{:?}", e))?;
    println!("{:?}", t);
    if s != "" {
        Err(format!("parse error, remaining -> {}", s))
    }
    else {
        let mut ta = t.type_check()?;
        Ok(t.transpile(&mut ta))
    }
}

fn main() {
    match type_check() {
        Ok(prog) => println!("{}", prog),
        err => println!("{:?}", err),
    }
}
