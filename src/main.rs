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

fn main() {
    //println!("{:?}", parse_function_apply("func()"));
    //println!("{:?}", parse_function_apply("func(1, 2)"));
    //println!("{:?}", parse_function_apply("func(1, 2, 3,)"));
    //println!("{:?}", parse_function_apply("func(   1,2,3,    )"));
    //println!("{:?}", parse_function_apply("add(func(1), func(2), func(3))"));
}
