extern crate nom;

pub mod literal;
pub mod expression;
pub mod identifier;
pub mod subseq;

pub mod unary_expr;

use literal::literal_integral_number;

fn main() {
    println!("{:?}", literal_integral_number("659"));
    println!("{:?}", literal_integral_number("6_5_9"));
    println!("{:?}", literal_integral_number("659i64"));
    println!("{:?}", literal_integral_number("6_5_9i64"));

    //println!("{:?}", parse_function_apply("func()"));
    //println!("{:?}", parse_function_apply("func(1, 2)"));
    //println!("{:?}", parse_function_apply("func(1, 2, 3,)"));
    //println!("{:?}", parse_function_apply("func(   1,2,3,    )"));
    //println!("{:?}", parse_function_apply("add(func(1), func(2), func(3))"));
}
