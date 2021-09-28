use std::collections::HashMap;

use crate::identifier::*;
use crate::cpp_inline::*;
use crate::trans::*;

use nom::IResult;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::sequence::*;

#[derive(Debug)]
pub struct UnitTestFunc {
    pub tester: String,
    pub problem: String,
    pub test_name: Identifier,
    block: CppInline,
}

impl UnitTestFunc {
    pub fn transpile(&self, ta: &TypeAnnotation) -> String {
        self.block.transpile(ta, &HashMap::new())
    }
}

fn parse_tester(s: &str) -> IResult<&str, String> {
    let (s, tester) = is_not(":")(s)?;
    Ok((s, tester.to_string()))
}

fn parse_problem(s: &str) -> IResult<&str, String> {
    let (s, tester) = is_not(")")(s)?;
    Ok((s, tester.to_string()))
}


pub fn parse_unit_test_func(s: &str) -> IResult<&str, UnitTestFunc> {
    let (s, (_, _, _, tester, _, problem, _, _, test_name, _, cpp_inline)) = tuple((tag("testfn"), multispace0, char('('), parse_tester,
        char(':'), parse_problem,
        char(')'), multispace0, parse_identifier,
        multispace0, parse_cpp_inline
    ))(s)?;
    Ok((s, UnitTestFunc { tester, problem, test_name, block: cpp_inline }))
}

#[test]
fn parse_unit_test_func_test() {
    println!("{:?}", parse_unit_test_func(r#"testfn(library-checker-problems:sample/aplusb) aplusb_test $${
    long long a, b;
    std::cin >> a >> b;
    std::cout << a + b << std::endl;
}$$"#).unwrap())
}
