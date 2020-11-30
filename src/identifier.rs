use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

#[derive(Debug)]
pub struct Identifier<'a> {
    pub name: Vec<&'a str>,
}

pub fn parse_identifier(s: &str) -> IResult<&str, Identifier> {
    not(all_consuming(alt((
                    tag("if"),
                    tag("else"),
                    tag("while"),
                    tag("for"),
                    tag("in")
                    ))))(s)?;
    let (s, (head, tails)) = tuple((alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_"))))))(s)?;
    let mut name = vec![head];
    for s in tails {
        name.push(s);
    }
    Ok((s, Identifier { name }))
}

#[test]
fn parse_identifier_test() {
    println!("{:?}", parse_identifier("func"));
    println!("{:?}", parse_identifier("if"));
    println!("{:?}", parse_identifier("x"));
    println!("{:?}", parse_identifier("f_u_n_c91"));
}
