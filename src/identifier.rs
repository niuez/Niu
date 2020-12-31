use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Identifier {
    pub name: String,
}

impl<'a> Identifier {
    pub fn from_str(s: &str) -> Self {
        Identifier { name: s.to_string() }
    }
    pub fn from_vec_str(vec: Vec<&str>) -> Self {
        Identifier { name: vec.join("") }
    }
    pub fn into_string(&self) -> String {
        self.name.clone()
    }
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
    Ok((s, Identifier::from_vec_str(name)))
}

#[test]
fn parse_identifier_test() {
    println!("{:?}", parse_identifier("func"));
    println!("{:?}", parse_identifier("if"));
    println!("{:?}", parse_identifier("x"));
    println!("{:?}", parse_identifier("f_u_n_c91"));
}
