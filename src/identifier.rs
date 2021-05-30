use std::sync::atomic::{self, AtomicUsize};

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::unify::*;

static IDENTIFIER_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn get_identifier_counter() -> usize {
    IDENTIFIER_COUNTER.fetch_add(1, atomic::Ordering::SeqCst)
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub identifier_cnt: usize,
}

impl PartialEq for Identifier {
    fn eq(&self, right: &Self) -> bool {
        self.name.eq(&right.name)
    }
}
impl Eq for Identifier {}
impl std::hash::Hash for  Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl<'a> Identifier {
    pub fn from_str(s: &str) -> Self {
        Identifier { name: s.to_string(), identifier_cnt: get_identifier_counter() }
    }
    pub fn from_vec_str(vec: Vec<&str>) -> Self {
        Identifier { name: vec.join(""), identifier_cnt: get_identifier_counter() }
    }
    pub fn into_string(&self) -> String {
        self.name.clone()
    }
    pub fn generate_type_variable(&self, num: usize) -> Type {
        Type::TypeVariable(TypeVariable::Counter(self.identifier_cnt, num))
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
