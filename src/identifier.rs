use std::sync::atomic::{self, AtomicUsize};

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;

use crate::unify::*;

static TAG_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn get_tag_counter() -> usize {
    TAG_COUNTER.fetch_add(1, atomic::Ordering::SeqCst)
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub tag: Tag,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tag(usize);

impl Tag {
    pub fn new() -> Tag {
        Tag(get_tag_counter())
    }
    pub fn get_num(&self) -> usize {
        self.0
    }
    pub fn generate_type_variable(&self, label: &'static str, num: usize, equs: &mut TypeEquations) -> Type {
        let var = TypeVariable::Counter(self.get_num(), label, num);
        equs.add_want_solve(&var, false);
        Type::TypeVariable(var)
    }
    pub fn generate_not_void_type_variable(&self, label: &'static str, num: usize, equs: &mut TypeEquations) -> Type {
        let var = TypeVariable::Counter(self.get_num(), label, num);
        equs.add_want_solve(&var, true);
        Type::TypeVariable(var)
    }
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
        Identifier { name: s.to_string(), tag: Tag::new() }
    }
    pub fn from_vec_str(vec: Vec<&str>) -> Self {
        Identifier { name: vec.join(""), tag: Tag::new() }
    }
    pub fn into_string(&self) -> String {
        self.name.clone()
    }
    pub fn generate_type_variable(&self, label: &'static str, num: usize, equs: &mut TypeEquations) -> Type {
        self.tag.generate_type_variable(label, num,equs)
    }
    pub fn generate_not_void_type_variable(&self, label: &'static str, num: usize, equs: &mut TypeEquations) -> Type {
        self.tag.generate_not_void_type_variable(label, num, equs)
    }
    pub fn get_tag_number(&self) -> usize {
        self.tag.get_num()
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
    log::debug!("{:?}", parse_identifier("func").ok());
    log::debug!("{:?}", parse_identifier("if").ok());
    log::debug!("{:?}", parse_identifier("x").ok());
    log::debug!("{:?}", parse_identifier("f_u_n_c91").ok());
}
