use crate::identifier::Identifier;


#[derive(Debug, Clone)]
pub enum Type {
    Type(Identifier),
    Func(Box<Type>, Vec<Type>),
    Variable(Identifier),
    CallLazy(CallLazy),
    End,
}

#[derive(Debug, Clone)]
pub struct CallLazy {
    pub caller: Box<Type>,
    pub args: Vec<Type>,
}

#[derive(Debug)]
pub struct TypeEquation {
    pub left: Type,
    pub right: Type,
}

#[derive(Debug)]
pub struct TypeEquations {
    equs: Vec<TypeEquation>,
}

pub type TResult = Result<Type, String>;

pub trait GenType {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult;
}

impl TypeEquations {
    pub fn new() -> Self { Self { equs: Vec::new() } }
    pub fn add_equation(&mut self, left: Type, right: Type) {
        self.equs.push(TypeEquation { left, right });
    }
    pub fn unify(&mut self) {
        loop {
            let mut res = None;
            for i in 0..self.equs.len() {

            }
        }
    }
}
