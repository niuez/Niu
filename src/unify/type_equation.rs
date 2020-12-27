use crate::identifier::Identifier;


#[derive(Debug, Clone)]
pub enum Type {
    Type(Identifier),
    Func(Box<Type>, Vec<Type>),
    Variable(Identifier),
    End,
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

impl TypeEquations {
    pub fn new() -> Self { Self { equs: Vec::new() } }
    pub fn add_equation(&mut self, left: Type, right: Type) {
        self.equs.push(TypeEquation { left, right });
    }
}

pub type TResult = Result<Type, String>;

pub trait GenType {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult;
}
