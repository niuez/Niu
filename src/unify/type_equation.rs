use crate::identifier::Identifier;

#[derive(Debug, Clone)]
pub enum Type {
    Type(Identifier),
    Variable(Identifier),
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

pub trait GenType {
    fn gen_type(&self) -> Type;
}
