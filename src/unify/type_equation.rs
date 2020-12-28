use crate::identifier::Identifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Type(Identifier),
    Func(Vec<Type>, Box<Type>),
    TypeVariable(TypeVariable),
    End,
}

impl Type {
    fn occurs(&self, t: &TypeVariable) -> bool {
        match *self {
            Type::TypeVariable(ref s) if s == t => true,
            Type::Func(ref args, ref ret) => {
                for arg in args.iter() {
                    if arg.occurs(t) { return true; }
                }
                if ret.occurs(t) { return true; }
                false
            }
            _ => false,
        }
    }

    fn subst(&mut self, theta: &TypeSubst) {
        match *self {
            Type::Func(ref mut args, ref mut ret) => {
                for arg in args.iter_mut() {
                    arg.subst(theta);
                }
                ret.subst(theta);
            }
            Type::Type(_) => {},
            Type::End => {},
            // TypeVariable
            ref mut t => {
                let x = t.clone_type_variable();
                let TypeSubst { tv: y, t: into_t } = theta;
                if x == *y {
                    *t = into_t.clone();
                }
            }
        }
    }

    fn clone_type_variable(&self) -> TypeVariable {
        if let Type::TypeVariable(ref tv) = *self { tv.clone() }
        else { unreachable!("it is not TypeVariable") }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeVariable {
    Identifier(Identifier),
    Counter(usize),
}

#[derive(Debug)]
pub struct TypeEquation {
    pub left: Type,
    pub right: Type,
}

#[derive(Debug)]
pub struct TypeEquations {
    cnt: usize,
    equs: Vec<TypeEquation>,
}

impl TypeEquations {
}

#[derive(Debug)]
pub struct TypeSubst {
    pub tv: TypeVariable,
    pub t: Type,
}

pub type TResult = Result<Type, String>;

pub trait GenType {
    fn gen_type(&self, equs: &mut TypeEquations) -> TResult;
}

impl TypeEquations {
    pub fn new() -> Self { Self { equs: Vec::new(), cnt: 0, } }
    pub fn add_equation(&mut self, left: Type, right: Type) {
        self.equs.push(TypeEquation { left, right });
    }
    pub fn get_type_variable(&mut self) -> Type {
        let i = self.cnt;
        self.cnt += 1;
        Type::TypeVariable(TypeVariable::Counter(i))
    }

    fn subst(&mut self, theta: &TypeSubst) {
        for TypeEquation { left, right } in self.equs.iter_mut() {
            left.subst(theta);
            right.subst(theta);
        }
    }

    pub fn unify(&mut self) -> Result<Vec<TypeSubst>, String> {
        let mut thetas = Vec::new();
        while let Some(TypeEquation { left, right }) = self.equs.pop() {
            match (left, right) {
                (l, r) if l == r => {}
                (Type::Func(l_args, l_return), Type::Func(r_args, r_return)) => {
                    if l_args.len() != r_args.len() {
                        Err("length of args is not equal.")?;
                    }
                    for (l, r) in l_args.into_iter().zip(r_args.into_iter()) {
                        self.equs.push(TypeEquation { left: l, right: r });
                    }
                    self.equs.push(
                        TypeEquation { left: *l_return,
                        right: *r_return }
                        );
                }
                (Type::TypeVariable(lv), rt) => {
                    if rt.occurs(&lv) {
                        Err("unification failed, occurs")?;
                    }
                    let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                    self.subst(&th);
                    for TypeSubst { t, .. } in thetas.iter_mut() {
                        t.subst(&th);
                    }
                    thetas.push(th);
                }
                (rt, Type::TypeVariable(lv)) => {
                    if rt.occurs(&lv) {
                        Err("unification failed, occurs")?;
                    }
                    let th = TypeSubst { tv: lv.clone(), t: rt.clone() };
                    self.subst(&th);
                    for TypeSubst { t, .. } in thetas.iter_mut() {
                        t.subst(&th);
                    }
                    thetas.push(th);
                }
                (l, r) => {
                    Err(format!("unfication failed, {:?} != {:?}", l, r))?
                }
            }
        }
        Ok(thetas)
    }
}

#[test]
fn test_unify() {

    let counter = |i: usize| Type::TypeVariable(TypeVariable::Counter(i));
    let typ = |s: &str| Type::Type(Identifier::from_str(s));
    {
        let mut equs = TypeEquations { equs:
            vec![
                TypeEquation { left: counter(0), right: Type::Type(Identifier::from_str("int")) }
            ],
            cnt: 0,
        };
        println!("{:?}", equs.unify());
    }
    {
        let mut equs = TypeEquations { equs:
            vec![
                TypeEquation { left: counter(0), right: Type::Type(Identifier::from_str("int")) },
                TypeEquation { left: counter(1), right: Type::Type(Identifier::from_str("int")) }
            ],
            cnt: 0,
        };
        println!("{:?}", equs.unify());
    }
    {
        let mut equs = TypeEquations { equs:
            vec![
                TypeEquation { left: counter(0), right: Type::Type(Identifier::from_str("int")) },
                TypeEquation { left: counter(1), right: counter(0) }
            ],
            cnt: 0,
        };
        println!("{:?}", equs.unify());
    }
    {
        let mut equs = TypeEquations { equs:
            vec![
                TypeEquation { left: counter(0), right: Type::Func(vec![typ("int"), typ("bool")], Box::new(typ("i64"))) },
                TypeEquation { left: counter(3), right: counter(4) },
                TypeEquation { left: counter(0), right: Type::Func(vec![counter(1), counter(2)], Box::new(counter(4))) },
            ],
            cnt: 0,
        };
        println!("{:#?}", equs.unify());
    }
    {
        let mut equs = TypeEquations { equs:
            vec![
                TypeEquation { left: counter(0), right: Type::Func(vec![typ("int"), typ("bool")], Box::new(typ("i64"))) },
                TypeEquation { left: counter(3), right: counter(4) },
                TypeEquation { left: counter(0), right: Type::Func(vec![counter(1), counter(2)], Box::new(counter(4))) },
                TypeEquation { left: counter(2), right: typ("int") },
            ],
            cnt: 0,
        };
        println!("{:#?}", equs.unify());
    }
}
