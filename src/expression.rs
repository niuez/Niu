pub mod if_expr;

//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;
use nom::branch::*;

use crate::unary_expr::{ UnaryExpr, parse_unary_expr };
use crate::unify::*;
use crate::trans::*;

pub use if_expr::*;

#[derive(Debug)]
pub enum Expression {
    IfExpr(Box<IfExpr>),
    Expression(ExpOr),
}

impl GenType for Expression {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            Expression::Expression(ref e) => e.gen_type(equs, trs),
            Expression::IfExpr(ref ifexpr) => ifexpr.as_ref().gen_type(equs, trs),
        }
    }
}

impl Transpile for Expression {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Expression::Expression(ref e) => e.transpile(ta),
            Expression::IfExpr(ref ifexpr) => ifexpr.as_ref().transpile(ta),
        }
    }
}

fn default_parse_expression<P: ParseExpression>(s: &str) -> IResult<&str, P>
where
    P::Child: ParseExpression,
    P::Operator: ParseOperator,
{
    let (s, (head, _, tails)) = 
        tuple((P::Child::parse_expression, space0, many0(tuple((P::Operator::parse_operator, space0, P::Child::parse_expression, space0)))))(s)?;
    let mut terms = vec![head];
    let mut opes = Vec::new();

    for (ope, _, term, _) in tails {
        terms.push(term);
        opes.push(ope);
    }
    Ok((s, P::new_expr(terms, opes)))
}

trait ParseExpression: Sized {
    type Child: Sized;
    type Operator: Sized;
    fn new_expr(childs: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self;
    fn parse_expression(s: &str) -> IResult<&str, Self>;
}

trait ParseOperator: Sized {
    fn parse_operator(s: &str) -> IResult<&str, Self>;
}

#[derive(Debug)]
pub struct ExpOr {
    pub terms: Vec<ExpAnd>,
    pub opes: Vec<OperatorOr>,
}

impl GenType for ExpOr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        if self.terms.len() > 1 {
            for t in self.terms.iter() {
                let ty = t.gen_type(equs, trs)?;
                equs.add_equation(ty, Type::from_str("bool"));
            }
            Ok(Type::from_str("bool"))
        }
        else {
            self.terms[0].gen_type(equs, trs)
        }
    }
}

#[derive(Debug)]
pub struct OperatorOr();

impl Transpile for ExpOr {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorOr {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        "||".to_string()
    }
}


impl ParseExpression for ExpOr {
    type Child = ExpAnd;
    type Operator = OperatorOr;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorOr {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = tag("||")(s)?;
        Ok((s, OperatorOr()))
    }
}

#[derive(Debug)]
pub struct ExpAnd {
    pub terms: Vec<ExpOrd>,
    pub opes: Vec<OperatorAnd>,
}

impl GenType for ExpAnd {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        if self.terms.len() > 1 {
            for t in self.terms.iter() {
                let ty = t.gen_type(equs, trs)?;
                equs.add_equation(ty, Type::from_str("bool"));
            }
            Ok(Type::from_str("bool"))
        }
        else {
            self.terms[0].gen_type(equs, trs)
        }
    }
}

#[derive(Debug)]
pub struct OperatorAnd();

impl Transpile for ExpAnd {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorAnd {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        "&&".to_string()
    }
}

impl ParseExpression for ExpAnd {
    type Child = ExpOrd;
    type Operator = OperatorAnd;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorAnd {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = tag("&&")(s)?;
        Ok((s, OperatorAnd()))
    }
}


#[derive(Debug)]
pub struct ExpOrd {
    pub terms: Vec<ExpBitOr>,
    pub ope: Option<OperatorOrd>,
}

impl GenType for ExpOrd {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match self.ope {
            Some(_) => {
                let t0 = self.terms[0].gen_type(equs, trs)?;
                let t1 = self.terms[1].gen_type(equs, trs)?;
                equs.add_equation(t0, t1);
                Ok(Type::from_str("bool"))
            }
            None => self.terms[0].gen_type(equs, trs),

        }
    }
}

#[derive(Debug)]
pub enum OperatorOrd {
    Equal,
    NotEq,
    Less,
    Greater,
    Leq,
    Grq,
}

impl Transpile for ExpOrd {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match self.ope {
            Some(ref o) => format!("{} {} {}", self.terms[0].transpile(ta), o.transpile(ta), self.terms[1].transpile(ta)),
            None => self.terms[0].transpile(ta),
        }
    }
}

impl Transpile for OperatorOrd {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        match *self {
            OperatorOrd::Equal  => "==",
            OperatorOrd::NotEq  => "!=",
            OperatorOrd::Less   => "<",
            OperatorOrd::Greater=> ">",
            OperatorOrd::Leq    => "<=",
            OperatorOrd::Grq    => ">=",
        }.to_string()
    }
}

impl ParseExpression for ExpOrd {
    type Child = ExpBitOr;
    type Operator = OperatorOrd;
    fn new_expr(terms: Vec<Self::Child>, mut opes: Vec<Self::Operator>) -> Self {
        if terms.len() == 1 && opes.len() == 0 {
            Self { terms, ope: None }
        }
        else if terms.len() == 2 && opes.len() == 1 {
            Self { terms, ope: Some(opes.remove(0)) }
        }
        else {
            unreachable!();
        }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorOrd {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = alt((tag("=="), tag("!="), tag("<="), tag(">="), tag("<"), tag(">")))(s)?;
        let ope = match c {
            "==" => OperatorOrd::Equal,
            "!=" => OperatorOrd::NotEq,
            "<" => OperatorOrd::Less,
            ">" => OperatorOrd::Greater,
            "<=" => OperatorOrd::Leq,
            ">=" => OperatorOrd::Grq,
            _ => unreachable!("expression denided")
        };
        Ok((s, ope))
    }
}

#[derive(Debug)]
pub struct ExpBitOr {
    pub terms: Vec<ExpBitXor>,
    pub opes: Vec<OperatorBitOr>,
}

impl GenType for ExpBitOr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let ty = self.terms.iter().map(|t| t.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
        for i in 0..self.opes.len() {
            equs.add_equation(ty[i].clone(), ty[i + 1].clone());
        }
        Ok(ty[0].clone())
    }
}

#[derive(Debug)]
pub struct OperatorBitOr();

impl Transpile for ExpBitOr {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorBitOr {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        "|".to_string()
    }
}

impl ParseExpression for ExpBitOr {
    type Child = ExpBitXor;
    type Operator = OperatorBitOr;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorBitOr {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('|')(s)?;
        Ok((s, OperatorBitOr()))
    }
}

#[derive(Debug)]
pub struct ExpBitXor {
    pub terms: Vec<ExpBitAnd>,
    pub opes: Vec<OperatorBitXor>,
}

impl GenType for ExpBitXor {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let ty = self.terms.iter().map(|t| t.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
        for i in 0..self.opes.len() {
            equs.add_equation(ty[i].clone(), ty[i + 1].clone());
        }
        Ok(ty[0].clone())
    }
}

#[derive(Debug)]
pub struct OperatorBitXor();

impl Transpile for ExpBitXor {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorBitXor {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        "^".to_string()
    }
}

impl ParseExpression for ExpBitXor {
    type Child = ExpBitAnd;
    type Operator = OperatorBitXor;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorBitXor {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('^')(s)?;
        Ok((s, OperatorBitXor()))
    }
}

#[derive(Debug)]
pub struct ExpBitAnd {
    pub terms: Vec<ExpShift>,
    pub opes: Vec<OperatorBitAnd>,
}

impl GenType for ExpBitAnd {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let ty = self.terms.iter().map(|t| t.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
        for i in 0..self.opes.len() {
            equs.add_equation(ty[i].clone(), ty[i + 1].clone());
        }
        Ok(ty[0].clone())
    }
}

#[derive(Debug)]
pub struct OperatorBitAnd();

impl Transpile for ExpBitAnd {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorBitAnd {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        "&".to_string()
    }
}

impl ParseExpression for ExpBitAnd {
    type Child = ExpShift;
    type Operator = OperatorBitAnd;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorBitAnd {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('&')(s)?;
        Ok((s, OperatorBitAnd()))
    }
}

#[derive(Debug)]
pub struct ExpShift {
    pub terms: Vec<ExpAddSub>,
    pub opes: Vec<OperatorShift>,
}

impl GenType for ExpShift {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let ty = self.terms.iter().map(|t| t.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
        for i in 0..self.opes.len() {
            equs.add_equation(ty[i].clone(), ty[i + 1].clone());
        }
        Ok(ty[0].clone())
    }
}

#[derive(Debug)]
pub enum OperatorShift {
    Shl,
    Shr,
}

impl Transpile for ExpShift {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorShift {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        match *self {
            OperatorShift::Shl => "<<",
            OperatorShift::Shr => ">>",
        }.to_string()
    }
}

impl ParseExpression for ExpShift {
    type Child = ExpAddSub;
    type Operator = OperatorShift;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorShift {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = alt((tag("<<"), tag(">>")))(s)?;
        let ope = match c {
            "<<" => OperatorShift::Shl,
            ">>" => OperatorShift::Shr,
            _ => unreachable!()
        };
        Ok((s, ope))
    }
}


#[derive(Debug)]
pub struct ExpAddSub {
    pub terms: Vec<ExpMulDivRem>,
    pub opes: Vec<OperatorAddSub>,
}

impl GenType for ExpAddSub {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let ty = self.terms.iter().map(|t| t.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
        for i in 0..self.opes.len() {
            equs.add_equation(ty[i].clone(), ty[i + 1].clone());
        }
        Ok(ty[0].clone())
    }
}

#[derive(Debug)]
pub enum OperatorAddSub {
    Add,
    Sub,
}

impl Transpile for ExpAddSub {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.terms.len() {
            res.push_str(&self.terms[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorAddSub {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        match *self {
            OperatorAddSub::Add => "+",
            OperatorAddSub::Sub => "-",
        }.to_string()
    }
}

impl ParseExpression for ExpAddSub {
    type Child = ExpMulDivRem;
    type Operator = OperatorAddSub;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorAddSub {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = one_of("+-")(s)?;
        let ope = match c {
            '+' => OperatorAddSub::Add,
            '-' => OperatorAddSub::Sub,
            _ => unreachable!()
        };
        Ok((s, ope))
    }
}

#[derive(Debug)]
pub struct ExpMulDivRem {
    pub unary_exprs: Vec<UnaryExpr>,
    pub opes: Vec<OperatorMulDivRem>,
}

impl GenType for ExpMulDivRem {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let ty = self.unary_exprs.iter().map(|t| t.gen_type(equs, trs)).collect::<Result<Vec<_>, String>>()?;
        for i in 0..self.opes.len() {
            equs.add_equation(ty[i].clone(), ty[i + 1].clone());
        }
        Ok(ty[0].clone())
    }
}

#[derive(Debug)]
pub enum OperatorMulDivRem {
    Mul,
    Div,
    Rem
}

impl Transpile for ExpMulDivRem {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        let mut res = String::new();
        for i in 0..self.unary_exprs.len() {
            res.push_str(&self.unary_exprs[i].transpile(ta));
            if i < self.opes.len() { res.push_str(&self.opes[i].transpile(ta)); }
        }
        res
    }
}

impl Transpile for OperatorMulDivRem {
    fn transpile(&self, _: &TypeAnnotation) -> String {
        match *self {
            OperatorMulDivRem::Mul => "*",
            OperatorMulDivRem::Div => "/",
            OperatorMulDivRem::Rem => "%",
        }.to_string()
    }
}

impl ParseExpression for ExpMulDivRem {
    type Child = UnaryExpr;
    type Operator = OperatorMulDivRem;
    fn new_expr(unary_exprs: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { unary_exprs, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        let (s, (head, _, tails)) = 
            tuple((parse_unary_expr, space0, many0(tuple((Self::Operator::parse_operator, space0, parse_unary_expr, space0)))))(s)?;
        let mut unary_exprs = vec![head];
        let mut opes = Vec::new();

        for (ope, _, expr, _) in tails {
            unary_exprs.push(expr);
            opes.push(ope);
        }
        Ok((s, ExpMulDivRem { unary_exprs, opes }))
    }
}

impl ParseOperator for OperatorMulDivRem {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = one_of("*/%")(s)?;
        let ope = match c {
            '*' => OperatorMulDivRem::Mul,
            '/' => OperatorMulDivRem::Div,
            '%' => OperatorMulDivRem::Rem,
            _ => unreachable!()
        };
        Ok((s, ope))
    }
}




pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    let (s, expr) = alt((parse_if_expr, parse_expor))(s)?;
    Ok((s, expr))
}

pub fn parse_expor(s: &str) -> IResult<&str, Expression> {
    let (s, p) = ExpOr::parse_expression(s)?;
    Ok((s, Expression::Expression(p)))
}


#[test]
fn parse_expression_test() {
    println!("{:?}", parse_expression("1 + 2 - 3 + 4 - 5"));
    println!("{:?}", parse_expression("func(1 + 2, 3 - 4)"));
    println!("{:#?}", parse_expression("1 + 2 * 3 - 4 / 5"));
    println!("{:#?}", parse_expression("(1 + 2) * (3 - 4) / 5"));
}

#[test]
fn parse_bit_test() {
    println!("{:#?}", parse_expression("1 & 2 | 3 ^ 4"));
}
#[test]
fn parse_conditions_test() {
    println!("{:?}", parse_expression("1 == 2"));
    println!("{:?}", parse_expression("1 != 2"));
    println!("{:?}", parse_expression("1 < 2"));
    println!("{:?}", parse_expression("1 > 2"));
    println!("{:?}", parse_expression("1 <= 2"));
    println!("{:?}", parse_expression("1 >= 2"));
}

#[test]
fn parse_all_test() {
    println!("{:?}", parse_expression("1 + 2 == 3 * 4 || 5 << 6 & 7 >> 8 != 9 | 0 ^ 1 && 2 % 3 < 4 / 5 && 6 > 7 && 8 < 9 || 0 <= 1 && 2 >= 3"));
}

#[test]
#[should_panic]
fn parse_conditions_failure_test() {
    println!("{:?}", parse_expression("1 == 2 == 3"));
}
