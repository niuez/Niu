//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;
use nom::branch::*;
use crate::unary_expr::{ UnaryExpr, parse_unary_expr };

#[derive(Debug)]
pub enum Expression {
    Expression(ExpOr),
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

#[derive(Debug)]
pub struct OperatorOr();

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

#[derive(Debug)]
pub struct OperatorAnd();

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

#[derive(Debug)]
pub enum OperatorOrd {
    Equal,
    NotEq,
    Less,
    Greater,
    Leq,
    Grq,
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

#[derive(Debug)]
pub struct OperatorBitOr();

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

#[derive(Debug)]
pub struct OperatorBitXor();

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

#[derive(Debug)]
pub struct OperatorBitAnd();

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

#[derive(Debug)]
pub enum OperatorShift {
    Shl,
    Shr,
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
    pub terms: Vec<ExpMulDevRem>,
    pub opes: Vec<OperatorAddSub>,
}

#[derive(Debug)]
pub enum OperatorAddSub {
    Add,
    Sub,
}

impl ParseExpression for ExpAddSub {
    type Child = ExpMulDevRem;
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
pub struct ExpMulDevRem {
    pub unary_exprs: Vec<UnaryExpr>,
    pub opes: Vec<OperatorMulDevRem>,
}

#[derive(Debug)]
pub enum OperatorMulDevRem {
    Mul,
    Div,
    Rem
}

impl ParseExpression for ExpMulDevRem {
    type Child = UnaryExpr;
    type Operator = OperatorMulDevRem;
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
        Ok((s, ExpMulDevRem { unary_exprs, opes }))
    }
}

impl ParseOperator for OperatorMulDevRem {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, c) = one_of("*/%")(s)?;
        let ope = match c {
            '*' => OperatorMulDevRem::Mul,
            '/' => OperatorMulDevRem::Div,
            '%' => OperatorMulDevRem::Rem,
            _ => unreachable!()
        };
        Ok((s, ope))
    }
}




pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
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
