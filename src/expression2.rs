//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;
use nom::branch::*;
use crate::unary_expr::{ UnaryExpr, parse_unary_expr };

#[derive(Debug)]
pub enum Expression<'a> {
    Expression(ExpOr<'a>),
}

fn default_parse_expression<'a, P: ParseExpression<'a>>(s: &'a str) -> IResult<&'a str, P>
where
    P::Child: ParseExpression<'a>,
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

trait ParseExpression<'a>: Sized {
    type Child: 'a;
    type Operator: 'a;
    fn new_expr(childs: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self;
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self>;
}

trait ParseOperator: Sized {
    fn parse_operator(s: &str) -> IResult<&str, Self>;
}

#[derive(Debug)]
pub struct ExpOr<'a> {
    pub terms: Vec<ExpAnd<'a>>,
    pub opes: Vec<OperatorOr>,
}

#[derive(Debug)]
pub struct OperatorOr();

impl<'a> ParseExpression<'a> for ExpOr<'a> {
    type Child = ExpAnd<'a>;
    type Operator = OperatorOr;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
    }
}

impl ParseOperator for OperatorOr {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = tag("||")(s)?;
        Ok((s, OperatorOr()))
    }
}

#[derive(Debug)]
pub struct ExpAnd<'a> {
    pub terms: Vec<ExpOrd<'a>>,
    pub opes: Vec<OperatorAnd>,
}

#[derive(Debug)]
pub struct OperatorAnd();

impl<'a> ParseExpression<'a> for ExpAnd<'a> {
    type Child = ExpOrd<'a>;
    type Operator = OperatorAnd;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
    }
}

impl ParseOperator for OperatorAnd {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = tag("&&")(s)?;
        Ok((s, OperatorAnd()))
    }
}


#[derive(Debug)]
pub struct ExpOrd<'a> {
    pub terms: Vec<ExpBitOr<'a>>,
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

impl<'a> ParseExpression<'a> for ExpOrd<'a> {
    type Child = ExpBitOr<'a>;
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
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
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
pub struct ExpBitOr<'a> {
    pub terms: Vec<ExpBitXor<'a>>,
    pub opes: Vec<OperatorBitOr>,
}

#[derive(Debug)]
pub struct OperatorBitOr();

impl<'a> ParseExpression<'a> for ExpBitOr<'a> {
    type Child = ExpBitXor<'a>;
    type Operator = OperatorBitOr;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
    }
}

impl ParseOperator for OperatorBitOr {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('|')(s)?;
        Ok((s, OperatorBitOr()))
    }
}

#[derive(Debug)]
pub struct ExpBitXor<'a> {
    pub terms: Vec<ExpBitAnd<'a>>,
    pub opes: Vec<OperatorBitXor>,
}

#[derive(Debug)]
pub struct OperatorBitXor();

impl<'a> ParseExpression<'a> for ExpBitXor<'a> {
    type Child = ExpBitAnd<'a>;
    type Operator = OperatorBitXor;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
    }
}

impl ParseOperator for OperatorBitXor {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('^')(s)?;
        Ok((s, OperatorBitXor()))
    }
}

#[derive(Debug)]
pub struct ExpBitAnd<'a> {
    pub terms: Vec<ExpShift<'a>>,
    pub opes: Vec<OperatorBitAnd>,
}

#[derive(Debug)]
pub struct OperatorBitAnd();

impl<'a> ParseExpression<'a> for ExpBitAnd<'a> {
    type Child = ExpShift<'a>;
    type Operator = OperatorBitAnd;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
    }
}

impl ParseOperator for OperatorBitAnd {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('&')(s)?;
        Ok((s, OperatorBitAnd()))
    }
}

#[derive(Debug)]
pub struct ExpShift<'a> {
    pub terms: Vec<ExpAddSub<'a>>,
    pub opes: Vec<OperatorShift>,
}

#[derive(Debug)]
pub enum OperatorShift {
    Shl,
    Shr,
}

impl<'a> ParseExpression<'a> for ExpShift<'a> {
    type Child = ExpAddSub<'a>;
    type Operator = OperatorShift;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
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
pub struct ExpAddSub<'a> {
    pub terms: Vec<ExpMulDevRem<'a>>,
    pub opes: Vec<OperatorAddSub>,
}

#[derive(Debug)]
pub enum OperatorAddSub {
    Add,
    Sub,
}

impl<'a> ParseExpression<'a> for ExpAddSub<'a> {
    type Child = ExpMulDevRem<'a>;
    type Operator = OperatorAddSub;
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
        default_parse_expression::<'a, Self>(s)
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
pub struct ExpMulDevRem<'a> {
    pub unary_exprs: Vec<UnaryExpr<'a>>,
    pub opes: Vec<OperatorMulDevRem>,
}

#[derive(Debug)]
pub enum OperatorMulDevRem {
    Mul,
    Div,
    Rem
}

impl<'a> ParseExpression<'a> for ExpMulDevRem<'a> {
    type Child = UnaryExpr<'a>;
    type Operator = OperatorMulDevRem;
    fn new_expr(unary_exprs: Vec<Self::Child>, opes: Vec<Self::Operator>) -> Self {
        Self { unary_exprs, opes }
    }
    fn parse_expression(s: &'a str) -> IResult<&'a str, Self> {
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
