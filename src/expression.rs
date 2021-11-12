pub mod if_expr;
pub mod for_expr;
pub mod for_each_expr;

//use nom::branch::*;
use nom::IResult;
use nom::character::complete::*;
use nom::multi::*;
use nom::sequence::*; 
use nom::bytes::complete::*;
use nom::branch::*;

use crate::unary_expr::{ UnaryExpr, parse_unary_expr };
use crate::identifier::*;
use crate::traits::*;
use crate::unify::*;
use crate::trans::*;
use crate::mut_checker::*;
use crate::move_checker::*;
use crate::error::*;

pub use if_expr::*;
pub use for_expr::*;
pub use for_each_expr::*;

fn expr_gen_type<'a, EI: Iterator<Item=Type>, O: 'a, OI: Iterator<Item=&'a O>, F: Fn(&O) -> (&'static str, &'static str)>
(equs: &mut TypeEquations, mut exprs: EI, opes: OI, f: F, tag: Tag) -> TResult {
    let ty = exprs.next().unwrap();
    let mut left = ty;
    for (cnt, (right, ope)) in exprs.zip(opes).enumerate() {
        let (tr, method) = f(ope);
        let tr = TraitId::from_str(tr);
        let method = Identifier::from_str(method);
        let next_ty = Type::CallEquation( CallEquation {
            caller_type: None,
            trait_gen: Some(TraitGenerics { trait_id: tr, generics: vec![right.clone()] }),
            func_id: method,
            args: vec![left, right],
            caller_range: ErrorHint::None,
            tag: Tag::new()
        });
        left = tag.generate_type_variable("Operators", cnt, equs);
        equs.add_equation(next_ty, left.clone());
    }
    Ok(left)
}

#[derive(Debug)]
pub enum Expression {
    IfExpr(Box<IfExpr>),
    ForExpr(Box<ForExpr>),
    ForEachExpr(Box<ForEachExpr>),
    Expression(ExpOr),
}

impl GenType for Expression {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match *self {
            Expression::Expression(ref e) => e.gen_type(equs, trs),
            Expression::IfExpr(ref ifexpr) => ifexpr.as_ref().gen_type(equs, trs),
            Expression::ForExpr(ref forexpr) => forexpr.as_ref().gen_type(equs, trs),
            Expression::ForEachExpr(ref foreach) => foreach.as_ref().gen_type(equs, trs),
        }
    }
}

impl Transpile for Expression {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match *self {
            Expression::Expression(ref e) => e.transpile(ta),
            Expression::IfExpr(ref ifexpr) => ifexpr.as_ref().transpile(ta),
            Expression::ForExpr(ref forexpr) => forexpr.as_ref().transpile(ta),
            Expression::ForEachExpr(ref foreach) => foreach.as_ref().transpile(ta),
        }
    }
}

impl MutCheck for Expression {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        match *self {
            Expression::Expression(ref e) => e.mut_check(ta, vars),
            Expression::IfExpr(ref ifexpr) => ifexpr.as_ref().mut_check(ta, vars),
            Expression::ForExpr(ref forexpr) => forexpr.as_ref().mut_check(ta, vars),
            Expression::ForEachExpr(ref foreach) => foreach.as_ref().mut_check(ta, vars),
        }
    }
}

impl MoveCheck for Expression {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        match *self {
            Expression::Expression(ref e) => e.move_check(mc, ta),
            Expression::IfExpr(ref ifexpr) => ifexpr.as_ref().move_check(mc, ta),
            Expression::ForExpr(ref forexpr) => forexpr.as_ref().move_check(mc, ta),
            Expression::ForEachExpr(ref foreach) => foreach.as_ref().move_check(mc, ta),
        }
    }
}

fn default_parse_expression<P: ParseExpression>(s: &str) -> IResult<&str, P>
where
    P::Child: ParseExpression,
    P::Operator: ParseOperator,
{
    let (s, ((head, _, tails), range)) = 
        with_range(tuple((P::Child::parse_expression, multispace0, many0(tuple((P::Operator::parse_operator, multispace0, P::Child::parse_expression, multispace0))))))(s)?;
    let mut terms = vec![head];
    let mut opes = Vec::new();

    for (ope, _, term, _) in tails {
        terms.push(term);
        opes.push(ope);
    }
    Ok((s, P::new_expr(terms, opes, range)))
}

trait ParseExpression: Sized {
    type Child: Sized;
    type Operator: Sized;
    fn new_expr(childs: Vec<Self::Child>, opes: Vec<Self::Operator>, range: SourceRange) -> Self;
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
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

impl MutCheck for ExpOr {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpOr {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
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

impl MutCheck for ExpAnd {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpAnd {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}

#[derive(Debug)]
pub struct ExpOrd {
    pub terms: Vec<ExpBitOr>,
    pub ope: Option<OperatorOrd>,
    range: SourceRange
}

impl GenType for ExpOrd {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match self.ope {
            Some(OperatorOrd::Equal) | Some(OperatorOrd::NotEq) => {
                let t0 = self.terms[0].gen_type(equs, trs)?;
                let t1 = self.terms[1].gen_type(equs, trs)?;
                equs.add_has_trait(t0.clone(), TraitGenerics {
                    trait_id: TraitId { id: Identifier::from_str("Eq") },
                    generics: Vec::new(),
                }, self.range.hint("ord operator", ErrorHint::None));
                equs.add_equation(t0, t1);
                Ok(Type::from_str("bool"))
            }
            Some(_) => {
                let t0 = self.terms[0].gen_type(equs, trs)?;
                let t1 = self.terms[1].gen_type(equs, trs)?;
                equs.add_has_trait(t0.clone(), TraitGenerics {
                    trait_id: TraitId { id: Identifier::from_str("Ord") },
                    generics: Vec::new(),
                }, self.range.hint("ord operator", ErrorHint::None));
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
            Some(ref o) => {
                let left = self.terms[0].transpile(ta);
                let right = self.terms[1].transpile(ta);
                let left = if self.terms[0].has_bit_operator() { format!("({})", left) }
                           else { left };
                let right = if self.terms[1].has_bit_operator() { format!("({})", right) }
                           else { right };
                format!("{} {} {}", left, o.transpile(ta), right)
            }
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
    fn new_expr(terms: Vec<Self::Child>, mut opes: Vec<Self::Operator>, range: SourceRange) -> Self {
        if terms.len() == 1 && opes.len() == 0 {
            Self { terms, ope: None, range }
        }
        else if terms.len() == 2 && opes.len() == 1 {
            Self { terms, ope: Some(opes.remove(0)), range }
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

impl MutCheck for ExpOrd {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpOrd {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let _ = term.move_check(mc, ta)?;
            }
            Ok(MoveResult::Right)
        }
    }
}

#[derive(Debug)]
pub struct ExpBitOr {
    pub terms: Vec<ExpBitXor>,
    pub opes: Vec<OperatorBitOr>,
}

impl ExpBitOr {
    fn has_bit_operator(&self) -> bool {
        self.opes.len() > 0 || self.terms.iter().map(|t| t.has_bit_operator()).any(|b| b)
    }
}

impl GenType for ExpBitOr {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let exprs = self.terms.iter().map(|e| e.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
        expr_gen_type(equs, exprs.into_iter(), self.opes.iter(), |ope| match *ope {
                OperatorBitOr() => ("BitOr", "operator|"),
            }, Tag::new())
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
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

impl MutCheck for ExpBitOr {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpBitOr {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}

#[derive(Debug)]
pub struct ExpBitXor {
    pub terms: Vec<ExpBitAnd>,
    pub opes: Vec<OperatorBitXor>,
}

impl ExpBitXor {
    fn has_bit_operator(&self) -> bool {
        self.opes.len() > 0 || self.terms.iter().map(|t| t.has_bit_operator()).any(|b| b)
    }
}

impl GenType for ExpBitXor {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let exprs = self.terms.iter().map(|e| e.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
        expr_gen_type(equs, exprs.into_iter(), self.opes.iter(), |ope| match *ope {
                OperatorBitXor() => ("BitXor", "operator^"),
            }, Tag::new())
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
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

impl MutCheck for ExpBitXor {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpBitXor {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}
#[derive(Debug)]
pub struct ExpBitAnd {
    pub terms: Vec<ExpShift>,
    pub opes: Vec<OperatorBitAnd>,
}


impl ExpBitAnd {
    fn has_bit_operator(&self) -> bool {
        self.opes.len() > 0
    }
}

impl GenType for ExpBitAnd {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let exprs = self.terms.iter().map(|e| e.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
        expr_gen_type(equs, exprs.into_iter(), self.opes.iter(), |ope| match *ope {
                OperatorBitAnd() => ("BitAnd", "operator&"),
            }, Tag::new())
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
        Self { terms, opes }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        default_parse_expression::<Self>(s)
    }
}

impl ParseOperator for OperatorBitAnd {
    fn parse_operator(s: &str) -> IResult<&str, Self> {
        let (s, _) = char('&')(s)?;
        let (_, _) = none_of("&")(s)?;
        Ok((s, OperatorBitAnd()))
    }
}

impl MutCheck for ExpBitAnd {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpBitAnd {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}
#[derive(Debug)]
pub struct ExpShift {
    pub terms: Vec<ExpAddSub>,
    pub opes: Vec<OperatorShift>,
}

impl GenType for ExpShift {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let exprs = self.terms.iter().map(|e| e.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
        expr_gen_type(equs, exprs.into_iter(), self.opes.iter(), |ope| match *ope {
                OperatorShift::Shl => ("Shl", "operator<<"),
                OperatorShift::Shr => ("Shr", "operator>>"),
            }, Tag::new())
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
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

impl MutCheck for ExpShift {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpShift {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}
#[derive(Debug)]
pub struct ExpAddSub {
    pub terms: Vec<ExpMulDivRem>,
    pub opes: Vec<OperatorAddSub>,
}

impl GenType for ExpAddSub {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let exprs = self.terms.iter().map(|e| e.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
        expr_gen_type(equs, exprs.into_iter(), self.opes.iter(), |ope| match *ope {
                OperatorAddSub::Add => ("Add", "operator+"),
                OperatorAddSub::Sub => ("Sub", "operator-"),
            }, Tag::new())
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
    fn new_expr(terms: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
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

impl MutCheck for ExpAddSub {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.terms.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpAddSub {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.terms.len() == 1 {
            self.terms.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.terms.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}

#[derive(Debug)]
pub struct ExpMulDivRem {
    pub unary_exprs: Vec<ExpUnaryOpe>,
    pub opes: Vec<OperatorMulDivRem>,
    pub tag: Tag,
}

impl GenType for ExpMulDivRem {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        let exprs = self.unary_exprs.iter().map(|e| e.gen_type(equs, trs)).collect::<Result<Vec<_>, _>>()?;
        expr_gen_type(equs, exprs.into_iter(), self.opes.iter(), |ope| match *ope {
                OperatorMulDivRem::Mul => ("Mul", "operator*"),
                OperatorMulDivRem::Div => ("Div", "operator/"),
                OperatorMulDivRem::Rem => ("Rem", "operator%"),
            }, self.tag.clone())
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
    type Child = ExpUnaryOpe;
    type Operator = OperatorMulDivRem;
    fn new_expr(unary_exprs: Vec<Self::Child>, opes: Vec<Self::Operator>, _range: SourceRange) -> Self {
        Self { unary_exprs, opes, tag: Tag::new(), }
    }
    fn parse_expression(s: &str) -> IResult<&str, Self> {
        let (s, (head, _, tails)) = 
            tuple((parse_exp_unary_ope, multispace0, many0(tuple((Self::Operator::parse_operator, multispace0, parse_exp_unary_ope, multispace0)))))(s)?;
        let mut unary_exprs = vec![head];
        let mut opes = Vec::new();

        for (ope, _, expr, _) in tails {
            unary_exprs.push(expr);
            opes.push(ope);
        }
        Ok((s, ExpMulDivRem { unary_exprs, opes, tag: Tag::new() }))
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

impl MutCheck for ExpMulDivRem {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        if self.unary_exprs.len() == 1 {
            self.unary_exprs.last().unwrap().mut_check(ta, vars)
        }
        else {
            for term in self.unary_exprs.iter() {
                term.mut_check(ta, vars)?;
            }
            Ok(MutResult::NotMut)
        }
    }
}

impl MoveCheck for ExpMulDivRem {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        if self.unary_exprs.len() == 1 {
            self.unary_exprs.last().unwrap().move_check(mc, ta)
        }
        else {
            for term in self.unary_exprs.iter() {
                let res = term.move_check(mc, ta)?;
                mc.move_result(res)?;
            }
            Ok(MoveResult::Right)
        }
    }
}

#[derive(Debug)]
pub enum ExpUnaryOpe {
    UnaryExpr(UnaryExpr),
    Ref(Box<ExpUnaryOpe>),
    MutRef(Box<ExpUnaryOpe>),
    Deref(Box<ExpUnaryOpe>, Tag),
    Neg(Box<ExpUnaryOpe>, Tag),
    Not(Box<ExpUnaryOpe>, Tag),
}

impl GenType for ExpUnaryOpe {
    fn gen_type(&self, equs: &mut TypeEquations, trs: &TraitsInfo) -> TResult {
        match self {
            Self::UnaryExpr(ref exp) => exp.gen_type(equs, trs),
            Self::Ref(ref exp) => Ok(Type::Ref(Box::new(exp.as_ref().gen_type(equs, trs)?))),
            Self::MutRef(ref exp) => Ok(Type::MutRef(Box::new(exp.as_ref().gen_type(equs, trs)?))),
            Self::Deref(ref exp, ref tag) => {
                let alpha = tag.generate_not_void_type_variable("DerefType", 0, equs);
                let right = exp.as_ref().gen_type(equs, trs)?;
                equs.add_equation(alpha.clone(), right);
                equs.regist_check_copyable(tag.clone(), Type::Deref(Box::new(alpha.clone())));
                Ok(Type::Deref(Box::new(alpha)))
            }
            Self::Neg(ref exp, ref tag) => {
                Ok(Type::CallEquation( CallEquation {
                    caller_type: None,
                    trait_gen: Some(TraitGenerics { trait_id: TraitId { id: Identifier::from_str("Neg") } , generics: Vec::new() }),
                    func_id: Identifier::from_str("operator-"),
                    args: vec![exp.gen_type(equs, trs)?],
                    caller_range: ErrorHint::None,
                    tag: tag.clone(),
                }))
            }
            Self::Not(ref exp, ref tag) => {
                Ok(Type::CallEquation( CallEquation {
                    caller_type: None,
                    trait_gen: Some(TraitGenerics { trait_id: TraitId { id: Identifier::from_str("Not") } , generics: Vec::new() }),
                    func_id: Identifier::from_str("operator!"),
                    args: vec![exp.gen_type(equs, trs)?],
                    caller_range: ErrorHint::None,
                    tag: tag.clone(),
                }))
            }
        }
    }
}

impl Transpile for ExpUnaryOpe {
    fn transpile(&self, ta: &TypeAnnotation) -> String {
        match self {
            Self::UnaryExpr(ref exp) => exp.transpile(ta),
            Self::Ref(ref exp) => format!("{}", exp.as_ref().transpile(ta)),
            Self::MutRef(ref exp) => format!("{}", exp.as_ref().transpile(ta)),
            Self::Deref(ref exp, _) => format!("{}", exp.as_ref().transpile(ta)),
            Self::Neg(ref exp, _) => format!("-{}", exp.as_ref().transpile(ta)),
            Self::Not(ref exp, _) => format!("!{}", exp.as_ref().transpile(ta)),
        }
    }
}

impl MutCheck for ExpUnaryOpe {
    fn mut_check(&self, ta: &TypeAnnotation, vars: &mut VariablesInfo) -> Result<MutResult, String> {
        match self {
            Self::UnaryExpr(ref exp) => exp.mut_check(ta, vars),
            Self::Ref(ref exp) => {
                exp.mut_check(ta, vars)?;
                Ok(MutResult::NotMut)
            }
            Self::MutRef(ref exp) => {
                match exp.mut_check(ta, vars)? {
                    MutResult::Mut => Ok(MutResult::NotMut),
                    _ => Err(format!("expr {:?} is needed mutable", exp)),
                }
            }
            Self::Deref(ref exp, ref tag) => {
                exp.mut_check(ta, vars)?;
                let deref_ty = ta.annotation(tag.get_num(), "DerefType", 0);
                match deref_ty {
                    Type::MutRef(_) => Ok(MutResult::Mut),
                    _ => Ok(MutResult::NotMut),
                }
            }
            Self::Neg(ref exp, _) => {
                exp.mut_check(ta, vars)?;
                Ok(MutResult::NotMut)
            }
            Self::Not(ref exp, _) => {
                exp.mut_check(ta, vars)?;
                Ok(MutResult::NotMut)
            }
        }
    }
}

impl MoveCheck for ExpUnaryOpe {
    fn move_check(&self, mc: &mut VariablesMoveChecker, ta: &TypeAnnotation) -> Result<MoveResult, String> {
        match self {
            Self::UnaryExpr(ref exp) => exp.move_check(mc, ta),
            Self::Ref(ref exp) => {
                exp.move_check(mc, ta)?;
                Ok(MoveResult::Right)
            }
            Self::MutRef(ref exp) => {
                exp.move_check(mc, ta)?;
                Ok(MoveResult::Right)
            }
            Self::Deref(ref exp, ref tag) => {
                exp.move_check(mc, ta)?;
                if ta.is_copyable(tag) {
                    Ok(MoveResult::Right)
                }
                else {
                    Ok(MoveResult::Deref)
                }
            }
            Self::Neg(ref exp, _) => {
                exp.move_check(mc, ta)?;
                Ok(MoveResult::Right)
            }
            Self::Not(ref exp, _) => {
                exp.move_check(mc, ta)?;
                Ok(MoveResult::Right)
            }
        }
    }
}

pub fn parse_exp_unary_ope_ref(s: &str) -> IResult<&str, ExpUnaryOpe> {
    let (s, (_, _, exp)) = tuple((char('&'), multispace0, parse_exp_unary_ope))(s)?;
    Ok((s, ExpUnaryOpe::Ref(Box::new(exp))))
}
pub fn parse_exp_unary_ope_mutref(s: &str) -> IResult<&str, ExpUnaryOpe> {
    let (s, (_, _, exp)) = tuple((tag("&mut"), multispace0, parse_exp_unary_ope))(s)?;
    Ok((s, ExpUnaryOpe::MutRef(Box::new(exp))))
}

pub fn parse_exp_unary_ope_deref(s: &str) -> IResult<&str, ExpUnaryOpe> {
    let (s, (_, _, exp)) = tuple((char('*'), multispace0, parse_exp_unary_ope))(s)?;
    Ok((s, ExpUnaryOpe::Deref(Box::new(exp), Tag::new())))
}

pub fn parse_exp_unary_ope_neg(s: &str) -> IResult<&str, ExpUnaryOpe> {
    let (s, (_, _, exp)) = tuple((char('-'), multispace0, parse_exp_unary_ope))(s)?;
    Ok((s, ExpUnaryOpe::Neg(Box::new(exp), Tag::new())))
}

pub fn parse_exp_unary_ope_not(s: &str) -> IResult<&str, ExpUnaryOpe> {
    let (s, (_, _, exp)) = tuple((char('!'), multispace0, parse_exp_unary_ope))(s)?;
    Ok((s, ExpUnaryOpe::Not(Box::new(exp), Tag::new())))
}

pub fn parse_exp_unary_ope_unary_exp(s: &str) -> IResult<&str, ExpUnaryOpe> {
    let (s, exp) = parse_unary_expr(s)?;
    Ok((s, ExpUnaryOpe::UnaryExpr(exp)))
}

pub fn parse_exp_unary_ope(s: &str) -> IResult<&str, ExpUnaryOpe> {
    alt((parse_exp_unary_ope_mutref, parse_exp_unary_ope_ref, parse_exp_unary_ope_deref, parse_exp_unary_ope_neg, parse_exp_unary_ope_not, parse_exp_unary_ope_unary_exp))(s)
}




pub fn parse_expression(s: &str) -> IResult<&str, Expression> {
    let (s, expr) = alt((parse_if_expr, parse_for_expr, parse_expor))(s)?;
    Ok((s, expr))
}

pub fn parse_expor(s: &str) -> IResult<&str, Expression> {
    let (s, p) = ExpOr::parse_expression(s)?;
    Ok((s, Expression::Expression(p)))
}


#[test]
fn parse_expression_test() {
    println!("{:?}", parse_expression("1 + 2 - 3 + 4 - 5").ok());
    println!("{:?}", parse_expression("func(1 + 2, 3 - 4)").ok());
    println!("{:#?}", parse_expression("1 + 2 * 3 - 4 / 5").ok());
    println!("{:#?}", parse_expression("(1 + 2) * (3 - 4) / 5").ok());
}

#[test]
fn parse_bit_test() {
    println!("{:#?}", parse_expression("1 & 2 | 3 ^ 4").ok());
}
#[test]
fn parse_conditions_test() {
    println!("{:?}", parse_expression("1 == 2").ok());
    println!("{:?}", parse_expression("1 != 2").ok());
    println!("{:?}", parse_expression("1 < 2").ok());
    println!("{:?}", parse_expression("1 > 2").ok());
    println!("{:?}", parse_expression("1 <= 2").ok());
    println!("{:?}", parse_expression("1 >= 2").ok());
}

#[test]
fn parse_all_test() {
    println!("{:?}", parse_expression("1 + 2 == 3 * 4 || 5 << 6 & 7 >> 8 != 9 | 0 ^ 1 && 2 % 3 < 4 / 5 && 6 > 7 && 8 < 9 || 0 <= 1 && 2 >= 3").ok());
}

#[test]
fn parse_ref_test() {
    println!("{:?}", parse_expression("*var").ok());
    println!("{:?}", parse_expression("&var").ok());
}

#[test]
#[should_panic]
fn parse_conditions_failure_test() {
    println!("{:?}", parse_expression("1 == 2 == 3").ok());
}
