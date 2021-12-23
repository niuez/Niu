use nom::*;
use nom::error::ParseError;
use nom::combinator::*;

use crate::error::*;

#[derive(Debug, Clone)]
pub struct SourceRange<'a> {
    start: usize,
    end: usize,
    source: &'a str,
}
pub fn with_range<'a, O, E, P>(mut p: P) -> impl FnMut(&'a str) -> IResult<&'a str, (O, SourceRange), E>
where E: ParseError<&'a str>,
      P: Parser<&'a str, O, E> {
          move |s| {
              let (ss, (source, res)) = consumed(p)(s);
              let range = SourceRange { start: s.len(), end: ss.len(), source };
              (ss, (res, source))
          }
      }

impl<'a> SourceRange<'a> {
    pub fn get_start_line_number(&self, s: &str) -> usize {
        s.get(0..=(s.len() - self.start)).unwrap().lines().count()
    }
    pub fn hint(&self, hint: &str, prev: ErrorHint) -> ErrorHint {
        RangeHint::new(self.clone(), hint, prev)
    }
    pub fn merge(&self, right: &SourceRange) -> SourceRange {
        //SourceRange { start: self.start, end: right.end }
        right.clone()
    }
}

#[derive(Debug, Clone)]
pub struct RangeHint<'a> {
    range: SourceRange<'a>,
    hint: String,
    prev: Box<ErrorHint>,
}

impl<'a> RangeHint<'a> {
    pub fn new(range: SourceRange, hint: &str, prev: ErrorHint) -> ErrorHint {
        ErrorHint::Range(Self { range, hint: hint.to_string(), prev: Box::new(prev) })
    }
}


impl<'a> NiuError for RangeHint<'a> {
    fn what(&self, data: &ErrorData) -> String {
        let start_line_num = self.range.get_start_line_number(data.statement);
        let code = self.range.get_range_str(data.statement).lines()
            .enumerate()
            .map(|(i, s)| format!("{:04} |     {}", i + start_line_num, s)).collect::<Vec<_>>().join("\n");
        format!("{}\nhint: {}\n     |\n{}\n     |", self.prev.as_ref().what(data), self.hint, code)
    }
}
