use nom::*;
use nom::error::ParseError;

use crate::error::*;

#[derive(Debug, Clone)]
pub struct SourceRange {
    start: usize,
    end: usize,
}
pub fn with_range<'a, O, E, P>(mut p: P) -> impl FnMut(&'a str) -> IResult<&'a str, (O, SourceRange), E>
where E: ParseError<&'a str>,
      P: Parser<&'a str, O, E> {
          move |s| {
              let (ss, o) = p.parse(s)?;
              let range = SourceRange { start: s.len(), end: ss.len() };
              Ok((ss, (o, range)))
          }
      }

impl SourceRange {
    pub fn get_range_str<'a>(&self, s: &'a str) -> &'a str {
        s.get((s.len() - self.start)..(s.len() - self.end)).unwrap()
    }
    pub fn hint(&self, hint: &str, prev: ErrorHint) -> ErrorHint {
        RangeHint::new(self.clone(), hint, prev)
    }
}

#[derive(Debug, Clone)]
pub struct RangeHint {
    range: SourceRange,
    hint: String,
    prev: Box<ErrorHint>,
}

impl RangeHint {
    pub fn new(range: SourceRange, hint: &str, prev: ErrorHint) -> ErrorHint {
        ErrorHint::Range(Self { range, hint: hint.to_string(), prev: Box::new(prev) })
    }
}


impl NiuError for RangeHint {
    fn what(&self, data: &ErrorData) -> String {
        format!("hint: {}\n    | {}\n{}", self.hint, self.range.get_range_str(data.statement).to_string(), self.prev.as_ref().what(data))
    }
}
