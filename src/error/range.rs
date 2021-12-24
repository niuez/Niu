use nom::*;
use nom::error::ParseError;

use crate::error::*;
use crate::content_str::*;

#[derive(Debug, Clone)]
pub struct SourceRange {
    start: usize,
    end: usize,
    name: usize,
}
pub fn with_range<'a, O, E, P>(mut p: P) -> impl FnMut(ContentStr<'a>) -> IResult<ContentStr<'a>, (O, SourceRange), E>
where E: ParseError<ContentStr<'a>>,
      P: Parser<ContentStr<'a>, O, E> {
          move |s| {
              let start = s.s.len();
              let name = s.name;
              let (ss, o) = p.parse(s)?;
              let range = SourceRange { start, end: ss.s.len(), name, };
              Ok((ss, (o, range)))
          }
      }

impl SourceRange {
    pub fn empty() -> Self {
        SourceRange { start: 0, end: 0, name: 0 }
    }
    pub fn get_range_str<'a>(&self, s: &'a str) -> &'a str {
        s.get((s.len() - self.start)..(s.len() - self.end)).unwrap()
    }
    pub fn get_start_line_number(&self, s: &str) -> usize {
        s.get(0..=(s.len() - self.start)).unwrap().lines().count()
    }
    pub fn hint(&self, hint: &str, prev: ErrorHint) -> ErrorHint {
        RangeHint::new(self.clone(), hint, prev)
    }
    pub fn merge(&self, right: &SourceRange) -> SourceRange {
        SourceRange { start: self.start, end: right.end, name: self.name, }
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
        let start_line_num = self.range.get_start_line_number(data.statement);
        let code = self.range.get_range_str(data.statement).lines()
            .enumerate()
            .map(|(i, s)| format!("{:04} |     {}", i + start_line_num, s)).collect::<Vec<_>>().join("\n");
        format!("{}\nhint: {}\n     |\n{}\n     |", self.prev.as_ref().what(data), self.hint, code)
    }
}
