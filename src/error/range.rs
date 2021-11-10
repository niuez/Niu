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
}

#[derive(Debug, Clone)]
pub struct ErrorRange {
    range: SourceRange,
    err: Box<Error>,
}

impl ErrorRange {
    pub fn boxed(range: SourceRange, err: Error) -> Error {
        Error::Range(Self { range, err: Box::new(err) })
    }
}


impl NiuError for ErrorRange {
    fn what(&self, data: &ErrorData) -> String {
        format!("{}\n{}", self.range.get_range_str(data.statement).to_string(), self.err.as_ref().what(data))
    }
}
