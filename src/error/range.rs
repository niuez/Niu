use nom::*;
use nom::error::ParseError;

use crate::error::*;

#[derive(Debug)]
pub struct SourceRange {
    start: usize,
    end: usize,
}

impl SourceRange {
    pub fn parse_with_range<'a, O, E, P>(s: &'a str, mut p: P) -> IResult<&'a str, (O, SourceRange), E>
        where E: ParseError<&'a str>,
              P: Parser<&'a str, O, E> {
        let (ss, o) = p.parse(s)?;
        let range = SourceRange { start: s.len(), end: ss.len() };
        Ok((ss, (o, range)))
    }
    pub fn get_range_str<'a>(&self, s: &'a str) -> &'a str {
        s.get((s.len() - self.start)..(s.len() - self.end)).unwrap()
    }
}

#[derive(Debug)]
pub struct ErrorRange {
    range: SourceRange,
    err: Box<dyn NiuError>,
}

impl ErrorRange {
    pub fn boxed(range: SourceRange, err: Box<dyn NiuError>) -> Box<Self> {
        Box::new(Self { range, err })
    }
}


impl NiuError for ErrorRange {
    fn what(&self, data: &ErrorData) -> String {
        format!("{}\n{}", self.range.get_range_str(data.statement).to_string(), self.err.as_ref().what(data))
    }
}
