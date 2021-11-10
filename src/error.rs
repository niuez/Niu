pub mod range;
pub mod comment;

pub use range::*;
pub use comment::*;

pub struct ErrorData<'a> {
    pub statement: &'a str,
}

pub trait NiuError: std::fmt::Debug {
    fn what(&self, data: &ErrorData) -> String;
}

#[derive(Debug, Clone)]
pub enum Error {
    Range(RangeHint),
    Comment(ErrorComment),
    None,
}

impl NiuError for Error {
    fn what(&self, data: &ErrorData) -> String {
        match *self {
            Self::Range(ref a) => a.what(data),
            Self::Comment(ref a) => a.what(data),
            Self::None => format!(""),
        }
    }
}
