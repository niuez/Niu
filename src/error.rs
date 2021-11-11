pub mod range;
pub mod comment;
pub mod details;

pub use range::*;
pub use comment::*;
pub use details::*;

pub struct ErrorData<'a> {
    pub statement: &'a str,
}

pub trait NiuError: std::fmt::Debug {
    fn what(&self, data: &ErrorData) -> String;
}

#[derive(Debug, Clone)]
pub enum ErrorHint {
    Range(RangeHint),
    None,
}

impl ErrorHint {
    pub fn err(self) -> Error {
        Error::Hint(self)
    }
}

impl NiuError for ErrorHint {
    fn what(&self, data: &ErrorData) -> String {
        match *self {
            Self::Range(ref a) => a.what(data),
            Self::None => format!(""),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    Hint(ErrorHint),
    Comment(ErrorComment),
    Details(ErrorDetails),
    None,
}

impl NiuError for Error {
    fn what(&self, data: &ErrorData) -> String {
        match *self {
            Self::Hint(ref a) => a.what(data),
            Self::Comment(ref a) => a.what(data),
            Self::Details(ref a) => a.what(data),
            Self::None => format!(""),
        }
    }
}
