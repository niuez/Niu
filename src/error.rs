pub mod range;
pub mod comment;
pub mod details;
pub mod unify;

use std::collections::HashMap;

pub use range::*;
pub use comment::*;
pub use details::*;
pub use unify::*;

pub struct ErrorData<'a> {
    pub programs: &'a HashMap<usize, String>,
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
    Unify(ErrorUnify),
    None,
}

impl NiuError for Error {
    fn what(&self, data: &ErrorData) -> String {
        match *self {
            Self::Hint(ref a) => a.what(data),
            Self::Comment(ref a) => a.what(data),
            Self::Details(ref a) => a.what(data),
            Self::Unify(ref a) => a.what(data),
            Self::None => format!(""),
        }
    }
}
