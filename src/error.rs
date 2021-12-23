pub mod range;
pub mod comment;
pub mod details;
pub mod unify;

pub use range::*;
pub use comment::*;
pub use details::*;
pub use unify::*;

pub struct ErrorData<'a> {
    pub statement: &'a str,
}

pub trait NiuError: std::fmt::Debug {
    fn what(&self, data: &ErrorData) -> String;
}

#[derive(Debug, Clone)]
pub enum ErrorHint<'a> {
    Range(RangeHint<'a>),
    None,
}

impl<'a> ErrorHint<'a> {
    pub fn err(self) -> Error<'a> {
        Error::Hint(self)
    }
}

impl<'a> NiuError for ErrorHint<'a> {
    fn what(&self, data: &ErrorData) -> String {
        match *self {
            Self::Range(ref a) => a.what(data),
            Self::None => format!(""),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error<'a> {
    Hint(ErrorHint<'a>),
    Comment(ErrorComment<'a>),
    Details(ErrorDetails<'a>),
    Unify(ErrorUnify<'a>),
    None,
}

impl<'a> NiuError for Error<'a> {
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
