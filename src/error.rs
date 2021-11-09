pub mod range;
pub mod comment;
pub mod empty;

pub use range::*;
pub use comment::*;
pub use empty::*;

pub struct ErrorData<'a> {
    pub statement: &'a str,
}

pub trait NiuError {
    fn what(&self, data: &ErrorData) -> String;
}

impl<'a, E: NiuError + 'a> From<Box<E>> for Box<dyn NiuError + 'a> {
    fn from(t: Box<E>) -> Box<dyn NiuError + 'a> {
        t
    }
}

