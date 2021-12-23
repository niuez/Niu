use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorComment<'a> {
    comment: String,
    prev: Box<Error<'a>>,
}

impl<'a> ErrorComment<'a> {
    pub fn empty(comment: String) -> Error<'a> {
        Error::Comment(ErrorComment {
            comment,
            prev: Box::new(Error::None),
        })
    }
    pub fn new(comment: String, prev: Error) -> Error {
        Error::Comment(ErrorComment {
            comment,
            prev: Box::new(prev),
        })
    }
}

impl<'a> NiuError for ErrorComment<'a> {
    fn what(&self, data: &ErrorData) -> String {
        format!("{}\nerror: {}", self.prev.as_ref().what(data), self.comment)
    }
}

