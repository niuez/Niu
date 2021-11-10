use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorComment {
    comment: String,
    prev: Box<Error>,
}

impl ErrorComment {
    pub fn empty(comment: String) -> Error {
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

impl NiuError for ErrorComment {
    fn what(&self, data: &ErrorData) -> String {
        format!("{}\nerror: {}", self.prev.as_ref().what(data), self.comment)
    }
}

