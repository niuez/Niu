use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorComment {
    comment: String,
    err: Box<Error>,
}

impl ErrorComment {
    pub fn empty(comment: String) -> Error {
        Error::Comment(ErrorComment {
            comment,
            err: Box::new(Error::None),
        })
    }
    pub fn new(comment: String, err: Error) -> Error {
        Error::Comment(ErrorComment {
            comment,
            err: Box::new(err),
        })
    }
}

impl NiuError for ErrorComment {
    fn what(&self, data: &ErrorData) -> String {
        format!("{}\n", self.comment)
    }
}

