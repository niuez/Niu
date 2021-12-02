use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorUnify {
    comment: String,
    hint: ErrorHint,
    err: Box<Error>,
}

impl ErrorUnify {
    pub fn new(comment: String, hint: ErrorHint, err: Error) -> Error {
        Error::Unify(ErrorUnify {
            comment,
            hint,
            err: Box::new(err),
        })
    }
}

impl NiuError for ErrorUnify {
    fn what(&self, data: &ErrorData) -> String {
        format!("unify error: {} @\n{}\n{}", self.comment, self.hint.what(data), self.err.as_ref().what(data))
    }
}

