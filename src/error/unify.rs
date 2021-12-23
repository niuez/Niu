use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorUnify<'a> {
    comment: String,
    hint: ErrorHint<'a>,
    err: Box<Error<'a>>,
}

impl<'a> ErrorUnify<'a> {
    pub fn new(comment: String, hint: ErrorHint, err: Error) -> Error<'a> {
        Error::Unify(ErrorUnify {
            comment,
            hint,
            err: Box::new(err),
        })
    }
}

impl<'a> NiuError for ErrorUnify<'a> {
    fn what(&self, data: &ErrorData) -> String {
        format!("unify error: {} @\n{}\n{}", self.comment, self.hint.what(data), self.err.as_ref().what(data))
    }
}

