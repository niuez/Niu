use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorDetails<'a> {
    comment: String,
    details: Vec<Error<'a>>,
    prev: Box<Error<'a>>,
}

impl<'a> ErrorDetails<'a> {
    pub fn new(comment: String, details: Vec<Error>, prev: Error) -> Error<'a> {
        Error::Details(ErrorDetails {
            comment,
            details,
            prev: Box::new(prev),
        })
    }
}

impl<'a> NiuError for ErrorDetails<'a> {
    fn what(&self, data: &ErrorData) -> String {
        let details = self.details.iter().map(|d| d.what(data)).collect::<Vec<_>>().join("\n");
        format!("{}\ndetail: {} -----\n{}\n-------", self.prev.as_ref().what(data), self.comment, details)
    }
}

