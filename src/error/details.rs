use crate::error::*;

#[derive(Debug, Clone)]
pub struct ErrorDetails {
    comment: String,
    details: Vec<Error>,
    prev: Box<Error>,
}

impl ErrorDetails {
    pub fn new(comment: String, details: Vec<Error>, prev: Error) -> Error {
        Error::Details(ErrorDetails {
            comment,
            details,
            prev: Box::new(prev),
        })
    }
}

impl NiuError for ErrorDetails {
    fn what(&self, data: &ErrorData) -> String {
        let details = self.details.iter().map(|d| d.what(data)).collect::<Vec<_>>().join("\n");
        format!("{}\ndetail: {} -----\n{}\n-------", self.prev.as_ref().what(data), self.comment, details)
    }
}

