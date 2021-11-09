use crate::error::*;

pub struct ErrorComment {
    comment: String,
}

impl ErrorComment {
    pub fn boxed(comment: String) -> Box<Self> {
        Box::new(ErrorComment {
            comment,
        })
    }
}

impl NiuError for ErrorComment {
    fn what(&self, data: &ErrorData) -> String {
        format!("{}\n", self.comment)
    }
}

