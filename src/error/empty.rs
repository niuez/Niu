use crate::error::*;

#[derive(Debug)]
pub struct ErrorEmpty;

impl ErrorEmpty {
    pub fn boxed() -> Box<Self> {
        Box::new(ErrorEmpty)
    }
}

impl NiuError for ErrorEmpty {
    fn what(&self, _data: &ErrorData) -> String {
        String::new()
    }
}
