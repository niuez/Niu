use crate::trans::TypeAnnotation;

trait MutCheck {
    fn mut_check(&self, ta: &TypeAnnotation) -> Result<(), String>;
}
