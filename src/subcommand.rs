pub mod unit_test;
pub mod generate;
pub mod tester;
pub mod document;

use std::path::*;

pub fn create_test_directory(library_dir: &Path) -> Result<(), String> {
    let test_dir = library_dir.join(".test");
    if !test_dir.exists() {
        std::fs::create_dir(test_dir)
            .map_err(|e| format!("failure to create .test, {:?}", e))?;
    }
    Ok(())
}
