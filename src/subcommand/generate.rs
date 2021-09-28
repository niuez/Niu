use std::path::*;
use std::io::Write;

use crate::unit_test::UnitTestTranspiled;
use crate::subcommand::unit_test::*;

fn get_targets_list_rec(dir: &Path, list: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            get_targets_list_rec(&path, list)?;
        }
        else if path.extension().map_or(false, |ext| ext == "niu") {
            list.push(path.to_path_buf())
        }
    }
    Ok(())
}


fn get_targets_list(library_dir: &Path) -> Result<Vec<PathBuf>, String> {
    let mut list = Vec::new();
    get_targets_list_rec(library_dir, &mut list)
        .map_err(|e| format!("searching targets error {:?}", e))?;
    Ok(list)
}

fn generate_test(library_dir: &Path, trans_file: &Path, unit_test: UnitTestTranspiled) -> Result<Tests, String> {
    let UnitTestTranspiled { program, problem, test_name, tester } = unit_test;
    log::info!("generate test {}", test_name.into_string());
    let test_file = library_dir.join(".test").join(&test_name.into_string()).with_extension("cpp");
    let trans_file = Path::new("..").join(trans_file.strip_prefix(library_dir).unwrap());
    let program = format!("#include <iostream>\n#include \"{}\"\nint main() {{\n{}\n}}", trans_file.to_str().unwrap(), program);
    let mut test_file = std::fs::File::create(&test_file)
        .map_err(|e| format!("failure to create {:?}, {:?}", test_file, e))?;
    test_file.write_all(program.as_bytes())
        .map_err(|e| format!("failure to write {:?}", e))?;
    Ok(Tests {
        program_file: format!("{}.cpp", test_name.into_string()),
        tester,
        problem,
    })
}

fn transpile_target(library_dir: &Path, target: &Path) -> Result<Vec<Tests>, String> {
    log::info!("tranpile {:?}", target);
    let trans_dir = target.parent().unwrap().strip_prefix(library_dir)
        .map_err(|e| format!("strip error {:?}", e))?;
    let trans_dir = library_dir.join("transpile").join(trans_dir);
    let trans_path = trans_dir.join(target.file_stem().unwrap()).with_extension("hpp");


    match crate::type_check_with_tests(target.to_str().unwrap()) {
        Ok((prog, unit_tests)) => {
            std::fs::create_dir_all(&trans_dir)
                .map_err(|e| format!("failure to create dir {:?}, {:?}", trans_dir, e))?;
            let mut trans_file = std::fs::File::create(&trans_path)
                .map_err(|e| format!("failure to create file {:?}, {:?}", trans_path, e))?;
            trans_file.write_all(prog.as_bytes())
                .map_err(|e| format!("failure to write {:?}", e))?;
            unit_tests.into_iter()
                .map(|unit_test| generate_test(library_dir, &trans_path, unit_test))
                .collect::<Result<_, _>>()
        }
        Err(err) => {
            Err(format!("failure to transpile {:?}, {}", target, err))
        }
    }
}

pub fn generate_headers(library_dir: &Path) -> Result<(), String> {
    let lib_conf = super::unit_test::load_library_config(Path::new("."))?;
    let list = get_targets_list(library_dir)?;
    let mut tests = Vec::new();
    for target in list.into_iter() {
        let mut test = transpile_target(library_dir, &target)?;
        tests.append(&mut test);
    }
    if !tests.is_empty() {
        log::info!("generate tests.toml");
        let tests_conf = TestConfig::generate(&lib_conf, &tests);
        let mut tests_conf_file = std::fs::File::create(library_dir.join(".test").join("tests.toml"))
            .map_err(|e| format!("failure to create test.toml {:?}", e))?;
        tests_conf_file.write_all(toml::to_string(&tests_conf).map_err(|e| format!("serialize toml error {:?}", e))?.as_bytes())
            .map_err(|e| format!("failure to write to test.toml {:?}", e))?;
    }
    else {
        log::info!("test not found, generating tests.toml was skipped");
    }
    Ok(())
}
