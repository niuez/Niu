use serde::Deserialize;
use std::path::*;
use std::process::{ Stdio, Command };
use std::os::unix::io::{FromRawFd, IntoRawFd};

#[derive(Deserialize)]
struct TestConfig {
    compiler: String,
    compile_options: Vec<String>,
    testers: Vec<Testers>,
    tests: Vec<Tests>,
}

#[derive(Deserialize)]
struct Testers {
    name: String,
    generator: String,
}


#[derive(Deserialize)]
struct Tests {
    program: String,
    tester: String,
    problem: String,
}

fn find_input_names(problem_dir: &Path) -> std::io::Result<Vec<std::ffi::OsString>> {
    let mut names = Vec::new();
    for input in std::fs::read_dir(problem_dir.join("in"))? {
        let input = input?;
        let name = input.path().file_stem().unwrap().to_os_string();
        log::info!("input {:?}", name);
        names.push(name)
    }
    Ok(names)
}

pub fn test_cppfiles(libraries_dir: &Path) -> Result<(), String> {
    let test_config = std::fs::read_to_string(libraries_dir.join(".test/tests.toml"))
        .map_err(|e| format!("cant open tests.toml, {:?}", e))?;
    let test_config: TestConfig = toml::from_str(&test_config)
        .map_err(|e| format!("cant parse tests.toml, {:?}", e))?;
    let TestConfig { compiler, compile_options, testers, tests } = test_config;
    for Tests { program, tester, problem } in tests.into_iter() {
        log::info!("start test {}", program);
        let Testers { generator, .. } = testers.iter().find(|Testers { ref name, .. }| tester == *name).ok_or(format!("not found tester {:?}", tester))?;

        log::info!("generate test {}:{}", tester, problem);
        let mut command = Command::new(libraries_dir.join(".test").join(&tester).join(&generator).as_os_str())
            .arg(libraries_dir.join(".test").join(&tester).join(&problem).join("info.toml").as_os_str())
            .spawn()
            .map_err(|e| format!("command build error, {:?}", e))?;
        if !command.wait().map_err(|e| format!("command run error, {:?}", e))?.success() {
            log::error!("failure to generate {}:{}", tester, problem);
            return Err(format!("failure to generate {}:{}", tester, problem));
        }

        log::info!("compile program {}", program);
        let mut command = Command::new(&compiler)
            .current_dir(libraries_dir.join(".test").as_os_str())
            .arg(&program)
            .args(compile_options.clone())
            .spawn()
            .map_err(|e| format!("command build error, {:?}", e))?;
        if !command.wait().map_err(|e| format!("command run error, {:?}", e))?.success() {
            log::error!("failure to compile {}", program);
            return Err(format!("failure to compile {}", program));
        }

        log::info!("search input file");
        let input_names = find_input_names(libraries_dir.join(".test").join(&tester).join(&problem).as_path())
            .map_err(|e| format!("error finding inputs, {:?}", e))?;

        for input in input_names.into_iter() {
            log::info!("test {:?}", input);
            {
                let input_file = std::fs::File::open(libraries_dir.join(".test").join(&tester).join(&problem).join("in").join(&input).with_extension("in"))
                    .map_err(|e| format!("cant open input file, {:?}", e))?;
                let output_file = std::fs::File::create(libraries_dir.join(".test").join(&input).with_extension("out").as_os_str())
                    .map_err(|e| format!("cant create output file, {:?}", e))?;

                let mut command = Command::new("./a.out")
                    .current_dir(libraries_dir.join(".test").as_os_str())
                    //.stdin(Stdio::piped())
                    //.stdout(Stdio::piped())
                    .stdin(unsafe { Stdio::from_raw_fd(input_file.into_raw_fd()) })
                    .stdout(unsafe { Stdio::from_raw_fd(output_file.into_raw_fd()) })
                    .spawn()
                    .map_err(|e| format!("failure to execute a.out, {:?}", e))?;
                /*{
                    let in_ = command.stdin.as_mut().unwrap();
                    let result = command.stdout.as_mut().unwrap();
                    std::io::copy(&mut input_file, in_).unwrap();
                    std::io::copy(result, &mut output_file).unwrap();
                }*/
                if !command.wait().map_err(|e| format!("executing a.out error, {:?}", e))?.success() {
                    log::error!("failure to execute a.out");
                    return Err(format!("failure to execute a.out"));
                }
            }
            log::info!("check {:?}", input);
            let mut command = Command::new(Path::new(&tester).join(&problem).join("checker"))
                .current_dir(libraries_dir.join(".test").as_os_str())
                .arg(Path::new(&tester).join(&problem).join("in").join(&input).with_extension("in"))
                .arg(Path::new(&tester).join(&problem).join("out").join(&input).with_extension("out"))
                .arg(Path::new(&input).with_extension("out"))
                .spawn()
                .map_err(|e| format!("failure to execute checker, {:?}", e))?;
            if command.wait().map_err(|e| format!("failure to execute checker, {:?}", e))?.success() {
                log::info!("pass {:?}", input);
            }
            else {
                log::error!("failure {:?}", input);
                return Err(format!("failure test {:?} in {}", input, program));
            }
            
        }
    }
    Ok(())
}


