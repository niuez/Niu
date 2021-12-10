use serde::{ Serialize, Deserialize };
use std::path::*;
use std::process::{ Stdio, Command };
//use std::os::unix::io::{FromRawFd, IntoRawFd};

#[derive(Clone, Deserialize, Serialize)]
pub struct TestConfig {
    pub compiler: String,
    pub compile_options: Vec<String>,
    pub testers: Vec<Testers>,
    pub tests: Vec<Tests>,
}

#[derive(Clone, Deserialize, Serialize)]
pub struct LibraryConfig {
    pub compiler: String,
    pub compile_options: Vec<String>,
    pub testers: Vec<Testers>,
}

#[derive(Clone, Deserialize, Serialize)]
pub struct Testers {
    pub name: String,
    pub repo: String,
    pub generator: String,
}


#[derive(Clone, Deserialize, Serialize)]
pub struct Tests {
    pub program_file: String,
    pub tester: String,
    pub problem: String,
}

impl TestConfig {
    pub fn generate(lib_conf: &LibraryConfig, tests: &Vec<Tests>) -> Self {
        TestConfig {
            compiler: lib_conf.compiler.clone(),
            compile_options: lib_conf.compile_options.clone(),
            testers: lib_conf.testers.clone(),
            tests: tests.clone(),
        }
    }
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

pub fn get_library_dir(current_dir: &Path) -> Result<PathBuf, String> {
    let mut path = current_dir.canonicalize().map_err(|e| format!("cant canonicalize, {:?}", e))?;
    loop {
        let toml_path = path.join("library.toml");
        if toml_path.exists() {
            log::info!("library.toml found at {:?}", path);
            break Ok(path)
        }
        if let Some(par) = path.parent() {
            path = par.to_path_buf()
        }
        else {
            break Err(format!("not found library.toml"))
        }
    }
}

pub fn load_library_config(libraries_dir: &Path) -> Result<LibraryConfig, String> {
    let library_config = std::fs::read_to_string(libraries_dir.join("library.toml"))
        .map_err(|e| format!("cant open library.toml, {:?}", e))?;
    toml::from_str(&library_config)
        .map_err(|e| format!("cant parse library.toml, {:?}", e))
}


pub fn test_cppfiles(libraries_dir: &Path, test_name: Option<&str>) -> Result<(), String> {
    let test_config = std::fs::read_to_string(libraries_dir.join(".test").join("tests.toml"))
        .map_err(|e| format!("cant open tests.toml, {:?}", e))?;
    let test_config: TestConfig = toml::from_str(&test_config)
        .map_err(|e| format!("cant parse tests.toml, {:?}", e))?;
    let TestConfig { compiler, compile_options, testers, tests } = test_config;
    for Tests { program_file, tester, problem } in tests.into_iter() {
        if test_name.map(|name| !program_file.contains(name)).unwrap_or(false) {
            log::info!("skip test {}", program_file);
            continue
        }
        log::info!("start test {}", program_file);
        let Testers { generator, .. } = testers.iter().find(|Testers { ref name, .. }| tester == *name).ok_or(format!("not found tester {:?}", tester))?;

        log::info!("generate test {}:{}", tester, problem);
        let mut command = Command::new(libraries_dir.join(".test").join(&tester).join(&generator).as_os_str())
            .arg(libraries_dir.join(".test").join(&tester).join(&problem).join("info.toml").as_os_str())
            .current_dir(libraries_dir)
            .spawn()
            .map_err(|e| format!("command build error, {:?}", e))?;
        if !command.wait().map_err(|e| format!("command run error, {:?}", e))?.success() {
            log::error!("failure to generate {}:{}", tester, problem);
            return Err(format!("failure to generate {}:{}", tester, problem));
        }

        log::info!("compile program_file {}", program_file);
        let mut command = Command::new(&compiler)
            .current_dir(libraries_dir.join(".test").as_os_str())
            .arg(&program_file)
            .args(compile_options.clone())
            .spawn()
            .map_err(|e| format!("command build error, {:?}", e))?;
        if !command.wait().map_err(|e| format!("command run error, {:?}", e))?.success() {
            log::error!("failure to compile {}", program_file);
            return Err(format!("failure to compile {}", program_file));
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
                    .stdin(Stdio::from(input_file))
                    .stdout(Stdio::from(output_file))
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
                return Err(format!("failure test {:?} in {}", input, program_file));
            }
            
        }
    }
    Ok(())
}

