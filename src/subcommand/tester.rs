use std::path::*;
use std::process::Command;

use super::unit_test::{Testers, load_library_config};

pub fn download_testers(library_dir: &Path) -> Result<(), String> {
    let test_dir = library_dir.join(".test");
    let lib_conf = load_library_config(library_dir)?;
    for Testers { ref name, ref repo, .. } in lib_conf.testers.iter() {
        log::info!("download tester {}", name);
        if test_dir.join(name).exists() {
            log::info!("tester {} exist, skip", name);
        }
        else {
            log::info!("clone from {}", repo);
            if Command::new("git")
                .arg("clone")
                .arg(repo)
                .arg(name)
                .arg("--depth")
                .arg("1")
                .current_dir(&test_dir)
                .spawn()
                .map_err(|e| format!("git clone spawn error, {:?}", e))?.wait()
                .map_err(|e| format!("wait error, {:?}", e))?
                .success()
                {
                    log::info!("cloned from {}", repo);
                }
            else {
                return Err(format!("failure to clone from {}", repo));
            }
        }
    }
    Ok(())
}
