use std::path::*;
use std::io::Write;
use std::collections::HashMap;

use crate::subcommand::unit_test::*;
use crate::subcommand::generate::*;

pub enum SummaryElement {
    Dir(PathBuf, HashMap<String, SummaryElement>),
    File(PathBuf),
}

fn name_to_title(s: &str) -> String {
    s.split('_').map(|s| {
        let mut iter = s.chars();
        let mut res = String::new();
        if let Some(c) = iter.next() {
            res.push(c.to_ascii_uppercase());
        }
        for c in iter {
            res.push(c);
        }
        res
    }).collect::<Vec<String>>().join(" ")
}

fn regist_summary_rec<'a>(summary_mp: &mut HashMap<String, SummaryElement>, now_dir: PathBuf, mut strip_iter: std::path::Iter<'a>, name: String) -> Result<(), String> {
    if let Some(dir) = strip_iter.next() {
        let dir_name = dir.to_str().unwrap().to_string();
        let now_dir = now_dir.join(dir);
        if !summary_mp.contains_key(&dir_name) {
            summary_mp.insert(dir_name.clone(), SummaryElement::Dir(now_dir.clone(), HashMap::new()));
            std::fs::create_dir_all(&now_dir)
                .map_err(|e| format!("failure to create dir, {:?}, {:?}", now_dir, e))?;
            let mut index_md = std::fs::File::create(now_dir.join("index.md"))
                .map_err(|e| format!("failure to create index.md, {:?}", e))?;
            index_md.write_fmt(format_args!("# {}", name_to_title(&dir_name)))
                .map_err(|e| format!("failure to write index.md, {:?}", e))?;
        }
        if let SummaryElement::Dir(_, new_mp) = summary_mp.get_mut(&dir_name).unwrap() {
            regist_summary_rec(new_mp, now_dir, strip_iter, name)?;
        }
    }
    else {
        let doc_md = now_dir.join(&name).with_extension("md");
        summary_mp.insert(name, SummaryElement::File(doc_md));
    }
    Ok(())
}

fn generate_summarymd_rec(summary_mp: &HashMap<String, SummaryElement>, indent: String) -> Vec<String> {
    let mut summary_doc = Vec::new();
    for (name, (path, next_mp)) in summary_mp
        .into_iter()
        .filter_map(|(name, elem)| {
            match elem {
                SummaryElement::Dir(path, mp) => Some((name, (path, mp))),
                _ => None,
            }
        })
    {
        summary_doc.push(format!("{}- [{}]({}/index.md)", indent, name_to_title(name), path.as_os_str().to_str().unwrap()));
        let mut child_summary = generate_summarymd_rec(next_mp, indent.clone() + "  ");
        summary_doc.append(&mut child_summary);
    }
    for (name, path) in summary_mp
        .into_iter()
        .filter_map(|(name, elem)| {
            match elem {
                SummaryElement::File(path) => Some((name, path)),
                _ => None,
            }
        })
    {
        summary_doc.push(format!("{}- [{}]({})", indent, name_to_title(name), path.as_os_str().to_str().unwrap()));
    }
    summary_doc
}

pub fn generate_document(library_dir: &Path) -> Result<(), String> {
    let target_list = generate_headers(library_dir)?;
    let lib_conf = load_library_config(library_dir)?;
    
    let mut summary = HashMap::<String, SummaryElement>::new();
    
    for (target, transpiled, tests) in target_list {
        let strip = target.strip_prefix(library_dir)
            .map_err(|e| format!("strip error {:?}", e))?;
        let strip_dir = strip.parent().unwrap();
        let name = strip.file_stem().unwrap();

        {
            let now_dir = PathBuf::new();
            regist_summary_rec(&mut summary, now_dir, strip_dir.iter(), name.to_str().unwrap().to_string())?;
        }


        let doc_dir = library_dir.join("md").join(strip_dir);
        let doc_md = doc_dir.join(name).with_extension("md");
        std::fs::create_dir_all(&doc_dir)
            .map_err(|e| format!("failure to create dir {:?}, {:?}", doc_dir, e))?;
        
        

        let mut md_file = std::fs::File::create(&doc_md)
            .map_err(|e| format!("failure to create md file {:?}, {:?}", doc_md, e))?;

        
        md_file.write_fmt(format_args!("# {}\n ## Tests\n\n", name_to_title(name.to_str().unwrap())))
            .map_err(|e| format!("failure to write, {:?}", e))?;
        for test in tests {
            let repo = lib_conf.testers.iter().find(|tester| tester.name == test.tester).unwrap().repo.clone();
            md_file.write_fmt(format_args!("- [{}]({}): `{}`\n", test.tester, repo, test.problem))
                .map_err(|e| format!("failure to write, {:?}", e))?;
        }
        md_file.write_fmt(format_args!("\n## Niu\n\n```\n"))
            .map_err(|e| format!("failure to write, {:?}", e))?;
        {
            let niu_prog = std::fs::read_to_string(target)
                .map_err(|e| format!("failure to read target, {:?}", e))?;
            md_file.write_all(niu_prog.as_bytes())
                .map_err(|e| format!("failure to write niu prog, {:?}", e))?;
        }
        md_file.write_fmt(format_args!("```\n\n## C++\n\n```cpp\n"))
            .map_err(|e| format!("failure to write, {:?}", e))?;
        {
            let niu_prog = std::fs::read_to_string(transpiled)
                .map_err(|e| format!("failure to read target, {:?}", e))?;
            md_file.write_all(niu_prog.as_bytes())
                .map_err(|e| format!("failure to write niu prog, {:?}", e))?;
        }
        md_file.write_fmt(format_args!("```\n"))
            .map_err(|e| format!("failure to write, {:?}", e))?;
    }
    {
        let summary_dir = library_dir.join("md").join("SUMMARY.md");
        let summary_str = generate_summarymd_rec(&summary, "  ".to_string())
            .join("\n");
        let mut summary_file = std::fs::File::create(summary_dir)
            .map_err(|e| format!("failure to crate SUMMARY.md, {:?}", e))?;
        summary_file.write_fmt(format_args!("- [Introduction](../readme.md)\n"))
            .map_err(|e| format!("failure to write SUMMARY.md, {:?}", e))?;
        summary_file.write_all(summary_str.as_bytes())
            .map_err(|e| format!("failure to write SUMMARY.md, {:?}", e))?;
    }
    Ok(())
}
