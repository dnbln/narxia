use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process;

use clap::Parser;
use dir_structure::DirStructureItem;
use git_voyage::Guide;
use git_voyage::StepRef;

#[derive(Parser, Debug)]
enum App {
    Patchup {
        dir: PathBuf,
        #[clap(long)]
        step: String,
    },
    FinishPatchup {
        dir: PathBuf,
    },
    Build {
        dir: PathBuf,
        #[clap(short)]
        output: Option<PathBuf>,
    },
}

fn edit(editor: &Path, path: &Path) -> git_voyage::Result<()> {
    let mut proc = process::Command::new(editor).arg(path).spawn()?;
    let r = proc.wait()?;
    if !r.success() {
        return Err(git_voyage::Error::IO(io::Error::other(format!(
            "Editor exited with non-zero status {}",
            r.code().unwrap_or(-1)
        ))));
    }
    Ok(())
}

fn main() {
    let app = App::parse();

    match app {
        App::Patchup { dir, step } => {
            let step = StepRef::new(step);
            let mut guide = Guide::read(&dir).unwrap();
            let step_dir = guide.get_step_dir(&step).unwrap();
            let old_code = step_dir.code.value();
            let editor = git2::Config::open_default()
                .unwrap()
                .get_path("core.editor")
                .unwrap();
            edit(&editor, &step_dir.code_path()).expect("Failed to edit");

            let new_code = fs::read_to_string(step_dir.code_path())
                .expect("Failed to read code file after editing");

            if new_code != *old_code {
                eprintln!("Code changed, performing patchup...");
                git_voyage::patchup(&mut guide, &dir, &step, &new_code, |p| edit(&editor, p))
                    .unwrap();
            } else {
                eprintln!("No changes detected in code, skipping patchup.");
            }
        }
        App::FinishPatchup { dir } => {
            let mut guide = Guide::read(&dir).unwrap();
            git_voyage::repatch(&mut guide, &dir).unwrap();
            eprintln!("Patchup finished");
        }
        App::Build { dir, output } => {
            let guide = Guide::read(&dir).unwrap();
            let tmpl = guide.render_guide();
            match output {
                Some(out) => {
                    fs::write(out, tmpl).expect("Failed to write output file");
                }
                None => {
                    println!("{}", tmpl);
                }
            }
        }
    }
}
