use std::path::PathBuf;

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

fn main() {
    let app = App::parse();

    match app {
        App::Patchup { dir, step } => {
            let step = StepRef::new(step);
            let mut guide = Guide::read(&dir).unwrap();
            let step_dir = guide.get_step_dir(&step).unwrap();
            let old_code = step_dir.code.value();
            let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vim".into());
            let mut proc = std::process::Command::new(editor)
                .arg(step_dir.code_path())
                .spawn()
                .expect("Failed to open editor");
            let r = proc.wait().expect("Editor process failed");
            if !r.success() {
                eprintln!(
                    "Editor exited with non-zero status {}",
                    r.code().unwrap_or(-1)
                );
                std::process::exit(1);
            }

            let new_code = std::fs::read_to_string(step_dir.code_path())
                .expect("Failed to read code file after editing");

            if new_code != *old_code {
                eprintln!("Code changed, performing patchup...");
                git_voyage::patchup(&mut guide, &dir, &step, &new_code).unwrap();
            } else {
                eprintln!("No changes detected in code, skipping patchup.");
                return;
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
                    std::fs::write(out, tmpl).expect("Failed to write output file");
                }
                None => {
                    println!("{}", tmpl);
                }
            }
        }
    }
}
