use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process;

use clap::Parser;
use dir_structure::traits::sync::DirStructureItem;
use git_voyage::Extension;
use git_voyage::Guide;
use git_voyage::StepRef;

#[derive(Parser, Debug)]
#[command(name = "git voyage")]
#[command(bin_name = "git voyage")]
enum App {
    /// Initialize a new guide.
    Init {
        /// The directory that will contain the guide.
        dir: PathBuf,

        /// The extension of the template files. Should include the leading dot.
        ///
        /// Use --template-ext '' for no extension.
        #[clap(short = 'e', long = "template-ext", default_value = ".mdx")]
        template_extension: String,
    },
    /// Adds a new step to the guide.
    Add {
        /// The directory that will contain the guide.
        dir: PathBuf,
        /// The name of the step to add.
        #[clap(long)]
        step: String,

        /// The extension of the code file. Should include the leading dot.
        ///
        /// Will default to the extension of the previous step, or no extension if
        /// there is no previous step.
        #[clap(long = "code-ext")]
        code_extension: Option<String>,

        /// The extension to use for the before/after files. Should include the leading dot.
        ///
        /// If not specified, it will default to the extension of the previous step, or
        /// `.mdx` if there isn't any step in the guide yet. Also see `--after`.
        ///
        /// Pass --before-after-ext '' to not use any extension.
        #[clap(long = "before-after-ext")]
        before_after_extension: Option<String>,

        /// The name of the step to add after.
        ///
        /// Will add the step after the specified step. If not specified,
        /// the step will be added at the end of the guide.
        #[clap(long = "after")]
        after_step: Option<String>,
    },
    /// Patch a step in the guide.
    ///
    /// This will also track the changes made to the step,
    /// and will apply them to all subsequent steps.
    ///
    /// It will first open the code of the step in the editor,
    /// and when the editor is closed, it will first check if the
    /// code has changed.
    ///
    /// If the code has changed, it will perform the patch to the
    /// step, and then apply it to all subsequent steps, via
    /// `git-rebase`.
    ///
    /// If there are any conflicts, it will stop and open the editor
    /// again, so that you can resolve the conflicts.
    Patch {
        /// The directory containing the guide.
        dir: PathBuf,
        /// The name of the step we want to patch.
        ///
        /// `git-voyage` will apply the patch to the step with this name,
        /// and then apply it to all subsequent steps.
        #[clap(long)]
        step: String,
    },
    /// Build the guide.
    Build {
        /// The directory containing the guide.
        dir: PathBuf,
        /// A path to the file to write the guide to.
        ///
        /// If not specified, the guide will be printed to stdout.
        #[clap(short, long)]
        out: Option<PathBuf>,
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
        App::Init {
            dir,
            template_extension,
        } => {
            if dir.exists() {
                if dir.is_file() {
                    eprintln!("{} is a file, not a directory", dir.display());
                    process::exit(1);
                }

                let entries = fs::read_dir(&dir).expect("Failed to read guide directory");
                if entries.count() > 0 {
                    eprintln!("{} is not an empty directory", dir.display());
                    process::exit(1);
                }
            }
            Guide::new_default(dir.clone(), &template_extension)
                .write(&dir)
                .expect("Failed to write guide");
            fs::write(dir.join(".gitignore"), "/.repo\n").expect("Failed to write .gitignore");
            eprintln!("Guide initialized in {}", dir.display());
        }
        App::Add {
            dir,
            step,
            code_extension,
            before_after_extension,
            after_step,
        } => {
            let mut guide = Guide::read(&dir).unwrap();
            let step = StepRef::new(step);
            let after_step = after_step.map(StepRef::new);

            let code_extension = code_extension.map(Extension::new);
            let before_after_extension = before_after_extension.map(Extension::new);

            guide.add_step(
                &step,
                after_step.as_ref(),
                code_extension,
                before_after_extension,
            );
            guide.write(&dir).expect("Failed to write guide");
            eprintln!("Step {} added to the guide", step.path());
        }
        App::Patch { dir, step } => {
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

            if new_code != **old_code {
                eprintln!("Code changed, performing patchup...");
                git_voyage::patchup(&mut guide, &dir, &step, &new_code, |p| edit(&editor, p))
                    .unwrap();
                guide.write(&dir).expect("Failed to write guide");
            } else {
                eprintln!("No changes detected in code, skipping patchup.");
            }
        }
        App::Build { dir, out: output } => {
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
