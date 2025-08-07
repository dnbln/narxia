#![feature(path_file_prefix)]

use std::collections::HashMap;
use std::fmt::Write;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::result;
use std::slice;

use dir_structure::DirChildSingle;
use dir_structure::DirChildSingleOpt;
use dir_structure::DirChildren;
use dir_structure::DirStructure;
use dir_structure::Versioned;
use dir_structure::file_prefix_filter;
use dir_structure::json::Json;
use git2::RebaseOperationType;
use git2::Repository;
use git2::build::CheckoutBuilder;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to read steps: {0}")]
    ReadStepsError(#[from] serde_json::Error),
    #[error("git error: {0}")]
    GitError(#[from] git2::Error),
    #[error("dir-structure error: {0}")]
    DirStructureError(#[from] dir_structure::Error),
    #[error("IO error: {0}")]
    IO(#[from] io::Error),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(DirStructure)]
pub struct Guide {
    #[dir_structure(path = "steps.json")]
    steps: Versioned<Json<Steps>>,
    #[dir_structure(path = "steps")]
    step_dirs: DirChildren<StepDir>,
    code_header: Option<String>,
    code_footer: Option<String>,
    #[dir_structure(path = self)]
    template: DirChildSingle<String, TemplateFilter>,
}

impl Guide {
    pub fn get_step_dir(&self, step: &StepRef) -> Option<&StepDir> {
        self.step_dirs.get_value_by_name(&step.0)
    }

    pub fn steps_iter(&self) -> StepsIter<'_> {
        StepsIter(self.steps.steps.iter(), &self.step_dirs)
    }

    pub fn template(&self) -> &String {
        self.template.value()
    }

    pub fn render_template(&self, steps: String) -> String {
        self.template().replace("<__GitVoyageSteps />", &steps)
    }

    pub fn render_guide(&self) -> String {
        let mut steps = String::new();
        for (_step, step_dir) in self.steps_iter() {
            writeln!(&mut steps, "{}", step_dir.render_step(self)).unwrap();
        }
        self.render_template(steps)
    }
}

pub struct StepsIter<'a>(slice::Iter<'a, StepRef>, &'a DirChildren<StepDir>);

impl<'a> Iterator for StepsIter<'a> {
    type Item = (&'a StepRef, &'a StepDir);

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .and_then(|step| self.1.get_value_by_name(&step.0).map(|dir| (step, dir)))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a> ExactSizeIterator for StepsIter<'a> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> DoubleEndedIterator for StepsIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0
            .next_back()
            .and_then(|step| self.1.get_value_by_name(&step.0).map(|dir| (step, dir)))
    }
}

#[derive(DirStructure)]
pub struct StepDir {
    #[dir_structure(path = self)]
    before: DirChildSingleOpt<String, BeforeFilter>,
    #[dir_structure(path = self)]
    after: DirChildSingleOpt<String, AfterFilter>,
    #[dir_structure(path = self)]
    pub code: DirChildSingle<String, CodeFilter>,

    code_header: Option<String>,
    code_footer: Option<String>,
    self_path: PathBuf,
}

impl StepDir {
    pub fn code_path(&self) -> PathBuf {
        self.self_path.join(self.code.file_name())
    }

    pub fn render_step(&self, guide: &Guide) -> String {
        let mut output = String::new();
        if let DirChildSingleOpt::Some(before) = &self.before {
            writeln!(output, "{}", before.value()).unwrap();
        }
        if let Some(header) = &self.code_header.as_ref().or(guide.code_header.as_ref()) {
            writeln!(output, "{header}").unwrap();
        }
        writeln!(output, "{}", self.code.value()).unwrap();
        if let Some(footer) = &self.code_footer.as_ref().or(guide.code_footer.as_ref()) {
            writeln!(output, "{footer}").unwrap();
        }
        if let DirChildSingleOpt::Some(after) = &self.after {
            writeln!(output, "{}", after.value()).unwrap();
        }

        output
    }
}

file_prefix_filter!(pub TemplateFilter, "template");
file_prefix_filter!(pub BeforeFilter, "before");
file_prefix_filter!(pub AfterFilter, "after");
file_prefix_filter!(pub CodeFilter, "code");

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Steps {
    steps: Vec<StepRef>,
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct StepRef(String);

impl StepRef {
    pub fn new(path: String) -> Self {
        Self(path)
    }

    pub fn path(&self) -> &str {
        &self.0
    }
}

trait DbgGitErr {
    fn dbg_git_err(self) -> Self;
}

impl<T> DbgGitErr for result::Result<T, git2::Error> {
    fn dbg_git_err(self) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(e) => {
                eprintln!("Git error: {}", e);
                eprintln!("Class of error: {:?}", e.class());
                eprintln!("Message: {}", e.message());
                eprintln!("Code: {:?}", e.code());
                Err(e)
            }
        }
    }
}

fn perform_patchup(
    guide: &Guide,
    step: &StepRef,
    new_code: &str,
    repo_root: &Path,
    repo: &Repository,
    mut resolve_conflict: impl FnMut(&Path) -> Result<()>,
) -> Result<()> {
    let code_file = repo_root.join("code");
    fs::write(&code_file, b"")?;

    let mut index = repo.index()?;
    index.add_all(["*"].iter(), git2::IndexAddOption::DEFAULT, None)?;
    index.write()?;
    let tree_id = index.write_tree()?;
    let signature = repo.signature()?;
    let c = repo.commit(
        Some("HEAD"),
        &signature,
        &signature,
        "Initial commit",
        &repo.find_tree(tree_id)?,
        &[],
    )?;

    let mut current_commit = repo.find_commit(c)?;
    let mut before_ed_branch = None;
    let mut edit_branch = None;

    for s in &guide.steps.steps {
        let mut index = repo.index()?;
        let code = &*guide.step_dirs.get_name(&s.0).unwrap().value().code;
        fs::write(&code_file, code)?;
        index.add_all(["code"], git2::IndexAddOption::DEFAULT, None)?;
        index.write()?;
        let tree_id = index.write_tree()?;
        let c = repo.commit(
            Some("HEAD"),
            &signature,
            &signature,
            &format!("step: {}", s.0),
            &repo.find_tree(tree_id)?,
            &[&current_commit],
        )?;

        current_commit = repo.find_commit(c)?;

        if s == step {
            before_ed_branch = Some(repo.branch("before-edit", &current_commit, true)?);
            fs::write(&code_file, new_code)?;

            index.add_all(["code"], git2::IndexAddOption::DEFAULT, None)?;
            index.write()?;
            let tree_id = index.write_tree()?;
            let edit_c = repo.commit(
                Some("HEAD"),
                &signature,
                &signature,
                &format!("patchup: {}", s.0),
                &repo.find_tree(tree_id)?,
                &[&current_commit],
            )?;

            let ed = repo.find_commit(edit_c)?;

            edit_branch = Some(repo.branch("edit", &ed, true)?);

            repo.reset(
                current_commit.as_object(),
                git2::ResetType::Hard,
                Some(CheckoutBuilder::new().path("code").force()),
            )?;
        }
    }

    let final_branch = repo.branch("final", &current_commit, true)?;

    let before_edit =
        repo.reference_to_annotated_commit(&before_ed_branch.unwrap().into_reference())?;
    let edit = repo.reference_to_annotated_commit(&edit_branch.unwrap().into_reference())?;
    let f = repo.reference_to_annotated_commit(&final_branch.into_reference())?;

    let mut merge_opts = git2::MergeOptions::new();
    merge_opts.file_favor(git2::FileFavor::Normal);
    merge_opts.diff3_style(true);
    merge_opts.rename_threshold(50);
    merge_opts.fail_on_conflict(false);

    let mut rebase = repo
        .rebase(
            Some(&f),
            Some(&before_edit),
            Some(&edit),
            Some(git2::RebaseOptions::new().merge_options(merge_opts)),
        )
        .dbg_git_err()?;

    while let Some(info) = rebase.next() {
        let info = info.dbg_git_err()?;
        let len = repo
            .diff_index_to_workdir(None, None)
            .dbg_git_err()?
            .deltas()
            .len();
        if len > 0 {
            eprintln!("Found {} changes in commit: {}", len, info.id());
            resolve_conflict(&code_file)?;
            repo.index()?.add_path(Path::new("code"))?;
        }
        match info.kind() {
            Some(RebaseOperationType::Pick) => {
                rebase.commit(None, &signature, None).dbg_git_err()?;
            }
            Some(RebaseOperationType::Reword) => {
                eprintln!("Rewording commit: {}", info.id());
                rebase.commit(None, &signature, None).dbg_git_err()?;
            }
            Some(RebaseOperationType::Edit) => {
                eprintln!("Editing commit: {}", info.id());
                rebase.commit(None, &signature, None).dbg_git_err()?;
            }
            Some(RebaseOperationType::Squash) => {
                eprintln!("Squashing commit: {}", info.id());
                rebase.commit(None, &signature, None).dbg_git_err()?;
            }
            Some(RebaseOperationType::Fixup) => {
                eprintln!("Fixing up commit: {}", info.id());
                rebase.commit(None, &signature, None).dbg_git_err()?;
            }
            Some(RebaseOperationType::Exec) => {}
            None => {}
        }
    }

    rebase.finish(None)?;

    Ok(())
}

pub fn patchup(
    guide: &mut Guide,
    dir: &Path,
    step: &StepRef,
    new_code: &str,
    resolve_conflict: impl FnMut(&Path) -> Result<()>,
) -> Result<()> {
    let repo_root = dir.join(".repo");
    if repo_root.exists() {
        fs::remove_dir_all(&repo_root)?;
    }
    fs::create_dir_all(&repo_root)?;
    let repo = Repository::init(&repo_root)?;
    match perform_patchup(guide, step, new_code, &repo_root, &repo, resolve_conflict) {
        Ok(_) => {
            repatch(guide, dir)?;
        }
        Err(e) => {
            eprintln!("Failed to patchup: {}", e);
            return Err(e);
        }
    }

    Ok(())
}

pub fn repatch(guide: &mut Guide, dir: &Path) -> Result<()> {
    let repo_root = dir.join(".repo");
    if !repo_root.exists() {
        return Err(Error::IO(io::Error::new(
            io::ErrorKind::NotFound,
            "Repository not found",
        )));
    }
    let repo = Repository::open(&repo_root)?;
    let code_file = repo_root.join("code");

    if !code_file.exists() {
        return Err(Error::IO(io::Error::new(
            io::ErrorKind::NotFound,
            "Code file not found",
        )));
    }

    let mut files = HashMap::new();

    let mut commit = repo.head()?.peel_to_commit()?;

    loop {
        let message = commit.message().unwrap();
        let contents = commit
            .tree()?
            .get_path(Path::new("code"))?
            .to_object(&repo)?
            .peel_to_blob()?
            .content()
            .to_vec();
        if let Some(step_name) = message.strip_prefix("step: ") {
            // most likely original commit, we don't want to overwrite it, if it already exists in the map
            // (patchup comes after the original commit, and when we explore we walk backwards)
            if !files.contains_key(step_name) {
                files.insert(step_name.to_string(), contents);
            }
        } else if let Some(step_name) = message.strip_prefix("patchup: ") {
            files.insert(step_name.to_string(), contents);
        }

        if commit.parent_count() == 0 {
            break;
        }

        commit = commit.parent(0)?;
    }

    for step_dir in guide.step_dirs.iter_mut() {
        let step_name = step_dir.file_name().clone();
        if let Some(contents) = files.get(step_name.to_str().unwrap()) {
            step_dir.value_mut().code = DirChildSingle::new(
                step_dir.value().self_path.join("code"),
                String::from_utf8(contents.clone()).unwrap(),
            );
            eprintln!("Repatched step: {}", step_name.to_str().unwrap());
        } else {
            panic!(
                "No patch found for step: {}\nSomething went terribly wrong.",
                step_name.to_str().unwrap()
            );
        }
    }

    Ok(())
}
