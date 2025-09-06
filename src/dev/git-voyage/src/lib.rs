pub extern crate git2;

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::result;
use std::slice;

use dir_structure::DirStructure;
use dir_structure::data_formats::json_pretty::JsonPretty;
use dir_structure::dir_children::DirChildSingle;
use dir_structure::dir_children::DirChildSingleOpt;
use dir_structure::dir_children::DirChildren;
use dir_structure::dir_children::Filter;
use dir_structure::dir_children::ForceCreateDirChildren;
use dir_structure::file_prefix_filter;
use dir_structure::traits::resolve::resolve_path;
use dir_structure::versioned::Versioned;
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
    DirStructureError(#[from] dir_structure::error::Error),
    #[error("IO error: {0}")]
    IO(#[from] io::Error),
    #[error("Failed to parse step reference: {0}")]
    ParseStepReferenceError(String),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(DirStructure)]
pub struct Guide {
    #[dir_structure(path = "steps.json")]
    steps: Versioned<JsonPretty<Steps>>,
    #[dir_structure(path = "steps")]
    step_dirs: ForceCreateDirChildren<StepDir>,
    code_header: Option<Versioned<String>>,
    code_footer: Option<Versioned<String>>,
    #[dir_structure(path = self)]
    template: Versioned<DirChildSingle<String, TemplateFilter>>,

    self_path: PathBuf,
}

impl Guide {
    pub fn new_default(dir: PathBuf, template_extension: &str) -> Self {
        Self {
            steps: Versioned::new_dirty(
                JsonPretty(Steps { steps: vec![] }),
                resolve_path!([Guide @ dir.clone()].steps),
            ),
            step_dirs: ForceCreateDirChildren::new(DirChildren::new()),
            code_header: Some(Versioned::new_dirty(
                String::from("```"),
                dir.join("code_header"),
            )),
            code_footer: Some(Versioned::new_dirty(
                String::from("```"),
                dir.join("code_footer"),
            )),
            template: Versioned::new_dirty(
                DirChildSingle::new(
                    format!("template{template_extension}"),
                    String::from(
                        r#"
---
title: My guide
description: A guide to guides
---

Beginning of the guide.

And more...

Do not change this next line, it is used to render the steps:
<__GitVoyageSteps />

End of the guide.
"#
                        .trim_start(),
                    ),
                ),
                dir.join(format!("template{template_extension}")),
            ),
            self_path: dir,
        }
    }

    pub fn add_step(
        &mut self,
        step: &StepRef,
        after_step: Option<&StepRef>,
        code_extension: Option<Extension>,
        before_after_extension: Option<Extension>,
    ) {
        let (code, code_extension, before_after) = if let Some(after) = after_step {
            let pos = self
                .steps
                .steps
                .iter()
                .position(|s| s == after)
                .expect("Step to add after not found");
            self.steps.steps.insert(pos + 1, step.clone());

            let code_extension = code_extension
                .or_else(|| {
                    self.step_dirs
                        .get_value_by_name(&after.0)
                        .map(|dir| dir.code_extension())
                })
                .unwrap_or_else(Extension::default_code_extension);

            let before_after = before_after_extension
                .or_else(|| {
                    self.step_dirs
                        .get_value_by_name(&after.0)
                        .and_then(|dir| dir.before_after_extension())
                })
                .unwrap_or_else(Extension::default_before_after_extension);

            (
                self.step_dirs
                    .get_value_by_name(&after.0)
                    .expect("Step to add after not found")
                    .code
                    .value()
                    .clone(),
                code_extension,
                before_after,
            )
        } else {
            let (code, code_extension, before_after) = match self.steps.steps.last() {
                Some(last_step) => {
                    let code_extension = code_extension
                        .or_else(|| {
                            self.step_dirs
                                .get_value_by_name(&last_step.0)
                                .map(|dir| dir.code_extension())
                        })
                        .unwrap_or_else(Extension::default_code_extension);

                    let before_after = before_after_extension
                        .or_else(|| {
                            self.step_dirs
                                .get_value_by_name(&last_step.0)
                                .and_then(|dir| dir.before_after_extension())
                        })
                        .unwrap_or_else(Extension::default_before_after_extension);

                    (
                        self.step_dirs
                            .get_value_by_name(&last_step.0)
                            .expect("Last step not found")
                            .code
                            .value()
                            .clone(),
                        code_extension,
                        before_after,
                    )
                }
                None => (
                    Versioned::new_dirty(
                        String::new(),
                        resolve_path!([&self.self_path as Guide].step_dirs.${&step.0}.code),
                    ),
                    code_extension.unwrap_or_else(Extension::default_code_extension),
                    before_after_extension
                        .unwrap_or_else(Extension::default_before_after_extension),
                ),
            };

            self.steps.steps.push(step.clone());
            (code, code_extension, before_after)
        };

        let step_dir = StepDir {
            before: DirChildSingleOpt::Some(DirChildSingle::new(
                format!("before{before_after}"),
                Versioned::new_dirty(
                    String::new(),
                    resolve_path!([&self.self_path as Guide].step_dirs.${&step.0})
                        .join(format!("before{before_after}")),
                ),
            )),
            after: DirChildSingleOpt::Some(DirChildSingle::new(
                format!("after{before_after}"),
                Versioned::new_dirty(
                    String::new(),
                    resolve_path!([&self.self_path as Guide].step_dirs.${&step.0})
                        .join(format!("after{before_after}")),
                ),
            )),
            code: DirChildSingle::new(format!("code{code_extension}"), code),
            code_header: None,
            code_footer: None,
            self_path: resolve_path!([Guide @ self.self_path.clone()].step_dirs.${&step.0}),
        };
        self.step_dirs.push(step.0.clone(), step_dir);
    }

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

#[derive(Debug)]
pub struct Extension(String);

impl fmt::Display for Extension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Extension {
    pub const fn new(ext: String) -> Self {
        Self(ext)
    }

    pub fn default_code_extension() -> Self {
        Self::new(String::new())
    }

    pub fn default_before_after_extension() -> Self {
        Self::new(String::from(".mdx"))
    }

    pub fn value(&self) -> &str {
        &self.0
    }

    fn guess_from<T, F: Filter>(dir_child: &DirChildSingle<T, F>) -> Self {
        let s = dir_child.file_name().to_str().unwrap();
        let p = Path::new(dir_child.file_name());
        let prefix = if let Some(prefix) = p.file_prefix() {
            prefix.to_str().unwrap()
        } else {
            ""
        };
        Self::new(s.strip_prefix(prefix).unwrap().to_owned())
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
    before: DirChildSingleOpt<Versioned<String>, BeforeFilter>,
    #[dir_structure(path = self)]
    after: DirChildSingleOpt<Versioned<String>, AfterFilter>,
    #[dir_structure(path = self)]
    pub code: DirChildSingle<Versioned<String>, CodeFilter>,

    code_header: Option<Versioned<String>>,
    code_footer: Option<Versioned<String>>,
    self_path: PathBuf,
}

impl StepDir {
    pub fn code_path(&self) -> PathBuf {
        self.self_path.join(self.code.file_name())
    }

    pub fn render_step(&self, guide: &Guide) -> String {
        let mut output = String::new();
        if let DirChildSingleOpt::Some(before) = &self.before {
            writeln!(output, "{}", &**before.value()).unwrap();
        }
        if let Some(header) = self.code_header.as_ref().or(guide.code_header.as_ref()) {
            writeln!(output, "{}", &**header).unwrap();
        }
        writeln!(output, "{}", &**self.code.value()).unwrap();
        if let Some(footer) = self.code_footer.as_ref().or(guide.code_footer.as_ref()) {
            writeln!(output, "{}", &**footer).unwrap();
        }
        if let DirChildSingleOpt::Some(after) = &self.after {
            writeln!(output, "{}", &**after.value()).unwrap();
        }

        output
    }

    pub fn before_after_extension(&self) -> Option<Extension> {
        self.before
            .as_ref()
            .to_option()
            .or(self
                .after
                .as_ref()
                .to_option()
                .map(DirChildSingle::map_filter))
            .map(|dir| Extension::guess_from(&dir))
    }

    pub fn code_extension(&self) -> Extension {
        Extension::guess_from(&self.code)
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
) -> Result<BTreeMap<String, String>> {
    let code_file = repo_root.join("code");
    fs::write(&code_file, b"")?;

    let mut index = repo.index()?;
    index.add_all(["code"].iter(), git2::IndexAddOption::DEFAULT, None)?;
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
        fs::write(&code_file, &**code)?;
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
    merge_opts.fail_on_conflict(false);

    let mut rebase = repo
        .rebase(
            Some(&f),
            Some(&before_edit),
            Some(&edit),
            Some(git2::RebaseOptions::new().merge_options(merge_opts)),
        )
        .dbg_git_err()?;

    let mut empty_steps = BTreeMap::new();
    let mut parent = None;

    while let Some(info) = rebase.next() {
        let info = info.dbg_git_err()?;
        let index = repo.index().dbg_git_err()?;
        let len = repo
            .diff_index_to_workdir(None, None)
            .dbg_git_err()?
            .deltas()
            .len();
        println!("Index has conflicts: {}", index.has_conflicts());
        if len > 0 {
            eprintln!("Found {} changes in commit: {}", len, info.id());
            resolve_conflict(&code_file)?;
            repo.index()?.add_path(Path::new("code"))?;
        }
        match info.kind() {
            Some(RebaseOperationType::Exec) => {}
            _ => match rebase.commit(None, &signature, None) {
                Ok(oid) => {
                    parent = Some(oid);
                }
                Err(e) if e.code() == git2::ErrorCode::Applied => {
                    let rebasing_commit = repo.find_commit(info.id())?;
                    let original = repo.find_commit(parent.unwrap())?;
                    let Some(message) = rebasing_commit.message() else {
                        return Err(Error::ParseStepReferenceError(
                            "Commit message is not valid UTF-8".to_owned(),
                        ));
                    };
                    let Some(reference) = message
                        .strip_prefix("step: ")
                        .or_else(|| message.strip_prefix("patchup: "))
                    else {
                        return Err(Error::ParseStepReferenceError(
                            "Commit message does not start with 'step: ' or 'patchup: '".to_owned(),
                        ));
                    };

                    let code_file = original.tree()?.get_path(Path::new("code"))?;
                    let code_blob = code_file.to_object(repo)?.peel_to_blob()?;
                    let code_content =
                        String::from_utf8(code_blob.content().to_vec()).map_err(|_| {
                            Error::ParseStepReferenceError(
                                "Code content is not valid UTF-8".to_owned(),
                            )
                        })?;

                    empty_steps.insert(reference.to_owned(), code_content);
                }
                Err(e) => {
                    Err(e).dbg_git_err()?;
                }
            },
        }
    }

    rebase.finish(None)?;

    Ok(empty_steps)
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
        Ok(empty_commits) => {
            repatch(guide, dir, &empty_commits)?;
        }
        Err(e) => {
            eprintln!("Failed to patchup: {}", e);
            return Err(e);
        }
    }

    Ok(())
}

pub fn repatch(
    guide: &mut Guide,
    dir: &Path,
    empty_commits: &BTreeMap<String, String>,
) -> Result<()> {
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
            let c = &mut step_dir.value_mut().code;
            c.value_mut()
                .edit_eq_check(|v| *v = String::from_utf8(contents.clone()).unwrap());
            eprintln!("Repatched step: {}", step_name.to_str().unwrap());
        } else if let Some(contents) = empty_commits.get(step_name.to_str().unwrap()) {
            let c = &mut step_dir.value_mut().code;
            c.value_mut().edit_eq_check(|v| *v = contents.clone());
            eprintln!(
                "Repatched step (empty commit): {}",
                step_name.to_str().unwrap()
            );
        } else {
            panic!(
                "No patch found for step: {}\nSomething went terribly wrong.",
                step_name.to_str().unwrap()
            );
        }
    }

    Ok(())
}
