use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;

use colored::Colorize;
use miette::{bail, Context, IntoDiagnostic};
use narxia_syn::syntree::tests_data::{
    AccessorCalledDataList, AccessorCalledDataReturned, AccessorInfo, ElemRef,
};
use narxia_test_runner::parser_tests::parser_tests_dir;

struct InputFile {
    path: PathBuf,
    contents: String,
}

#[derive(Debug, Clone, Copy)]
struct InputFileId(usize);

struct AllCalledData {
    map: HashMap<AccessorInfo, CalledInfo>,
}

struct CalledInfo {
    returned_something: bool,
    call_set: Vec<IndividualCallInfo>,
}

struct IndividualCallInfo {
    called_at: (InputFileId, ElemRef),
    returned: Vec<ElemRef>,
}

impl AllCalledData {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn join(&mut self, file_id: InputFileId, acdl: AccessorCalledDataList) {
        for acd in acdl.accessors {
            let (currently_returned_something, node_refs) = match acd.returned {
                AccessorCalledDataReturned::Returned(nr) => (true, vec![nr]),
                AccessorCalledDataReturned::ReturnedList(l) => (!l.is_empty(), l),
                AccessorCalledDataReturned::Nothing => (false, vec![]),
                AccessorCalledDataReturned::EmptyAllowed => {
                    (true, vec![]) // empty allowed counts as
                                   // returned something for the purposes of this test
                }
            };

            match self.map.entry(acd.accessor_info) {
                Entry::Occupied(oe) => {
                    let previously_returned_something = oe.into_mut();
                    previously_returned_something.returned_something |=
                        currently_returned_something;
                    previously_returned_something
                        .call_set
                        .push(IndividualCallInfo {
                            called_at: (file_id, acd.called_at),
                            returned: node_refs,
                        });
                }
                Entry::Vacant(ve) => {
                    ve.insert(CalledInfo {
                        returned_something: currently_returned_something,
                        call_set: vec![IndividualCallInfo {
                            called_at: (file_id, acd.called_at),
                            returned: node_refs,
                        }],
                    });
                }
            }
        }
    }
}

fn check_all(input_files: Vec<InputFile>) -> miette::Result<()> {
    let mut global_called_list = AllCalledData::new();
    for (id, input_file) in input_files.iter().enumerate() {
        eprintln!(
            "Checking file {}",
            format!("{}", input_file.path.display()).bright_blue()
        );
        eprintln!("{}", ">>>".bright_white());
        eprintln!("{}", input_file.contents.bright_blue());
        eprintln!("{}", "<<<".bright_white());

        let id = InputFileId(id);
        let ctx = narxia_driver::DriverCtx::initialize();
        let src_file =
            narxia_driver::load_file(&ctx, input_file.path.clone(), input_file.contents.clone());
        let syn_file = narxia_driver::parse_file_and_assert_no_errors(&ctx, src_file);
        let tree = syn_file.tree(&ctx.db);

        let mut acdl = AccessorCalledDataList::new();
        tree.get_root().call_accessors(&mut acdl);
        global_called_list.join(id, acdl);
    }

    let mut missing = Vec::new();

    for (info, called) in global_called_list.map {
        if !called.returned_something {
            missing.push((info, called));
        }
    }

    if !missing.is_empty() {
        eprintln!(
            "{}",
            r#"
Missing accessors:
The following accessors were called, but they didn't return anything
(or they returned an empty list), which is a problem because we have
no guarantee now that the parse tree matches our syn models.
"#
            .trim()
            .bright_red()
        );
        for (info, called) in missing {
            eprintln!(
                "  {}::{}",
                info.ty_name.bright_cyan(),
                info.accessor_name.bright_blue().bold()
            );

            for call in called.call_set {
                eprintln!(
                    "    Called at {}@{}",
                    format!("{:?}", call.called_at.1.kind).bright_green(),
                    format!("{}", call.called_at.1.span).purple().bold(),
                );
                eprintln!(
                    "    In file {}",
                    format!("{}", input_files[call.called_at.0 .0].path.display()).bright_blue()
                );
            }
        }
        bail!("Missing accessors");
    }

    Ok(())
}

fn run() -> miette::Result<()> {
    let p = parser_tests_dir();
    let mut input_files = Vec::new();
    for entry in p.read_dir().into_diagnostic().context("read_dir")? {
        let entry = entry.into_diagnostic().context("read_dir")?;
        let path = entry.path();
        if path.is_dir() {
            let p = path.join(narxia_test_runner::parser_tests::INPUT_FILE_NAME);
            let contents = std::fs::read_to_string(&p)
                .into_diagnostic()
                .with_context(|| format!("Cannot read input file at {p:?}"))?;
            input_files.push(InputFile { path: p, contents });
        }
    }

    if input_files.is_empty() {
        bail!("No input files found");
    }

    check_all(input_files)
}

#[test]
pub fn check() -> miette::Result<()> {
    run()
}
