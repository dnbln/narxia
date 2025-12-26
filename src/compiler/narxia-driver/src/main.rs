use core::fmt;
use std::ffi::OsString;
use std::io;
use std::path::PathBuf;

use clap::Parser;
use clap_complete::Shell;
use clap_complete::ValueHint;
use clap_complete::generate;
use miette::IntoDiagnostic;
use narxia_codegen::CodegenBackend;
use narxia_codegen::ir;
use narxia_driver::HirDbg;
use narxia_driver::ctxt::DriverCtx;
use narxia_hir_typechk::sema;
use narxia_log::info;

/// Compiler for narxia.
#[derive(Parser, Debug)]
#[command(name = "nrx", author, version)]
enum NarxiaDriverCommand {
    /// Parse the given file.
    ///
    /// This command will parse the given file and print the resulting syntax tree.
    #[clap(name = "parse")]
    Parse(NarxiaDriverParseCommand),
    #[clap(name = "display-hir")]
    DisplayHir(NarxiaDriverDisplayHirCommand),
    #[clap(name = "display-hir-debug")]
    DisplayHirDebug(NarxiaDriverDisplayHirDebugCommand),
    #[clap(name = "hiri")]
    Hiri(NarxiaDriverHiriCommand),
    #[clap(name = "sema-analysis")]
    SemaAnalysis(NarxiaDriverSemaAnalysisCommand),
    #[clap(name = "codegen")]
    #[clap(alias = "cg")]
    Codegen(NarxiaDriverCodegenCommand),
    #[clap(name = "ssa")]
    Ssa(NarxiaDriverSsaCommand),

    #[clap(name = "comp")]
    Comp { shell: Shell },
    #[clap(name = "man-gen")]
    ManGen {
        #[clap(value_hint = ValueHint::DirPath)]
        output: PathBuf,
    },
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverParseCommand {
    #[clap(value_hint = ValueHint::FilePath)]
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverDisplayHirCommand {
    #[clap(value_hint = ValueHint::FilePath)]
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverDisplayHirDebugCommand {
    #[clap(value_hint = ValueHint::FilePath)]
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverHiriCommand {
    #[clap(value_hint = ValueHint::FilePath)]
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverSemaAnalysisCommand {
    #[clap(value_hint = ValueHint::FilePath)]
    file: PathBuf,
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverSsaCommand {
    #[clap(value_hint = ValueHint::FilePath)]
    file: PathBuf,
    #[clap(long)]
    validate: bool,
}

#[derive(Debug, Clone)]
enum Out {
    File(PathBuf),
    Stdout,
}

impl From<OsString> for Out {
    fn from(value: OsString) -> Self {
        if value == "-" {
            Out::Stdout
        } else {
            Out::File(value.into())
        }
    }
}

impl fmt::Display for Out {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Out::File(path) => write!(f, "{}", path.display()),
            Out::Stdout => write!(f, "-"),
        }
    }
}

#[derive(Parser, Debug)]
pub struct NarxiaDriverCodegenCommand {
    file: PathBuf,
    #[clap(long, short, default_value = "-")]
    out: Out,
}

fn main() -> miette::Result<()> {
    narxia_driver::init_panic_hook();

    let ctx = DriverCtx::initialize();

    let tcx = ctx.db.get_global_ty_ctxt().make_ty_ctxt();

    let _span = narxia_log::einfo_span!("main");

    let cmd = NarxiaDriverCommand::parse();
    match cmd {
        NarxiaDriverCommand::Parse(parse_cmd) => {
            narxia_log::i!("Parse command: {parse_cmd:?}");

            let file = parse_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            println!("{:?}", tree.tree(&ctx.db));
        }
        NarxiaDriverCommand::DisplayHir(display_hir_cmd) => {
            narxia_log::i!("Display HIR command: {display_hir_cmd:?}");

            let file = display_hir_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;
            let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(file);

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            let hir = ctx.lower_file(file_map_entry, tree);

            let hir_mod = hir.mod_def(&ctx.db);
            let hir_map = tcx.hir_map();

            println!("{}", hir_map.get_mod(hir_mod).hir_dbg(&ctx));
        }
        NarxiaDriverCommand::DisplayHirDebug(display_hir_cmd) => {
            narxia_log::i!("Display HIR debug command: {display_hir_cmd:?}");

            let file = display_hir_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;
            let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(file);

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);
            let hir = ctx.lower_file(file_map_entry, tree);

            println!("{:?}", hir.mod_def(&ctx.db).hir_dbg(&ctx));
        }

        NarxiaDriverCommand::SemaAnalysis(sema_cmd) => {
            narxia_log::i!("Sema analysis command: {sema_cmd:?}");

            let file = sema_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;
            let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(file);

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);

            narxia_log::i!("Parsed file");

            let hir = ctx.lower_file(file_map_entry, tree);
            narxia_log::i!("Lowered file");

            let hir_mod = hir.mod_def(&ctx.db);

            let analysis_results = sema::analyze_program_structure(tcx, hir_mod);

            println!("{analysis_results:?}");

            for scope in analysis_results.program_structure.scopes() {
                let parent = analysis_results.program_structure.parent(scope);
                let self_scope = analysis_results.program_structure.self_element(scope);

                if let Some(self_scope) = self_scope {
                    let hir_self = analysis_results.program_structure.element(self_scope);

                    let hir_parent = parent
                        .and_then(|parent| analysis_results.program_structure.self_element(parent))
                        .map(|parent| analysis_results.program_structure.element(parent));

                    println!("Scope: {:?}", hir_self.hir_dbg(&ctx));

                    if let Some(hir_parent) = hir_parent {
                        println!("Parent: {:?}", hir_parent.hir_dbg(&ctx));
                    }
                }
            }

            sema::resolve_work(tcx, hir_mod, &analysis_results);

            tcx.dump_resolutions_diagnostics(&ctx.db.src_file_db);
        }

        NarxiaDriverCommand::Hiri(hiri_cmd) => {
            narxia_log::i!("HIRI command: {hiri_cmd:?}");

            let file = hiri_cmd.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;
            let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(file);

            narxia_log::i!("Read file");

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);

            narxia_log::i!("Parsed file");

            let hir = ctx.lower_file(file_map_entry, tree);
            narxia_log::i!("Lowered file");

            let hir_mod = hir.mod_def(&ctx.db);

            let hir_map = tcx.hir_map();

            let mut ictx = narxia_hiri::InterpContext::new(&hir_map);

            narxia_hiri::interp_mod(&mut ictx, hir_map.get_mod(hir_mod));
        }

        NarxiaDriverCommand::Codegen(cg) => {
            narxia_log::i!("Codegen command: {cg:?}");

            let file = cg.file;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;
            let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(file);

            narxia_log::i!("Read file");

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);

            narxia_log::i!("Parsed file");

            let hir = ctx.lower_file(file_map_entry, tree);
            narxia_log::i!("Lowered file");

            let mut cg_tyctxt = narxia_codegen::TyCtxt::new();

            let mut stdout = io::stdout();
            let out = match &cg.out {
                Out::File(path_buf) => narxia_codegen::Out::File(path_buf.clone()),
                Out::Stdout => narxia_codegen::Out::ToWrite(&mut stdout),
            };

            narxia_codegen_llvm::Backend::new()
                .generate_code(
                    &cg_tyctxt,
                    &ir::Mod {
                        globals: vec![],
                        functions: vec![],
                        global_code: ir::Block { instr: vec![] },
                    },
                    out,
                )
                .unwrap();
        }
        NarxiaDriverCommand::Ssa(ssa) => {
            narxia_log::i!("Ssa command: {ssa:?}");

            let file = ssa.file;
            let validate = ssa.validate;
            let file = narxia_driver::read_file(&ctx, file).into_diagnostic()?;
            let file_map_entry = ctx.db.get_global_ty_ctxt().add_file_map_entry(file);

            narxia_log::i!("Read file");

            ctx.trace_file(file);

            let tree = narxia_driver::parse_file_and_assert_no_errors(&ctx, file);

            narxia_log::i!("Parsed file");

            let hir = ctx.lower_file(file_map_entry, tree);
            narxia_log::i!("Lowered file");

            let hir_mod = hir.mod_def(&ctx.db);

            info!("Hir map updated");

            let analysis_results = sema::analyze_program_structure(tcx, hir_mod);
            info!("Analysis results: {:?}", analysis_results);

            sema::resolve_work(tcx, hir_mod, &analysis_results);

            tcx.dump_resolutions();

            let hir_map = ctx.db.get_global_ty_ctxt().make_ty_ctxt().hir_map();
            let module = narxia_ssa_lower::convert(tcx, &hir_map, hir_mod);
            println!("{module:#?}");

            if validate {
                let result = narxia_ssa_validator::validate(&module);

                let Err(result) = result else {
                    return Ok(());
                };

                narxia_ssa_validator::present_validation_errors(&module, &result);
            }
        }
        NarxiaDriverCommand::Comp { shell } => {
            narxia_log::i!("Comp command: {shell:?}");

            let mut cmd = <NarxiaDriverCommand as clap::CommandFactory>::command();
            generate(shell, &mut cmd, env!("CARGO_BIN_NAME"), &mut io::stdout());
        }
        NarxiaDriverCommand::ManGen { output } => {
            narxia_log::i!("ManGen command: {output:?}");

            let cmd = <NarxiaDriverCommand as clap::CommandFactory>::command();

            clap_mangen::generate_to(cmd, &output).into_diagnostic()?;
        }
    }

    Ok(())
}
