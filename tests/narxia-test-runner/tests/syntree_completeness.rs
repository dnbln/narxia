// Checks for the completeness of the syntree models.
//
// This test checks that there is a sequence of accessors
// that we can call to reach any node or token in the tree.
//
// This test uses the following approach:
// 1. The narxia_syn_helper::syntree_node macro generates a "call_accessors" method,
//    which calls all accessors on the node and collects the results in a list.
// 2. We look at the results in the list and check that all the nodes and tokens
//    are reachable.
// 3. If there is a node or token that is not reachable, then we fail this test.
//
// This test is meant to be used in conjunction with
// the syntree_correctness test, to prove that our model of the syntree
// matches what the parser produces.

use colored::{ColoredString, Colorize};
use libtest_mimic::{Arguments, Failed, Trial};
use miette::{bail, IntoDiagnostic};
use narxia_syn::syntax_kind::SyntaxKind;
use narxia_syn::syntree::tests_data::{
    AccessorCalledDataList, AccessorCalledDataReturned, ElemRef,
};
use narxia_syn::syntree::{Node, Token, TreeNode};
use narxia_syn::text_span::TextSpan;
use narxia_test_runner::parser_tests::ParserTestSingleFolder;

fn node_chk(node: &Node) -> impl Fn(&ElemRef) -> bool + '_ {
    |e| e.kind == node.kind() && e.span == TextSpan::of_node(node)
}

fn token_chk(token: &Token) -> impl Fn(&ElemRef) -> bool + '_ {
    |e| e.kind == token.kind() && e.span == TextSpan::of(token)
}

fn acdl_contains(acdl: &AccessorCalledDataList, chk: impl Fn(&ElemRef) -> bool) -> bool {
    for acd in &acdl.accessors {
        match &acd.returned {
            AccessorCalledDataReturned::Returned(er) => {
                if chk(er) {
                    return true;
                }
            }
            AccessorCalledDataReturned::ReturnedList(element_refs) => {
                for er in element_refs {
                    if chk(er) {
                        return true;
                    }
                }
            }
            AccessorCalledDataReturned::Nothing => {}
            AccessorCalledDataReturned::EmptyAllowed => {}
        }
    }

    false
}

fn run_for_test(test: ParserTestSingleFolder) -> miette::Result<()> {
    let ctx = narxia_driver::DriverCtx::initialize();
    let input = test.input.perform_read().into_diagnostic()?;
    let src_file = narxia_driver::load_file(&ctx, test.input_file_path(), input.0);
    let syn_file = narxia_driver::parse_file_and_assert_no_errors(&ctx, src_file);

    let tree = syn_file.tree(&ctx.db);
    let mut acdl = AccessorCalledDataList::new();
    tree.get_root().call_accessors(&mut acdl);

    let mut missing = Vec::new();
    let mut missing_tokens = Vec::new();

    for node in tree.get_root().get_node().descendants_with_tokens() {
        match node {
            narxia_syn::syntree::SyntaxElement::Node(node) => {
                if node.kind() != SyntaxKind::Root && !acdl_contains(&acdl, node_chk(&node)) {
                    missing.push(node);
                }
            }
            narxia_syn::syntree::SyntaxElement::Token(t) => {
                if !acdl_contains(&acdl, token_chk(&t)) {
                    missing_tokens.push(t);
                }
            }
        }
    }

    if !missing.is_empty() || !missing_tokens.is_empty() {
        eprintln!(
            "{:?}",
            narxia_syn::syntree::CustomTreePresenter::new(
                narxia_syn::syntree::SyntaxElementRef::Node(tree.get_root().get_node()),
                narxia_syn::syntree::Offset::new(0),
                2,
                &|f, offset, node| {
                    let is_missing = missing.iter().any(|it| {
                        it.kind() == node.kind() && TextSpan::of_node(it) == TextSpan::of_node(node)
                    });

                    let painter = match is_missing {
                        true => |s: ColoredString| s.bright_red(),
                        false => |s: ColoredString| s.green(),
                    };

                    let data = painter(ColoredString::from(
                        format!("{:?}@{}", node.kind(), TextSpan::of_node(node)).as_str(),
                    ));

                    writeln!(f, "{:offset$}{data}", "", offset = offset.0)
                },
                &|f, offset, token| {
                    let is_missing = missing_tokens.iter().any(|it| {
                        it.kind() == token.kind() && TextSpan::of(it) == TextSpan::of(token)
                    });

                    let painter = match is_missing {
                        true => |s: ColoredString| s.bright_red(),
                        false => |s: ColoredString| s.green(),
                    };

                    let data = painter(ColoredString::from(
                        format!(
                            "{:?}@{} {:?}",
                            token.kind(),
                            TextSpan::of(token),
                            token.text()
                        )
                        .as_str(),
                    ));

                    writeln!(f, "{:offset$}{data}", "", offset = offset.0)
                },
            )
        );

        bail!(
            "Missing elements: {:?} and missing tokens: {:?}",
            missing,
            missing_tokens
        );
    }

    Ok(())
}

fn run_test_wrap(test: ParserTestSingleFolder) -> Result<(), Failed> {
    run_for_test(test).map_err(Failed::from)
}

narxia_test_runner::parser_test_trials!(collect_trials, run_test_wrap);

fn main() -> miette::Result<()> {
    let args = Arguments::from_args();
    let trials = collect_trials()?;
    libtest_mimic::run(&args, trials).exit();
}
