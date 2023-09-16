use narxia_syn::dbg_node;
use narxia_syn::syntree::SynTree;
use narxia_syn::token_source::text_ts::TextTokenSource;

fn main() {
    narxia_log_impl::init();

    let mut ts = TextTokenSource::new(
        r#"
    fn main(args: Args) {
        let x: A = a + b.c.d e f[g]
    }
    "#,
    );
    let mut parser = narxia_syn::parser::Parser::new(&mut ts);
    parser.parse();
    let (tree, errors) = parser.finish_to_tree();
    if !errors.is_empty() {
        eprintln!("errors: {errors:?}");
    }
    // assert!(errors.is_empty());
    let tree = SynTree::new(tree);

    let root = tree.get_root();
    dbg_node!(root);
    for item in root.get_item_list() {
        if let Some(fn_def) = item.get_fn_def() {
            let name_ident = fn_def.get_fn_head().get_fn_name().unwrap().get_ident();
            let name = name_ident.text();
            println!("Found fn with name: {}", name);
            for stmt in fn_def.get_block().unwrap().get_stmt_list() {
                if let Some(let_stmt) = stmt.get_let_stmt() {
                    let name_ident = let_stmt.get_pat().unwrap().get_ident();
                    let name = name_ident.text();
                    println!("Found let with name: {}", name);
                    let expr = let_stmt.get_expr().unwrap();

                    dbg_node!(expr);
                }
            }
        }
    }
}
