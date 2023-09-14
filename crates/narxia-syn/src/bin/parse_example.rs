use narxia_syn::syntree::SynTree;
use narxia_syn::token_source::text_ts::TextTokenSource;

fn main() {
    let mut ts = TextTokenSource::new(r#"fn main(v: T) {
        {
            if (it) c else if (it2) c2 else c3

            let x = "abc"
            x
        }
    }"#);
    let mut parser = narxia_syn::parser::Parser::new(&mut ts);
    parser.parse();
    let (tree, errors) = parser.finish_to_tree();
    if !errors.is_empty() {
        eprintln!("errors: {errors:?}");
    }
    // assert!(errors.is_empty());
    let tree = SynTree::new(tree);
    println!("{:?}", tree);
}