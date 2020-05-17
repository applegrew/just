use crate::parser::api::parse_to_ast;

#[test]
fn test_var() {
    let r = parse_to_ast("var a=10;").unwrap();
    println!("{:#?}", r);
}