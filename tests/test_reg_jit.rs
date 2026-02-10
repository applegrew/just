extern crate just;

use just::parser::JsParser;
use just::runner::jit;
use just::runner::ds::value::{JsNumberType, JsValue};

#[test]
fn test_reg_jit_simple_add() {
    if !cfg!(target_arch = "x86_64") {
        eprintln!("Skipping reg_jit test on non-x86_64 host");
        return;
    }
    let ast = JsParser::parse_to_ast_from_str("var x = 3 + 4;").unwrap();
    let (result, mut ctx) = jit::compile_and_run_reg_jit_with_ctx(&ast);
    assert!(result.is_ok());
    let value = ctx.get_binding("x").unwrap_or(JsValue::Undefined);
    match value {
        JsValue::Number(JsNumberType::Integer(n)) => assert_eq!(n, 7),
        JsValue::Number(JsNumberType::Float(f)) => assert_eq!(f, 7.0),
        other => panic!("x was {:?}, expected numeric", other),
    }
}
