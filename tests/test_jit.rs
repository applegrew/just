extern crate just;

use just::parser::JsParser;
use just::runner::jit;
use just::runner::plugin::registry::BuiltInRegistry;
use just::runner::ds::value::{JsValue, JsNumberType};

fn jit_run_get_var(code: &str, var_name: &str) -> JsValue {
    let ast = JsParser::parse_to_ast_from_str(code).unwrap();
    let registry = BuiltInRegistry::with_core();
    let (_, mut ctx) = jit::compile_and_run_with_ctx(&ast, &registry);
    ctx.get_binding(var_name).unwrap_or(JsValue::Undefined)
}

fn jit_run_get_int(code: &str, var_name: &str) -> i64 {
    match jit_run_get_var(code, var_name) {
        JsValue::Number(JsNumberType::Integer(n)) => n,
        other => panic!("{} was {:?}, expected integer", var_name, other),
    }
}

fn jit_disasm(code: &str) -> String {
    let ast = JsParser::parse_to_ast_from_str(code).unwrap();
    let chunk = jit::compile(&ast);
    chunk.disassemble("test")
}

#[test]
fn test_jit_simple_var() {
    assert_eq!(jit_run_get_int("var x = 42;", "x"), 42);
}

#[test]
fn test_jit_assignment() {
    assert_eq!(jit_run_get_int("var a = 5; var b = 10; a = b;", "a"), 10);
}

#[test]
fn test_jit_arithmetic() {
    assert_eq!(jit_run_get_int("var x = 3 + 4 * 2;", "x"), 11);
}

#[test]
fn test_jit_simple_for() {
    let code = "var sum = 0; for (var i = 0; i < 5; i = i + 1) { sum = sum + i; }";
    eprintln!("{}", jit_disasm(code));
    assert_eq!(jit_run_get_int(code, "sum"), 10);
}

#[test]
fn test_jit_nested_for() {
    let code = r#"
var count = 0;
for (var i = 0; i < 3; i = i + 1) {
    for (var j = 0; j < 4; j = j + 1) {
        count = count + 1;
    }
}
"#;
    eprintln!("{}", jit_disasm(code));
    assert_eq!(jit_run_get_int(code, "count"), 12);
}

#[test]
fn test_jit_fibonacci() {
    // Minimal fibonacci with n=3 and debug trace
    let code = r#"
var a = 0;
var b = 1;
for (var i = 0; i < 3; i = i + 1) {
    var temp = a;
    a = b;
    b = temp + b;
}
"#;
    assert_eq!(jit_run_get_int(code, "a"), 2);
}

#[test]
fn test_jit_while_loop() {
    let code = "var i = 0; var sum = 0; while (i < 5) { sum = sum + i; i = i + 1; }";
    assert_eq!(jit_run_get_int(code, "sum"), 10);
}

#[test]
fn test_jit_if_else() {
    assert_eq!(jit_run_get_int("var x = 0; if (true) { x = 1; } else { x = 2; }", "x"), 1);
    assert_eq!(jit_run_get_int("var x = 0; if (false) { x = 1; } else { x = 2; }", "x"), 2);
}

#[test]
fn test_jit_comparison() {
    assert_eq!(jit_run_get_int("var x = 0; if (3 < 5) { x = 1; }", "x"), 1);
    assert_eq!(jit_run_get_int("var x = 0; if (5 < 3) { x = 1; }", "x"), 0);
}

#[test]
fn test_jit_break() {
    let code = r#"
var count = 0;
for (var i = 0; i < 10; i = i + 1) {
    if (i === 5) { break; }
    count = count + 1;
}
"#;
    assert_eq!(jit_run_get_int(code, "count"), 5);
}
