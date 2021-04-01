mod parser_util;

use parser_util::assert_parse;

#[test]
fn test_simple() {
    assert_parse("simple");
}

#[test]
fn test_closure() {
    assert_parse("closure");
}

#[test]
fn test_arrows() {
    assert_parse("arrows");
}

#[test]
fn test_bin_octal() {
    assert_parse("bin_octal");
}

#[test]
fn test_classes() {
    assert_parse("classes");
}

#[test]
fn test_default_rest_spread() {
    assert_parse("default_rest_spread");
}

#[test]
fn test_destruct() {
    assert_parse("destruct");
}

#[test]
fn test_for_of() {
    assert_parse("for_of");
}

#[test]
fn test_generator() {
    assert_parse("generator");
}

#[test]
fn test_let_const() {
    assert_parse("let_const");
}

#[test]
fn test_new_std_lib_functions() {
    assert_parse("new_std_lib_funcs");
}

#[test]
fn test_object_literals() {
    assert_parse("object_literals");
}

#[test]
fn test_string_templates() {
    assert_parse("string_templates");
}

#[test]
fn test_symbols() {
    assert_parse("symbols");
}

#[test]
fn test_tail_calls() {
    assert_parse("tail_calls");
}

#[test]
fn test_promises() {
    assert_parse("promises");
}
