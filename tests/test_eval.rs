//! Tests for the expression evaluation engine.
//!
//! These tests verify the basic expression evaluation functionality
//! including literals, operators, and type conversions.

extern crate just;

use just::runner::eval::expression::{evaluate_expression, to_boolean};
use just::runner::plugin::types::EvalContext;
use just::runner::ds::value::{JsValue, JsNumberType};
use just::parser::ast::{
    ExpressionType, LiteralData, LiteralType, NumberLiteralType, Meta,
    BinaryOperator, UnaryOperator, LogicalOperator,
};
use std::rc::Rc;

/// Helper to create a simple meta for tests.
fn test_meta() -> Meta {
    Meta {
        start_index: 0,
        end_index: 0,
        script: Rc::new(String::new()),
    }
}

/// Helper to create a number literal expression.
fn num_expr(n: i64) -> ExpressionType {
    ExpressionType::Literal(LiteralData {
        meta: test_meta(),
        value: LiteralType::NumberLiteral(NumberLiteralType::IntegerLiteral(n)),
    })
}

/// Helper to create a float literal expression.
fn float_expr(f: f64) -> ExpressionType {
    ExpressionType::Literal(LiteralData {
        meta: test_meta(),
        value: LiteralType::NumberLiteral(NumberLiteralType::FloatLiteral(f)),
    })
}

/// Helper to create a string literal expression.
fn str_expr(s: &str) -> ExpressionType {
    ExpressionType::Literal(LiteralData {
        meta: test_meta(),
        value: LiteralType::StringLiteral(s.to_string()),
    })
}

/// Helper to create a boolean literal expression.
fn bool_expr(b: bool) -> ExpressionType {
    ExpressionType::Literal(LiteralData {
        meta: test_meta(),
        value: LiteralType::BooleanLiteral(b),
    })
}

/// Helper to create a null literal expression.
fn null_expr() -> ExpressionType {
    ExpressionType::Literal(LiteralData {
        meta: test_meta(),
        value: LiteralType::NullLiteral,
    })
}

/// Helper to create a binary expression.
fn binary_expr(op: BinaryOperator, left: ExpressionType, right: ExpressionType) -> ExpressionType {
    ExpressionType::BinaryExpression {
        meta: test_meta(),
        operator: op,
        left: Rc::new(left),
        right: Rc::new(right),
    }
}

/// Helper to create a unary expression.
fn unary_expr(op: UnaryOperator, arg: ExpressionType) -> ExpressionType {
    ExpressionType::UnaryExpression {
        meta: test_meta(),
        operator: op,
        argument: Rc::new(arg),
    }
}

/// Helper to create a logical expression.
fn logical_expr(op: LogicalOperator, left: ExpressionType, right: ExpressionType) -> ExpressionType {
    ExpressionType::LogicalExpression {
        meta: test_meta(),
        operator: op,
        left: Rc::new(left),
        right: Rc::new(right),
    }
}

/// Helper to create a conditional expression.
fn cond_expr(test: ExpressionType, consequent: ExpressionType, alternate: ExpressionType) -> ExpressionType {
    ExpressionType::ConditionalExpression {
        meta: test_meta(),
        test: Rc::new(test),
        consequent: Rc::new(consequent),
        alternate: Rc::new(alternate),
    }
}

// ============================================================================
// Literal tests
// ============================================================================

#[test]
fn test_number_literal() {
    let mut ctx = EvalContext::new();
    let expr = num_expr(42);
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(42)));
}

#[test]
fn test_float_literal() {
    let mut ctx = EvalContext::new();
    let expr = float_expr(3.14);
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    match result {
        JsValue::Number(JsNumberType::Float(f)) => assert!((f - 3.14).abs() < 0.001),
        _ => panic!("Expected float"),
    }
}

#[test]
fn test_string_literal() {
    let mut ctx = EvalContext::new();
    let expr = str_expr("hello");
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::String("hello".to_string()));
}

#[test]
fn test_boolean_literal_true() {
    let mut ctx = EvalContext::new();
    let expr = bool_expr(true);
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_boolean_literal_false() {
    let mut ctx = EvalContext::new();
    let expr = bool_expr(false);
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_null_literal() {
    let mut ctx = EvalContext::new();
    let expr = null_expr();
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Null);
}

// ============================================================================
// Binary arithmetic tests
// ============================================================================

#[test]
fn test_addition_numbers() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Add, num_expr(1), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(3)));
}

#[test]
fn test_addition_strings() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Add, str_expr("hello"), str_expr(" world"));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::String("hello world".to_string()));
}

#[test]
fn test_addition_mixed() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Add, num_expr(1), str_expr("2"));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::String("12".to_string()));
}

#[test]
fn test_subtraction() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Subtract, num_expr(5), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(2)));
}

#[test]
fn test_multiplication() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Multiply, num_expr(4), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(12)));
}

#[test]
fn test_division() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Divide, num_expr(10), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(5)));
}

#[test]
fn test_modulo() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::Modulo, num_expr(7), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(1)));
}

// ============================================================================
// Comparison tests
// ============================================================================

#[test]
fn test_less_than_true() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::LessThan, num_expr(1), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_less_than_false() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::LessThan, num_expr(2), num_expr(1));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_greater_than() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::GreaterThan, num_expr(5), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_less_equal() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::LessThanEqual, num_expr(3), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_greater_equal() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::GreaterThanEqual, num_expr(3), num_expr(5));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

// ============================================================================
// Equality tests
// ============================================================================

#[test]
fn test_strict_equality_numbers() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::StrictlyEqual, num_expr(5), num_expr(5));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_strict_equality_different_types() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::StrictlyEqual, num_expr(5), str_expr("5"));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_strict_inequality() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::StrictlyUnequal, num_expr(5), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_loose_equality_null_undefined() {
    let mut ctx = EvalContext::new();
    // null == null should be true
    let null_expr1 = null_expr();
    let null_expr2 = null_expr();
    let expr = binary_expr(BinaryOperator::LooselyEqual, null_expr1, null_expr2);
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

// ============================================================================
// Unary operator tests
// ============================================================================

#[test]
fn test_unary_minus() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::Minus, num_expr(5));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(-5)));
}

#[test]
fn test_unary_plus() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::Plus, str_expr("42"));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(42)));
}

#[test]
fn test_logical_not_true() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::LogicalNot, bool_expr(true));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_logical_not_false() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::LogicalNot, bool_expr(false));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(true));
}

#[test]
fn test_typeof_number() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::TypeOf, num_expr(42));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::String("number".to_string()));
}

#[test]
fn test_typeof_string() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::TypeOf, str_expr("hello"));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::String("string".to_string()));
}

#[test]
fn test_typeof_boolean() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::TypeOf, bool_expr(true));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::String("boolean".to_string()));
}

#[test]
fn test_void() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::Void, num_expr(42));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Undefined);
}

#[test]
fn test_bitwise_not() {
    let mut ctx = EvalContext::new();
    let expr = unary_expr(UnaryOperator::BitwiseNot, num_expr(5));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(-6)));
}

// ============================================================================
// Logical operator tests
// ============================================================================

#[test]
fn test_logical_and_both_true() {
    let mut ctx = EvalContext::new();
    let expr = logical_expr(LogicalOperator::And, bool_expr(true), num_expr(42));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(42)));
}

#[test]
fn test_logical_and_first_false() {
    let mut ctx = EvalContext::new();
    let expr = logical_expr(LogicalOperator::And, bool_expr(false), num_expr(42));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Boolean(false));
}

#[test]
fn test_logical_or_first_true() {
    let mut ctx = EvalContext::new();
    let expr = logical_expr(LogicalOperator::Or, num_expr(1), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(1)));
}

#[test]
fn test_logical_or_first_false() {
    let mut ctx = EvalContext::new();
    let expr = logical_expr(LogicalOperator::Or, bool_expr(false), num_expr(42));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(42)));
}

// ============================================================================
// Conditional (ternary) operator tests
// ============================================================================

#[test]
fn test_ternary_true() {
    let mut ctx = EvalContext::new();
    let expr = cond_expr(bool_expr(true), num_expr(1), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(1)));
}

#[test]
fn test_ternary_false() {
    let mut ctx = EvalContext::new();
    let expr = cond_expr(bool_expr(false), num_expr(1), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(2)));
}

// ============================================================================
// Bitwise operator tests
// ============================================================================

#[test]
fn test_bitwise_and() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::BitwiseAnd, num_expr(5), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(1))); // 101 & 011 = 001
}

#[test]
fn test_bitwise_or() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::BitwiseOr, num_expr(5), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(7))); // 101 | 011 = 111
}

#[test]
fn test_bitwise_xor() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::BitwiseXor, num_expr(5), num_expr(3));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(6))); // 101 ^ 011 = 110
}

#[test]
fn test_left_shift() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::BitwiseLeftShift, num_expr(1), num_expr(4));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(16)));
}

#[test]
fn test_right_shift() {
    let mut ctx = EvalContext::new();
    let expr = binary_expr(BinaryOperator::BitwiseRightShift, num_expr(16), num_expr(2));
    let result = evaluate_expression(&expr, &mut ctx).unwrap();
    assert_eq!(result, JsValue::Number(JsNumberType::Integer(4)));
}

// ============================================================================
// Type conversion tests (to_boolean)
// ============================================================================

#[test]
fn test_to_boolean_true() {
    assert!(to_boolean(&JsValue::Boolean(true)));
}

#[test]
fn test_to_boolean_false() {
    assert!(!to_boolean(&JsValue::Boolean(false)));
}

#[test]
fn test_to_boolean_zero() {
    assert!(!to_boolean(&JsValue::Number(JsNumberType::Integer(0))));
}

#[test]
fn test_to_boolean_nonzero() {
    assert!(to_boolean(&JsValue::Number(JsNumberType::Integer(42))));
}

#[test]
fn test_to_boolean_empty_string() {
    assert!(!to_boolean(&JsValue::String(String::new())));
}

#[test]
fn test_to_boolean_nonempty_string() {
    assert!(to_boolean(&JsValue::String("hello".to_string())));
}

#[test]
fn test_to_boolean_null() {
    assert!(!to_boolean(&JsValue::Null));
}

#[test]
fn test_to_boolean_undefined() {
    assert!(!to_boolean(&JsValue::Undefined));
}

#[test]
fn test_to_boolean_nan() {
    assert!(!to_boolean(&JsValue::Number(JsNumberType::NaN)));
}
