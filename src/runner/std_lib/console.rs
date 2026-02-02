//! Console built-in object.
//!
//! Provides console.log, console.error, console.warn, and console.info methods.

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::{JsValue, JsNumberType};
use crate::runner::plugin::registry::BuiltInRegistry;
use crate::runner::plugin::types::{BuiltInObject, EvalContext};

/// Register the console object with the registry.
pub fn register(registry: &mut BuiltInRegistry) {
    let console = BuiltInObject::new("console")
        .with_no_prototype()
        .add_method("log", console_log)
        .add_method("error", console_error)
        .add_method("warn", console_warn)
        .add_method("info", console_info);

    registry.register_object(console);
}

/// Format a JsValue for console output.
fn format_value(value: &JsValue) -> String {
    match value {
        JsValue::Undefined => "undefined".to_string(),
        JsValue::Null => "null".to_string(),
        JsValue::Boolean(b) => b.to_string(),
        JsValue::Number(n) => match n {
            JsNumberType::Integer(i) => i.to_string(),
            JsNumberType::Float(f) => {
                if f.fract() == 0.0 && f.abs() < 1e15 {
                    format!("{:.0}", f)
                } else {
                    f.to_string()
                }
            }
            JsNumberType::NaN => "NaN".to_string(),
            JsNumberType::PositiveInfinity => "Infinity".to_string(),
            JsNumberType::NegativeInfinity => "-Infinity".to_string(),
        },
        JsValue::String(s) => s.clone(),
        JsValue::Symbol(s) => s.to_string(),
        JsValue::Object(_) => "[object Object]".to_string(),
    }
}

/// Format all arguments for console output.
fn format_args(args: &[JsValue]) -> String {
    args.iter()
        .map(format_value)
        .collect::<Vec<_>>()
        .join(" ")
}

/// console.log - Log to stdout.
fn console_log(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    println!("{}", format_args(&args));
    Ok(JsValue::Undefined)
}

/// console.error - Log to stderr.
fn console_error(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    eprintln!("{}", format_args(&args));
    Ok(JsValue::Undefined)
}

/// console.warn - Log warning to stderr.
fn console_warn(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    eprintln!("Warning: {}", format_args(&args));
    Ok(JsValue::Undefined)
}

/// console.info - Log info to stdout (same as log).
fn console_info(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    println!("{}", format_args(&args));
    Ok(JsValue::Undefined)
}
