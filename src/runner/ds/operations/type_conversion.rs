use crate::parser::ast::{ExtendedNumberLiteralType, NumberLiteralType};
use crate::parser::JsParser;
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::object::ObjectType;
use crate::runner::ds::object_property::PropertyKey;
use crate::runner::ds::operations::object::get_method;
use crate::runner::ds::symbol::SYMBOL_TO_PRIMITIVE;
use crate::runner::ds::value::{JsNumberType, JsValue};

pub const TYPE_STR_UNDEFINED: &str = "undefined";
pub const TYPE_STR_NULL: &str = "null";
pub const TYPE_STR_BOOLEAN: &str = "boolean";
pub const TYPE_STR_BOOLEAN_TRUE: &str = "true";
pub const TYPE_STR_BOOLEAN_FALSE: &str = "false";
pub const TYPE_STR_STRING: &str = "string";
pub const TYPE_STR_SYMBOL: &str = "symbol";
pub const TYPE_STR_NUMBER: &str = "number";
pub const TYPE_STR_OBJECT: &str = "object";
pub const TYPE_STR_FUNCTION: &str = "function";
pub const TYPE_STR_NUMBER_NAN: &str = "NaN";
pub const TYPE_STR_NUMBER_INFINITY: &str = "Infinity";

pub fn get_type(a: &JsValue) -> &'static str {
    match a {
        JsValue::Undefined => TYPE_STR_UNDEFINED,
        JsValue::Null => TYPE_STR_NULL,
        JsValue::Boolean(_) => TYPE_STR_BOOLEAN,
        JsValue::String(_) => TYPE_STR_STRING,
        JsValue::Symbol(_) => TYPE_STR_SYMBOL,
        JsValue::Number(_) => TYPE_STR_NUMBER,
        JsValue::Object(o) => match *(**o).borrow() {
            ObjectType::Ordinary(_) => TYPE_STR_OBJECT,
            ObjectType::Function(_) => TYPE_STR_FUNCTION,
            ObjectType::Array(_) => TYPE_STR_OBJECT,
        },
    }
}

pub enum PreferredType {
    Default,
    String,
    Number,
}

pub fn to_primitive(v: &JsValue, preferred_type: PreferredType) -> Result<JsValue, JErrorType> {
    match v {
        JsValue::Undefined => Ok(v.clone()),
        JsValue::Null => Ok(v.clone()),
        JsValue::Boolean(_) => Ok(v.clone()),
        JsValue::String(_) => Ok(v.clone()),
        JsValue::Symbol(_) => Ok(v.clone()),
        JsValue::Number(_) => Ok(v.clone()),
        JsValue::Object(_) => {
            let m = get_method(v, &PropertyKey::Sym(SYMBOL_TO_PRIMITIVE.clone()));
            todo!()
        }
    }
}

pub fn to_object(v: &JsValue) -> Result<JsValue, JErrorType> {
    match v {
        JsValue::Undefined => Err(JErrorType::TypeError(format!(
            "'{}' cannot be converted to object",
            v
        ))),
        JsValue::Null => Err(JErrorType::TypeError(format!(
            "'{}' cannot be converted to object",
            v
        ))),
        JsValue::Boolean(_) => todo!(),
        JsValue::String(_) => todo!(),
        JsValue::Symbol(_) => todo!(),
        JsValue::Number(_) => todo!(),
        JsValue::Object(_) => Ok(v.clone()),
    }
}

pub fn to_number(v: &JsValue) -> Result<JsNumberType, JErrorType> {
    match v {
        JsValue::Undefined => Ok(JsNumberType::NaN),
        JsValue::Null => Ok(JsNumberType::Integer(0)),
        JsValue::Boolean(b) => Ok(JsNumberType::Integer(match *b {
            true => 1,
            false => 0,
        })),
        JsValue::String(s) => Ok(parse_string_to_number(s, false)),
        JsValue::Symbol(s) => Err(JErrorType::TypeError(format!(
            "'{}' symbol cannot be converted to number",
            s
        ))),
        JsValue::Number(n) => Ok(n.clone()),
        JsValue::Object(o) => {
            let pv = to_primitive(v, PreferredType::Default)?;
            to_number(&pv)
        }
    }
}

fn parse_string_to_number(s: &String, is_nan_mode: bool) -> JsNumberType {
    let res = JsParser::parse_numeric_string(s, is_nan_mode);
    match res {
        Ok(e) => match e {
            ExtendedNumberLiteralType::Std(n) => match n {
                NumberLiteralType::IntegerLiteral(i) => JsNumberType::Integer(i),
                NumberLiteralType::FloatLiteral(f) => JsNumberType::Float(f),
            },
            ExtendedNumberLiteralType::Infinity => JsNumberType::PositiveInfinity,
            ExtendedNumberLiteralType::NegativeInfinity => JsNumberType::NegativeInfinity,
        },
        Err(_) => {
            if is_nan_mode {
                JsNumberType::NaN
            } else {
                JsNumberType::Integer(0)
            }
        }
    }
}

pub fn to_unit_32(v: &JsValue) -> Result<u32, JErrorType> {
    let n = to_number(v)?;
    Ok(to_unit32_from_js_number_type(&n))
}

fn to_unit32_from_js_number_type(n: &JsNumberType) -> u32 {
    match n {
        JsNumberType::Integer(i) => (*i % (2 ^ 32)) as u32,
        JsNumberType::Float(f) => (f.floor() % (2 ^ 32) as f64) as u32,
        JsNumberType::NaN => 0,
        JsNumberType::PositiveInfinity => 0,
        JsNumberType::NegativeInfinity => 0,
    }
}

pub fn to_string(v: &JsValue) -> Result<String, JErrorType> {
    match v {
        JsValue::Undefined => Ok(TYPE_STR_UNDEFINED.to_string()),
        JsValue::Null => Ok(TYPE_STR_NULL.to_string()),
        JsValue::Boolean(b) => Ok(if *b {
            TYPE_STR_BOOLEAN_TRUE.to_string()
        } else {
            TYPE_STR_BOOLEAN_FALSE.to_string()
        }),
        JsValue::String(s) => Ok(s.to_string()),
        JsValue::Symbol(s) => Err(JErrorType::TypeError(format!("'{}' is a symbol", s))),
        JsValue::Number(n) => Ok(match n {
            JsNumberType::Integer(i) => to_string_int(*i),
            JsNumberType::Float(f) => format!("{}", f),
            JsNumberType::NaN => TYPE_STR_NUMBER_NAN.to_string(),
            JsNumberType::PositiveInfinity => TYPE_STR_NUMBER_INFINITY.to_string(),
            JsNumberType::NegativeInfinity => format!("-{}", TYPE_STR_NUMBER_INFINITY),
        }),
        JsValue::Object(_) => to_string(&to_primitive(v, PreferredType::String)?),
    }
}

pub fn to_string_int(i: i64) -> String {
    format!("{}", i)
}

pub fn canonical_numeric_index_string(s: &String) -> Option<u32> {
    if s == "-0" {
        Some(0)
    } else {
        let n = parse_string_to_number(s, false);
        let v = JsValue::Number(n);
        if let Ok(in_s) = to_string(&v) {
            if &in_s == s {
                if let JsValue::Number(n) = v {
                    return Some(to_unit32_from_js_number_type(&n));
                }
            }
        }
        None
    }
}
