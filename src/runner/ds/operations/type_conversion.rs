use crate::runner::ds::error::JErrorType;
use crate::runner::ds::object::ObjectType;
use crate::runner::ds::object_property::PropertyKey;
use crate::runner::ds::operations::object::get_method;
use crate::runner::ds::symbol::SYMBOL_TO_PRIMITIVE;
use crate::runner::ds::value::{JsNumberType, JsValue};

pub const TYPE_STR_UNDEFINED: &str = "undefined";
pub const TYPE_STR_NULL: &str = "null";
pub const TYPE_STR_BOOLEAN: &str = "boolean";
pub const TYPE_STR_STRING: &str = "string";
pub const TYPE_STR_SYMBOL: &str = "symbol";
pub const TYPE_STR_NUMBER: &str = "number";
pub const TYPE_STR_OBJECT: &str = "object";
pub const TYPE_STR_FUNCTION: &str = "function";

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

pub fn to_number(v: &JsValue) -> Result<JsValue, JErrorType> {
    match v {
        JsValue::Undefined => Ok(JsValue::Number(JsNumberType::NaN)),
        JsValue::Null => Ok(JsValue::Number(JsNumberType::Integer(0))),
        JsValue::Boolean(b) => Ok(JsValue::Number(JsNumberType::Integer(match *b {
            true => 1,
            false => 0,
        }))),
        JsValue::String(s) => todo!(),
        JsValue::Symbol(s) => Err(JErrorType::TypeError(format!(
            "'{}' symbol cannot be converted to number",
            s
        ))),
        JsValue::Number(_) => Ok(v.clone()),
        JsValue::Object(o) => {
            let pv = to_primitive(v, PreferredType::Default)?;
            to_number(&pv)
        }
    }
}

pub fn to_unit_32(v: &JsValue) -> Result<u32, JErrorType> {
    let n = to_number(v)?;
    match n {
        JsValue::Number(n) => match n {
            JsNumberType::Integer(i) => Ok((i % (2 ^ 32)) as u32),
            JsNumberType::Float(f) => Ok((f.floor() % (2 ^ 32) as f64) as u32),
            JsNumberType::NaN => Ok(0),
            JsNumberType::PositiveInfinity => Ok(0),
            JsNumberType::NegativeInfinity => Ok(0),
        },
        _ => Err(JErrorType::TypeError(
            "Unexpected type received".to_string(),
        )),
    }
}
