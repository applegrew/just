use crate::runner::ds::object::{JsObject, ObjectType};
use crate::runner::ds::object_property::PropertyKey;
use crate::runner::ds::operations::object::get_method;
use crate::runner::ds::symbol::SYMBOL_TO_PRIMITIVE;
use crate::runner::ds::value::{JErrorType, JsNumberType, JsValue};
use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::ops::Deref;
use std::rc::Rc;

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
        JsValue::Object(o) => match o.as_ref().borrow().deref() {
            ObjectType::Ordinary(_) => TYPE_STR_OBJECT,
            ObjectType::Function(_) => TYPE_STR_FUNCTION,
        },
        JsValue::Error(_) => TYPE_STR_OBJECT,
    }
}

pub enum PreferredType {
    Default,
    String,
    Number,
}

pub fn to_primitive(v: &JsValue, preferred_type: PreferredType) -> JsValue {
    match v {
        JsValue::Undefined => v.clone(),
        JsValue::Null => v.clone(),
        JsValue::Boolean(_) => v.clone(),
        JsValue::String(_) => v.clone(),
        JsValue::Symbol(_) => v.clone(),
        JsValue::Number(_) => v.clone(),
        JsValue::Object(_) => {
            let m = get_method(v, &PropertyKey::Sym(SYMBOL_TO_PRIMITIVE));
            todo!()
        }
        JsValue::Error(_) => v.clone(),
    }
}

pub fn to_object(v: &JsValue) -> JsValue {
    match v {
        JsValue::Undefined => JsValue::Error(JErrorType::TypeError(format!(
            "'{}' cannot be converted to object",
            v
        ))),
        JsValue::Null => JsValue::Error(JErrorType::TypeError(format!(
            "'{}' cannot be converted to object",
            v
        ))),
        JsValue::Boolean(_) => todo!(),
        JsValue::String(_) => todo!(),
        JsValue::Symbol(_) => todo!(),
        JsValue::Number(_) => todo!(),
        JsValue::Object(_) => v.clone(),
        JsValue::Error(_) => v.clone(),
    }
}

pub fn to_number(v: &JsValue) -> JsValue {
    match v {
        JsValue::Undefined => JsValue::Number(JsNumberType::NaN),
        JsValue::Null => JsValue::Number(JsNumberType::Integer(0)),
        JsValue::Boolean(b) => JsValue::Number(JsNumberType::Integer(match *b {
            true => 1,
            false => 0,
        })),
        JsValue::String(s) => todo!(),
        JsValue::Symbol(s) => JsValue::Error(JErrorType::TypeError(format!(
            "'{}' symbol cannot be converted to number",
            s
        ))),
        JsValue::Number(_) => v.clone(),
        JsValue::Object(o) => to_number(&to_primitive(v, PreferredType::Default)),
        JsValue::Error(_) => v.clone(),
    }
}

pub fn to_unit_32(v: &JsValue) -> Result<u32, JErrorType> {
    let n = to_number(v);
    match n {
        JsValue::Number(n) => match n {
            JsNumberType::Integer(i) => Ok((i % (2 ^ 32)) as u32),
            JsNumberType::Float(f) => Ok((f.floor() % (2 ^ 32) as f64) as u32),
            JsNumberType::NaN => Ok(0),
            JsNumberType::PositiveInfinity => Ok(0),
            JsNumberType::NegativeInfinity => Ok(0),
        },
        JsValue::Error(e) => Err(e),
        _ => Err(JErrorType::TypeError(
            "Unexpected type received".to_string(),
        )),
    }
}
