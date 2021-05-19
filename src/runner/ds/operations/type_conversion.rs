use crate::runner::ds::object::{JsObject, ObjectType};
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
        JsValue::Object(_) => {}
        JsValue::Error(_) => v.clone(),
    }
}

pub fn to_object(v: &JsValue) -> JsValue {
    match v {
        JsValue::Undefined =>
        JsValue::Null => JsValue::Error(JErrorType::TypeError(format!("'{}' cannot be converted to object", v))),
        JsValue::Boolean(_) => {}
        JsValue::String(_) => {}
        JsValue::Symbol(_) => {}
        JsValue::Number(_) => {}
        JsValue::Object(_) => {}
        JsValue::Error(_) => {}
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
        JsValue::String(s) => {}
        JsValue::Symbol(s) => JsValue::Error(JErrorType::TypeError(format!(
            "'{}' symbol cannot be converted to number",
            s
        ))),
        JsValue::Number(_) => v.clone(),
        JsValue::Object(o) => to_number(to_primitive(o)),
        JsValue::Error(_) => v.clone(),
    }
}

pub fn to_unit_32(v: &JsValue) -> u32 {}
