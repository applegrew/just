use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

use crate::runner::ds::object::JsObjectType;
use crate::runner::ds::operations::type_conversion::{TYPE_STR_NULL, TYPE_STR_UNDEFINED};
use crate::runner::ds::symbol::SymbolData;

pub enum JsValue {
    Undefined,
    Null,
    Boolean(bool),
    String(String),
    Symbol(SymbolData),
    Number(JsNumberType),
    Object(JsObjectType),
}
impl Clone for JsValue {
    fn clone(&self) -> Self {
        match self {
            JsValue::Undefined => JsValue::Undefined,
            JsValue::String(d) => JsValue::String(d.to_string()),
            JsValue::Boolean(d) => JsValue::Boolean(*d),
            JsValue::Null => JsValue::Null,
            JsValue::Number(d) => JsValue::Number(d.clone()),
            JsValue::Object(o) => JsValue::Object(o.clone()),
            JsValue::Symbol(d) => JsValue::Symbol(d.clone()),
        }
    }
}
impl Display for JsValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                JsValue::Undefined => TYPE_STR_UNDEFINED.to_string(),
                JsValue::Null => TYPE_STR_NULL.to_string(),
                JsValue::Boolean(b) => format!("bool({})", b),
                JsValue::String(s) => format!("\"{}\"", s),
                JsValue::Symbol(s) => s.to_string(),
                JsValue::Number(n) => n.to_string(),
                JsValue::Object(o) => (**o).borrow().as_js_object().to_string(),
            }
        )
    }
}

impl fmt::Debug for JsValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            JsValue::Undefined => write!(f, "JsValue::Undefined"),
            JsValue::Null => write!(f, "JsValue::Null"),
            JsValue::Boolean(b) => write!(f, "JsValue::Boolean({})", b),
            JsValue::String(s) => write!(f, "JsValue::String({:?})", s),
            JsValue::Symbol(s) => write!(f, "JsValue::Symbol({})", s),
            JsValue::Number(n) => write!(f, "JsValue::Number({:?})", n),
            JsValue::Object(_) => write!(f, "JsValue::Object(...)"),
        }
    }
}

impl PartialEq for JsValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JsValue::Undefined, JsValue::Undefined) => true,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
            (JsValue::String(a), JsValue::String(b)) => a == b,
            (JsValue::Number(a), JsValue::Number(b)) => a == b,
            (JsValue::Object(a), JsValue::Object(b)) => Rc::ptr_eq(a, b),
            (JsValue::Symbol(a), JsValue::Symbol(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum JsNumberType {
    Integer(i64),
    Float(f64),
    NaN,
    PositiveInfinity,
    NegativeInfinity,
}
impl Display for JsNumberType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            JsNumberType::Integer(i) => write!(f, "{}", i),
            JsNumberType::Float(nf) => write!(f, "{}", nf),
            JsNumberType::NaN => write!(f, "NaN"),
            JsNumberType::PositiveInfinity => write!(f, "+Infinity"),
            JsNumberType::NegativeInfinity => write!(f, "-Infinity"),
        }
    }
}
impl Clone for JsNumberType {
    fn clone(&self) -> Self {
        match self {
            JsNumberType::Integer(i) => JsNumberType::Integer(*i),
            JsNumberType::Float(nf) => JsNumberType::Float(*nf),
            JsNumberType::NaN => JsNumberType::NaN,
            JsNumberType::PositiveInfinity => JsNumberType::PositiveInfinity,
            JsNumberType::NegativeInfinity => JsNumberType::NegativeInfinity,
        }
    }
}

pub enum JsValueOrSelf<'a> {
    ValueRef(&'a JsValue),
    SelfValue,
}
