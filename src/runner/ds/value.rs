use crate::runner::ds::object::{JsObject, ObjectType};
use crate::runner::ds::operations::type_conversion::{TYPE_STR_NULL, TYPE_STR_UNDEFINED};
use crate::runner::ds::symbol::SymbolData;
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub enum JsValue {
    Undefined,
    Null,
    Boolean(bool),
    String(String),
    Symbol(SymbolData),
    Number(JsNumberType),
    Object(Rc<RefCell<ObjectType>>),
    Error(JErrorType),
}
impl Clone for JsValue {
    fn clone(&self) -> Self {
        match self {
            JsValue::Undefined => JsValue::Undefined,
            JsValue::String(d) => JsValue::String(d.to_string()),
            JsValue::Boolean(d) => JsValue::Boolean(*d),
            JsValue::Null => JsValue::Null,
            JsValue::Number(d) => unimplemented!(),
            JsValue::Object(o) => JsValue::Object(o.clone()),
            JsValue::Symbol(d) => unimplemented!(),
            JsValue::Error(e) => JsValue::Error(JErrorType::new_copy(e)),
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
                JsValue::Object(o) => (**o).borrow().to_string(),
                JsValue::Error(e) => e.to_string(),
            }
        )
    }
}

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

pub enum JErrorType {
    ReferenceError(String),
    TypeError(String),
}
impl JErrorType {
    pub fn new_copy(other: &Self) -> Self {
        match other {
            JErrorType::ReferenceError(m) => JErrorType::ReferenceError(m.to_string()),
            JErrorType::TypeError(m) => JErrorType::TypeError(m.to_string()),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            JErrorType::ReferenceError(m) => format!("Uncaught reference error: {}.", m),
            JErrorType::TypeError(m) => format!("Uncaught type error: {}.", m),
        }
    }
}
