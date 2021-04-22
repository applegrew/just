use crate::parser::ast::{FunctionData, IdentifierData};
use std::cell::RefCell;
use std::collections::HashMap;

pub struct JsExecutionError {}

pub struct JsScope<'a> {
    pub parent_ctx: Option<RefCell<JsScope<'a>>>,
    pub values: HashMap<&'a IdentifierData, Option<JsObject<'a>>>,
}

pub struct Lambda<'a> {
    pub def: &'a FunctionData,
    pub scope: &'a Box<JsScope<'a>>,
}

pub enum JsObject<'a> {
    Null,
    Function(Lambda<'a>),
    Arguments(Box<Vec<JsObject<'a>>>),
    This(Option<Box<JsObject<'a>>>),
}
