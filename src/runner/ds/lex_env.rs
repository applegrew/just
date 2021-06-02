use crate::runner::ds::env_record::EnvironmentRecordType;
use std::cell::RefCell;
use std::rc::Rc;

pub type JsLexEnvironmentType = Rc<RefCell<LexEnvironment>>;

pub struct LexEnvironment {
    pub inner: Box<EnvironmentRecordType>,
    pub outer: Option<JsLexEnvironmentType>,
}
