use crate::runner::ds::env_record::EnvironmentRecordType;
use std::cell::RefCell;
use std::rc::Rc;

pub struct LexEnvironment {
    pub inner: Box<EnvironmentRecordType>,
    pub outer: Option<Rc<RefCell<LexEnvironment>>>,
}
