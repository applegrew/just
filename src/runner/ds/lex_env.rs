use crate::runner::ds::env_record::EnvironmentRecordType;
use std::cell::RefCell;
use std::rc::Rc;

pub struct LexEnvironment<'code> {
    pub inner: Box<EnvironmentRecordType<'code>>,
    pub outer: Option<Rc<RefCell<LexEnvironment<'code>>>>,
}
