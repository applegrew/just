use crate::runner::ds::env_record::{
    DeclarativeEnvironmentRecord, EnvironmentRecordType, GlobalEnvironmentRecord,
    ObjectEnvironmentRecord,
};
use crate::runner::ds::lex_env::LexEnvironment;
use crate::runner::ds::misc::IdentifierReference;
use crate::runner::ds::object::ObjectType;
use crate::runner::ds::value::JsValue;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub fn get_identifier_reference<'a>(
    lex: Option<Rc<RefCell<LexEnvironment>>>,
    name: String,
) -> IdentifierReference<'a> {
    if let Some(lex) = lex {
        if lex
            .borrow()
            .borrow()
            .inner
            .borrow()
            .resolve_to_env_record()
            .has_binding(&name)
        {
            IdentifierReference {
                name,
                value: Some(lex.inner.borrow()),
            }
        } else {
            match &lex.borrow().borrow().outer {
                None => get_identifier_reference(None, name),
                Some(l) => get_identifier_reference(Some(l.clone()), name),
            }
        }
    } else {
        IdentifierReference { name, value: None }
    }
}

pub fn new_declarative_environment(
    outer_lex: Option<Rc<RefCell<LexEnvironment>>>,
) -> Rc<RefCell<LexEnvironment>> {
    Rc::new(RefCell::new(LexEnvironment {
        inner: Box::new(EnvironmentRecordType::Declarative(
            DeclarativeEnvironmentRecord::new(),
        )),
        outer: match outer_lex {
            None => None,
            Some(o) => Some(o.clone()),
        },
    }))
}

pub fn new_object_environment(
    o: Box<ObjectType>,
    outer_lex: Option<Rc<RefCell<LexEnvironment>>>,
) -> Rc<RefCell<LexEnvironment>> {
    Rc::new(RefCell::new(LexEnvironment {
        inner: Box::new(EnvironmentRecordType::Object(ObjectEnvironmentRecord::new(
            o,
        ))),
        outer: match outer_lex {
            None => None,
            Some(o) => Some(o.clone()),
        },
    }))
}

pub fn new_function_environment(F: x, newTarget: y) {}

pub fn new_global_environment(global_object: Box<ObjectType>) -> Rc<RefCell<LexEnvironment>> {
    Rc::new(RefCell::new(LexEnvironment {
        inner: Box::new(EnvironmentRecordType::Global(GlobalEnvironmentRecord::new(
            global_object,
        ))),
        outer: None,
    }))
}
