use crate::runner::ds::env_record::{
    DeclarativeEnvironmentRecord, EnvironmentRecordType, GlobalEnvironmentRecord,
    ObjectEnvironmentRecord,
};
use crate::runner::ds::lex_env::LexEnvironment;
use crate::runner::ds::misc::IdentifierReference;
use crate::runner::ds::object::{JsObjectType, ObjectType};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

pub fn get_identifier_reference<'a>(
    lex: Option<Rc<RefCell<LexEnvironment>>>,
    name: String,
) -> IdentifierReference<'a> {
    // if let Some(lex) = lex {
    //     let lex = (*lex).borrow();
    //     if lex.inner.resolve_to_env_record().has_binding(&name) {
    //         IdentifierReference {
    //             name,
    //             value: Some(lex.inner.borrow()),
    //         }
    //     } else {
    //         match &lex.outer {
    //             None => get_identifier_reference(None, name),
    //             Some(l) => get_identifier_reference(Some(l.clone()), name),
    //         }
    //     }
    // } else {
    //     IdentifierReference { name, value: None }
    // }
    todo!()
}
