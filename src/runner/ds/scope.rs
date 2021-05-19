use crate::parser::ast::IdentifierData;
use crate::runner::ds::{ErrorKind, JErrorType, JsObject, ScopeId};
use std::collections::HashMap;

pub struct JsScope<'a> {
    parent_ctx_id: Option<ScopeId>,
    id: ScopeId,
    bindings: HashMap<String, Option<JsObject<'a>>>,
    binding_flags: HashMap<String, Vec<BindingFlag>>,
    has_this: bool,
    has_super: bool,
}
