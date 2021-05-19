use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::runner::ds::env_record::EnvironmentRecordType;
use crate::runner::ds::value::JsValue;
use just::parser::ast::{
    FunctionBodyData, FunctionData, IdentifierData, LiteralData, LiteralType, Meta, PatternType,
};

pub struct IdentifierReference<'a> {
    pub name: String,
    pub value: Option<&'a EnvironmentRecordType>,
}

pub struct JsExecutionError {
    pub meta: Meta,
    pub message: String,
}

pub type ScopeId = usize;

pub const GLOBAL_SCOPE_ID: ScopeId = 0;

pub struct JsEnvironment<'a> {
    scope_id_counter: ScopeId,
    all_scopes: HashMap<ScopeId, JsScope<'a>>,
}

impl<'a> JsEnvironment<'a> {
    pub fn new() -> Self {
        let mut env = JsEnvironment {
            scope_id_counter: 1,
            all_scopes: HashMap::new(),
        };
        env.all_scopes.insert(
            GLOBAL_SCOPE_ID,
            JsScope {
                id: GLOBAL_SCOPE_ID,
                parent_ctx_id: None,
                values: HashMap::new(),
            },
        );
        env
    }

    pub fn new_scope(&mut self, parent_id: ScopeId) -> ScopeId {
        let id = self.scope_id_counter + 1;
        self.scope_id_counter = id;
        let scope: JsScope<'a> = JsScope {
            id,
            parent_ctx_id: Some(parent_id),
            values: HashMap::new(),
        };
        self.all_scopes.borrow_mut().insert(id, scope);
        id
    }

    pub fn get_scope_mut(&mut self, id: ScopeId) -> Option<&mut JsScope<'a>> {
        self.all_scopes.get_mut(&id)
    }

    pub fn get_scope(&self, id: ScopeId) -> Option<&JsScope<'a>> {
        self.all_scopes.get(&id)
    }

    pub fn take_scope(&mut self, id: ScopeId) -> Option<JsScope> {
        self.all_scopes.remove(&id)
    }

    pub fn put_back_scope(&mut self, scope: JsScope<'a>) {
        self.all_scopes.insert(scope.id, scope);
    }
}
