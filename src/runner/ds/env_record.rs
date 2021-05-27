use std::collections::HashMap;

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::function_object::JsFunctionObject;
use crate::runner::ds::object::ObjectType;
use crate::runner::ds::value::JsValue;

pub trait EnvironmentRecord {
    fn has_binding(&self, name: &String) -> bool;
    fn create_mutable_binding(&mut self, name: String, can_delete: bool);
    fn create_immutable_binding(&mut self, name: String);
    fn initialize_binding(&mut self, name: String, value: JsValue) -> bool;
    fn set_mutable_binding(&mut self, name: String, value: JsValue) -> Option<JErrorType>;
    fn get_binding_value(&self, name: &String) -> Result<&JsValue, JErrorType>;
    fn delete_binding(&mut self, name: &String) -> bool;
    fn has_this_binding(&self) -> bool;
    fn has_super_binding(&self) -> bool;
}

pub enum EnvironmentRecordType {
    Declarative(DeclarativeEnvironmentRecord),
    Object(ObjectEnvironmentRecord),
    Function(FunctionEnvironmentRecord),
    Global(GlobalEnvironmentRecord),
}

impl EnvironmentRecordType {
    pub fn resolve_to_env_record(&self) -> &dyn EnvironmentRecord {
        match self {
            EnvironmentRecordType::Declarative(d) => d,
            EnvironmentRecordType::Object(d) => d,
            EnvironmentRecordType::Function(d) => d,
            EnvironmentRecordType::Global(d) => d,
        }
    }
}

#[derive(PartialEq)]
pub enum BindingFlag {
    NoDelete,
    IsImmutable,
}

pub struct DeclarativeEnvironmentRecord {
    bindings: HashMap<String, Option<JsValue>>,
    binding_flags: HashMap<String, Vec<BindingFlag>>,
}

impl DeclarativeEnvironmentRecord {
    pub(crate) fn new() -> Self {
        DeclarativeEnvironmentRecord {
            bindings: HashMap::new(),
            binding_flags: HashMap::new(),
        }
    }
}

impl EnvironmentRecord for DeclarativeEnvironmentRecord {
    fn has_binding(&self, id: &String) -> bool {
        self.bindings.contains_key(id)
    }

    fn create_mutable_binding(&mut self, id: String, can_delete: bool) {
        if !self.has_binding(&id) {
            self.bindings.insert(id.to_string(), None);
            if !can_delete {
                self.binding_flags.insert(id, vec![BindingFlag::NoDelete]);
            }
        }
    }

    fn create_immutable_binding(&mut self, id: String) {
        if !self.has_binding(&id) {
            self.bindings.insert(id.to_string(), None);
            self.binding_flags
                .insert(id, vec![BindingFlag::IsImmutable]);
        }
    }

    fn initialize_binding(&mut self, id: String, value: JsValue) -> bool {
        if let Some(v) = self.bindings.get(&id) {
            if v.is_none() {
                self.bindings.insert(id, Some(value));
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn set_mutable_binding(&mut self, id: String, value: JsValue) -> Option<JErrorType> {
        if let Some(v) = self.bindings.get(&id) {
            if !v.is_none() {
                if !self
                    .binding_flags
                    .get(&id)
                    .unwrap()
                    .contains(&BindingFlag::IsImmutable)
                {
                    self.bindings.insert(id, Some(value));
                    None
                } else {
                    Some(JErrorType::TypeError(format!(
                        "'{}' is set and immutable",
                        id
                    )))
                }
            } else {
                Some(JErrorType::ReferenceError(format!(
                    "'{}' is not initialized",
                    id
                )))
            }
        } else {
            Some(JErrorType::ReferenceError(format!(
                "'{}' is not defined",
                id
            )))
        }
    }

    fn get_binding_value(&self, id: &String) -> Result<&JsValue, JErrorType> {
        match self.bindings.get(id) {
            None => Err(JErrorType::ReferenceError(format!(
                "'{}' is not defined",
                id
            ))),
            Some(v) => match v {
                None => Err(JErrorType::ReferenceError(format!(
                    "'{}' is not initialized",
                    id
                ))),
                Some(v) => Ok(v),
            },
        }
    }

    fn delete_binding(&mut self, id: &String) -> bool {
        if let Some(flags) = self.binding_flags.get(id) {
            if flags.contains(&BindingFlag::NoDelete) {
                false
            } else {
                self.bindings.remove(id);
                self.binding_flags.remove(id);
                true
            }
        } else {
            false
        }
    }

    fn has_this_binding(&self) -> bool {
        false
    }

    fn has_super_binding(&self) -> bool {
        false
    }
}

pub struct ObjectEnvironmentRecord {
    binding_object: Box<ObjectType>,
}

impl ObjectEnvironmentRecord {
    pub fn new(o: Box<ObjectType>) -> Self {
        ObjectEnvironmentRecord { binding_object: o }
    }
}

impl EnvironmentRecord for ObjectEnvironmentRecord {
    fn has_binding(&self, name: &String) -> bool {
        todo!()
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) {
        todo!()
    }

    fn create_immutable_binding(&mut self, name: String) {
        todo!()
    }

    fn initialize_binding(&mut self, name: String, value: JsValue) -> bool {
        todo!()
    }

    fn set_mutable_binding(&mut self, name: String, value: JsValue) -> Option<JErrorType> {
        todo!()
    }

    fn get_binding_value(&self, name: &String) -> Result<&JsValue, JErrorType> {
        todo!()
    }

    fn delete_binding(&mut self, name: &String) -> bool {
        todo!()
    }

    fn has_this_binding(&self) -> bool {
        todo!()
    }

    fn has_super_binding(&self) -> bool {
        todo!()
    }
}

pub struct FunctionEnvironmentRecord {
    base_env: DeclarativeEnvironmentRecord,
    this_value: Option<JsValue>,
    is_lexical_binding: bool,
    function_object: Box<dyn JsFunctionObject>,
    home_object: Option<ObjectType>,
    new_target: Option<Box<dyn JsFunctionObject>>,
}
impl FunctionEnvironmentRecord {
    // pub fn new(o: Box<ObjectType>) -> Self {}

    pub fn bind_this_value(&mut self, this: JsValue) -> Result<bool, JErrorType> {
        if !self.is_lexical_binding {
            if let Some(_) = &self.this_value {
                Err(JErrorType::ReferenceError(
                    "'this' is already initialized".to_string(),
                ))
            } else {
                self.this_value = Some(this);
                Ok(true)
            }
        } else {
            Err(JErrorType::TypeError(
                "Cannot set 'this' of Arrow Function".to_string(),
            ))
        }
    }

    pub fn get_this_binding(&self) -> Result<&JsValue, JErrorType> {
        if self.is_lexical_binding {
            Err(JErrorType::TypeError(
                "Cannot get 'this' of Arrow Function".to_string(),
            ))
        } else {
            if let Some(this) = &self.this_value {
                Ok(this)
            } else {
                Err(JErrorType::ReferenceError(
                    "'this' is not initialized".to_string(),
                ))
            }
        }
    }

    pub fn get_super_base(&self) -> Option<&ObjectType> {
        todo!()
    }

    pub fn has_this_binding(&self) -> bool {
        !self.is_lexical_binding
    }

    pub fn has_super_binding(&self) -> bool {
        if self.is_lexical_binding {
            false
        } else {
            self.home_object.is_some()
        }
    }
}
impl EnvironmentRecord for FunctionEnvironmentRecord {
    fn has_binding(&self, name: &String) -> bool {
        todo!()
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) {
        todo!()
    }

    fn create_immutable_binding(&mut self, name: String) {
        todo!()
    }

    fn initialize_binding(&mut self, name: String, value: JsValue) -> bool {
        todo!()
    }

    fn set_mutable_binding(&mut self, name: String, value: JsValue) -> Option<JErrorType> {
        todo!()
    }

    fn get_binding_value(&self, name: &String) -> Result<&JsValue, JErrorType> {
        todo!()
    }

    fn delete_binding(&mut self, name: &String) -> bool {
        todo!()
    }

    fn has_this_binding(&self) -> bool {
        todo!()
    }

    fn has_super_binding(&self) -> bool {
        todo!()
    }
}

pub struct GlobalEnvironmentRecord {
    object_record: ObjectEnvironmentRecord,
    declarative_record: DeclarativeEnvironmentRecord,
    var_names: Vec<String>,
}

impl GlobalEnvironmentRecord {
    pub fn new(global_object: Box<ObjectType>) -> Self {
        GlobalEnvironmentRecord {
            object_record: ObjectEnvironmentRecord::new(global_object),
            declarative_record: DeclarativeEnvironmentRecord::new(),
            var_names: Vec::new(),
        }
    }

    pub fn get_this_binding(&self) -> &ObjectType {
        &self.object_record.binding_object
    }

    pub fn has_var_declaration(&self, name: &String) -> bool {
        self.var_names.contains(name)
    }

    pub fn has_lexical_declaration(&self, name: &String) -> bool {
        self.declarative_record.has_binding(name)
    }

    pub fn has_restricted_global_property(&self, name: &String) -> bool {
        todo!()
    }

    pub fn can_declare_global_var(&self, name: &String) -> bool {
        todo!()
    }

    pub fn can_declare_global_function(&self, name: &String) -> bool {
        todo!()
    }

    pub fn create_global_var_binding(&self, name: String, can_delete: bool) {
        todo!()
    }

    pub fn create_global_function_binding(&self, name: String, f: JsValue, can_delete: bool) {
        todo!()
    }
}
impl EnvironmentRecord for GlobalEnvironmentRecord {
    fn has_binding(&self, name: &String) -> bool {
        todo!()
    }

    fn create_mutable_binding(&mut self, name: String, can_delete: bool) {
        todo!()
    }

    fn create_immutable_binding(&mut self, name: String) {
        todo!()
    }

    fn initialize_binding(&mut self, name: String, value: JsValue) -> bool {
        todo!()
    }

    fn set_mutable_binding(&mut self, name: String, value: JsValue) -> Option<JErrorType> {
        todo!()
    }

    fn get_binding_value(&self, name: &String) -> Result<&JsValue, JErrorType> {
        todo!()
    }

    fn delete_binding(&mut self, name: &String) -> bool {
        todo!()
    }

    fn has_this_binding(&self) -> bool {
        todo!()
    }

    fn has_super_binding(&self) -> bool {
        todo!()
    }
}
