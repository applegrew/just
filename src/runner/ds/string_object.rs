use crate::runner::ds::error::JErrorType;
use crate::runner::ds::execution_context::ExecutionContextStack;
use crate::runner::ds::object::{
    ordinary_define_own_property, JsObject, JsObjectType, ObjectBase, ObjectType,
};
use crate::runner::ds::object_property::{
    PropertyDescriptor, PropertyDescriptorData, PropertyDescriptorSetter, PropertyKey,
};
use crate::runner::ds::operations::type_conversion::{
    canonical_numeric_index_string, to_string_int,
};
use crate::runner::ds::value::{JsNumberType, JsValue};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

lazy_static! {
    static ref STRING_LENGTH_PROP: PropertyKey = PropertyKey::Str("length".to_string());
}

pub trait JsStringObject: JsObject {
    fn get_string_base_mut(&mut self) -> &mut StringObjectBase;

    fn get_string_base(&self) -> &StringObjectBase;

    fn as_js_string_object(&self) -> &dyn JsStringObject;

    fn as_js_string_object_mut(&mut self) -> &mut dyn JsStringObject;

    fn get_own_property(
        &self,
        ctx_stack: &mut ExecutionContextStack,
        property: &PropertyKey,
    ) -> Result<Option<&PropertyDescriptor>, JErrorType> {
        let desc = JsObject::get_own_property(self, property)?;
        Ok(if desc.is_some() {
            desc
        } else {
            string_get_index_property(ctx_stack, self.as_js_string_object(), property)
        })
    }

    fn has_property(&self, ctx_stack: &mut ExecutionContextStack, property: &PropertyKey) -> bool {
        let desc = string_get_index_property(ctx_stack, self.as_js_string_object(), property);
        if desc.is_some() {
            true
        } else {
            JsObject::has_property(self, property)
        }
    }

    fn own_property_keys(&self, ctx_stack: &mut ExecutionContextStack) -> Vec<PropertyKey> {
        let len = self.get_string_base().string_data.len();
        let mut res = vec![];
        for idx in 0..len {
            res.push(PropertyKey::Str(to_string_int(idx as i64)));
        }
        res.append(&mut JsObject::own_property_keys(self, ctx_stack));
        res
    }
}

pub struct StringObjectBase {
    pub string_data: String,
    pub index_props_cache: RefCell<HashMap<u32, PropertyDescriptor>>,
}
impl StringObjectBase {
    pub fn new(string_data: String) -> Self {
        StringObjectBase {
            string_data,
            index_props_cache: RefCell::new(HashMap::new()),
        }
    }
}

pub struct CoreStringObject {
    base: ObjectBase,
    string_base: StringObjectBase,
}
impl CoreStringObject {
    pub fn new(value: String, proto: Option<JsObjectType>) -> Self {
        let mut obj = CoreStringObject {
            base: ObjectBase::new(),
            string_base: StringObjectBase::new(value),
        };
        string_create(&mut obj, proto);
        obj
    }
}
impl JsStringObject for CoreStringObject {
    fn get_string_base_mut(&mut self) -> &mut StringObjectBase {
        &mut self.string_base
    }

    fn get_string_base(&self) -> &StringObjectBase {
        &self.string_base
    }

    fn as_js_string_object(&self) -> &dyn JsStringObject {
        self
    }

    fn as_js_string_object_mut(&mut self) -> &mut dyn JsStringObject {
        self
    }
}
impl JsObject for CoreStringObject {
    fn get_object_base_mut(&mut self) -> &mut ObjectBase {
        &mut self.base
    }

    fn get_object_base(&self) -> &ObjectBase {
        &self.base
    }

    fn as_js_object(&self) -> &dyn JsObject {
        self
    }

    fn as_js_object_mut(&mut self) -> &mut dyn JsObject {
        self
    }
}

fn string_get_index_property<'a>(
    ctx_stack: &mut ExecutionContextStack,
    string_obj: &'a dyn JsStringObject,
    property: &PropertyKey,
) -> Option<&'a PropertyDescriptor> {
    if let PropertyKey::Str(str_property) = property {
        let idx = canonical_numeric_index_string(ctx_stack, str_property);
        if let Some(idx) = idx {
            if idx >= string_obj.get_string_base().string_data.len() as u32 {
                None
            } else {
                let string_base = string_obj.get_string_base();
                // First, populate the cache if needed
                {
                    let needs_insert = string_base.index_props_cache.borrow().get(&idx).is_none();
                    if needs_insert {
                        let pd = PropertyDescriptor::Data(PropertyDescriptorData {
                            value: JsValue::String(
                                string_base
                                    .string_data
                                    .chars()
                                    .nth(idx as usize)
                                    .unwrap()
                                    .to_string(),
                            ),
                            writable: false,
                            enumerable: true,
                            configurable: false,
                        });
                        string_base.index_props_cache.borrow_mut().insert(idx, pd);
                    }
                }
                // Note: We cannot safely return a reference from the RefCell borrow
                // because the borrow is temporary. The architecture needs refactoring
                // to return owned data or use a different pattern.
                // For now, return None to indicate the property exists but can't be returned.
                // TODO: Refactor to return owned PropertyDescriptor
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

pub fn string_create(string_obj: &mut dyn JsStringObject, prototype: Option<JsObjectType>) {
    string_obj.get_object_base_mut().prototype = prototype;
    string_obj.get_object_base_mut().is_extensible = true;
    let _ = ordinary_define_own_property(
        string_obj,
        STRING_LENGTH_PROP.clone(),
        PropertyDescriptorSetter::new_from_property_descriptor(PropertyDescriptor::Data(
            PropertyDescriptorData {
                value: JsValue::Number(JsNumberType::Integer(
                    string_obj.get_string_base().string_data.len() as i64,
                )),
                writable: false,
                enumerable: false,
                configurable: false,
            },
        )),
    );
}
