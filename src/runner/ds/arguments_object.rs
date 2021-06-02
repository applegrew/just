use crate::runner::ds::error::JErrorType;
use crate::runner::ds::object::{
    ordinary_define_own_property, JsObject, JsObjectType, ObjectBase, ObjectType,
};
use crate::runner::ds::object_property::{
    PropertyDescriptor, PropertyDescriptorAccessor, PropertyDescriptorData,
    PropertyDescriptorSetter, PropertyKey,
};
use crate::runner::ds::operations::object::{
    create_data_property, define_property_or_throw, get, has_own_property, set, set_from_js_object,
};
use crate::runner::ds::operations::test_and_comparison::{
    same_js_object, same_value, same_value_with_js_object,
};
use crate::runner::ds::operations::type_conversion::to_string_int;
use crate::runner::ds::realm::{CodeRealm, WellKnownIntrinsics};
use crate::runner::ds::symbol::SYMBOL_ITERATOR;
use crate::runner::ds::value::{JsNumberType, JsValue, JsValueOrSelf};
use std::cell::RefCell;
use std::panic::resume_unwind;
use std::rc::Rc;
use crate::runner::ds::env_record::EnvironmentRecordType;

pub struct ArgumentsBaseObject {
    pub parameter_map: Option<JsObjectType>,
}
impl ArgumentsBaseObject {
    pub fn new() -> Self {
        ArgumentsBaseObject {
            parameter_map: None,
        }
    }
}

pub trait JsArgumentsObject: JsObject {
    fn get_argument_base_object(&self) -> &ArgumentsBaseObject;

    fn get_argument_base_object_mut(&mut self) -> &mut ArgumentsBaseObject;

    fn get_own_property(
        &self,
        property: &PropertyKey,
    ) -> Result<Option<&PropertyDescriptor>, JErrorType> {
        let desc = JsObject::get_own_property(self, property)?;
        if let Some(_) = desc {
            if let Some(parameter_map) = &self.get_argument_base_object().parameter_map {
                return (*(**parameter_map).borrow())
                    .as_js_object()
                    .get_own_property(property);
            }
            Ok(None)
        } else {
            Ok(desc)
        }
    }

    fn define_own_property(
        &mut self,
        property: PropertyKey,
        descriptor_setter: PropertyDescriptorSetter,
    ) -> Result<bool, JErrorType> {
        let is_accessor_desc = descriptor_setter.descriptor.is_accessor_descriptor();
        let is_value_present = descriptor_setter.honour_value;
        let (value, is_writable) =
            if let PropertyDescriptor::Data(PropertyDescriptorData {
                writable, value, ..
            }) = &descriptor_setter.descriptor
            {
                (value, *writable)
            } else {
                (&JsValue::Undefined, false)
            };
        let is_writable_and_is_false = descriptor_setter.honour_writable && is_writable;
        if ordinary_define_own_property(self.as_js_object_mut(), property, descriptor_setter)? {
            if let Some(parameter_map) = &self.get_argument_base_object().parameter_map {
                if has_own_property(parameter_map, &property)? {
                    let parameter_map = &mut *(**parameter_map).borrow_mut().as_js_object_mut();
                    if is_accessor_desc {
                        parameter_map.delete(&property);
                    } else {
                        if is_value_present {
                            set_from_js_object(parameter_map, property.clone(), value.clone());
                        }
                        if is_writable_and_is_false {
                            parameter_map.delete(&property);
                        }
                    }
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn get(&self, property: &PropertyKey, receiver: JsValueOrSelf) -> Result<JsValue, JErrorType> {
        if let Some(parameter_map) = &self.get_argument_base_object().parameter_map {
            if has_own_property(parameter_map, &property)? {
                return get(parameter_map, property);
            }
        }
        JsObject::get(self, property, receiver)
    }

    fn set(
        &mut self,
        property: PropertyKey,
        value: JsValue,
        receiver: JsValueOrSelf,
    ) -> Result<bool, JErrorType> {
        let is_same = if let JsValueOrSelf::ValueRef(v) = receiver {
            same_value_with_js_object(v, self.as_js_object())
        } else {
            true
        };
        if is_same {
            if let Some(map) = &mut self.get_argument_base_object_mut().parameter_map {
                if has_own_property(map, &property)? {
                    set(map, property, value);
                }
            }
        }
        JsObject::set(self, property.clone(), value.clone(), receiver)
    }

    fn delete(&mut self, property: &PropertyKey) -> Result<bool, JErrorType> {
        let result = JsObject::delete(self, property)?;
        if let Some(map) = &self.get_argument_base_object_mut().parameter_map {
            if result && has_own_property(map, &property)? {
                (**map).borrow_mut().as_js_object_mut().delete(property);
            }
        }
        Ok(result)
    }
}

pub struct CoreArgumentsObject {
    base: ObjectBase,
    arguments_base: ArgumentsBaseObject,
}
impl CoreArgumentsObject {
    pub fn new(code_realm: &CodeRealm, arguments_list: Vec<JsValue>) -> Self {
        let mut o = CoreArgumentsObject {
            base: ObjectBase::new(),
            arguments_base: ArgumentsBaseObject::new(),
        };
        create_unmapped_arguments_object(&mut o, code_realm, arguments_list);
        o
    }
}
impl JsArgumentsObject for CoreArgumentsObject {
    fn get_argument_base_object(&self) -> &ArgumentsBaseObject {
        &self.arguments_base
    }

    fn get_argument_base_object_mut(&mut self) -> &mut ArgumentsBaseObject {
        &mut self.arguments_base
    }
}
impl JsObject for CoreArgumentsObject {
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

pub fn create_unmapped_arguments_object(
    obj: &mut dyn JsArgumentsObject,
    code_realm: &CodeRealm,
    arguments_list: Vec<JsValue>,
) {
    obj.get_object_base_mut().prototype = Some(
        code_realm
            .get_intrinsics_value(&WellKnownIntrinsics::ObjectPrototype)
            .clone(),
    );
    let len = arguments_list.len() as i64;
    define_property_or_throw(
        obj,
        ARRAY_LENGTH_PROP.clone(),
        PropertyDescriptorSetter::new_from_property_descriptor(PropertyDescriptor::Data(
            PropertyDescriptorData {
                value: JsValue::Number(JsNumberType::Integer(len)),
                writable: true,
                enumerable: false,
                configurable: true,
            },
        )),
    )?;
    for idx in 0..len {
        create_data_property(
            obj,
            PropertyKey::Str(to_string_int(idx)),
            arguments_list[idx],
        );
    }
    define_property_or_throw(
        obj,
        PropertyKey::Sym(SYMBOL_ITERATOR.clone()),
        PropertyDescriptorSetter::new_from_property_descriptor(PropertyDescriptor::Data(
            PropertyDescriptorData {
                value: JsValue::Object(
                    code_realm
                        .get_intrinsics_value(&WellKnownIntrinsics::ArrayProtoValues)
                        .clone(),
                ),
                writable: true,
                enumerable: false,
                configurable: true,
            },
        )),
    )?;
    define_property_or_throw(
        obj,
        PropertyKey::Str("caller".to_string()),
        PropertyDescriptorSetter::new_from_property_descriptor(PropertyDescriptor::Accessor(
            PropertyDescriptorAccessor {
                set: Some(
                    code_realm
                        .get_intrinsics_value(&WellKnownIntrinsics::ThrowTypeError)
                        .clone(),
                ),
                get: Some(
                    code_realm
                        .get_intrinsics_value(&WellKnownIntrinsics::ThrowTypeError)
                        .clone(),
                ),
                enumerable: false,
                configurable: false,
            },
        )),
    )?;
    define_property_or_throw(
        obj,
        PropertyKey::Str("callee".to_string()),
        PropertyDescriptorSetter::new_from_property_descriptor(PropertyDescriptor::Accessor(
            PropertyDescriptorAccessor {
                set: Some(
                    code_realm
                        .get_intrinsics_value(&WellKnownIntrinsics::ThrowTypeError)
                        .clone(),
                ),
                get: Some(
                    code_realm
                        .get_intrinsics_value(&WellKnownIntrinsics::ThrowTypeError)
                        .clone(),
                ),
                enumerable: false,
                configurable: false,
            },
        )),
    )?;
}

pub fn create_mapped_arguments_object(
    obj: &mut dyn JsArgumentsObject,
    code_realm: &CodeRealm,
    func:&JsObjectType,
    formals:,
    arguments_list: Vec<JsValue>,
    env:&EnvironmentRecordType
) {
}
