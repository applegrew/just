use crate::runner::ds::array_object::ARRAY_LENGTH_PROP;
use crate::runner::ds::env_record::EnvironmentRecordType;
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::execution_context::ExecutionContextStack;
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
        if desc.is_some() {
            // Note: The parameter_map lookup cannot safely return a reference because
            // the RefCell borrow would be temporary. Fall back to base object behavior.
            // TODO: Refactor to return owned PropertyDescriptor instead of reference
        }
        Ok(desc)
    }

    fn define_own_property(
        &mut self,
        ctx_stack: &mut ExecutionContextStack,
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
        let descriptor_setter_value = value.clone();
        let descriptor_setter_property = property.clone();
        if ordinary_define_own_property(self.as_js_object_mut(), property, descriptor_setter)? {
            if let Some(parameter_map_rc) = &self.get_argument_base_object().parameter_map {
                if has_own_property(parameter_map_rc, &descriptor_setter_property)? {
                    // Keep the borrow guard alive for the duration of this block
                    let mut parameter_map_guard = (**parameter_map_rc).borrow_mut();
                    let parameter_map = parameter_map_guard.as_js_object_mut();
                    if is_accessor_desc {
                        let _ = parameter_map.delete(&descriptor_setter_property);
                    } else {
                        let descriptor_setter_property2 = descriptor_setter_property.clone();
                        if is_value_present {
                            let _ = set_from_js_object(
                                ctx_stack,
                                parameter_map,
                                descriptor_setter_property,
                                descriptor_setter_value,
                            );
                        }
                        if is_writable_and_is_false {
                            let _ = parameter_map.delete(&descriptor_setter_property2);
                        }
                    }
                }
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn get(
        &self,
        ctx_stack: &mut ExecutionContextStack,
        property: &PropertyKey,
        receiver: JsValueOrSelf,
    ) -> Result<JsValue, JErrorType> {
        if let Some(parameter_map) = &self.get_argument_base_object().parameter_map {
            if has_own_property(parameter_map, &property)? {
                return get(ctx_stack, parameter_map, property);
            }
        }
        JsObject::get(self, ctx_stack, property, receiver)
    }

    fn set(
        &mut self,
        ctx_stack: &mut ExecutionContextStack,
        property: PropertyKey,
        value: JsValue,
        receiver: JsValueOrSelf,
    ) -> Result<bool, JErrorType> {
        let is_same = if let JsValueOrSelf::ValueRef(v) = receiver {
            same_value_with_js_object(v, self.as_js_object())
        } else {
            true
        };
        let property2 = property.clone();
        let value2 = value.clone();
        if is_same {
            if let Some(map) = &mut self.get_argument_base_object_mut().parameter_map {
                if has_own_property(map, &property)? {
                    let _ = set(ctx_stack, map, property, value);
                }
            }
        }
        JsObject::set(self, ctx_stack, property2, value2, receiver)
    }

    fn delete(&mut self, property: &PropertyKey) -> Result<bool, JErrorType> {
        let result = JsObject::delete(self, property)?;
        if let Some(map) = &self.get_argument_base_object_mut().parameter_map {
            if result && has_own_property(map, &property)? {
                let _ = (**map).borrow_mut().as_js_object_mut().delete(property);
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
        let _ = create_unmapped_arguments_object(&mut o, code_realm, arguments_list);
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
) -> Result<(), JErrorType> {
    obj.get_object_base_mut().prototype = Some(
        code_realm
            .get_intrinsics_value(&WellKnownIntrinsics::ObjectPrototype)
            .clone(),
    );
    let len = arguments_list.len() as i64;
    define_property_or_throw(
        obj.as_js_object_mut(),
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
    let mut arg_iter = arguments_list.into_iter();
    for idx in 0..len {
        let _ = create_data_property(
            obj.as_js_object_mut(),
            PropertyKey::Str(to_string_int(idx)),
            arg_iter.next().unwrap(),
        );
    }
    define_property_or_throw(
        obj.as_js_object_mut(),
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
        obj.as_js_object_mut(),
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
        obj.as_js_object_mut(),
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
    Ok(())
}

// pub fn create_mapped_arguments_object(
//     obj: &mut dyn JsArgumentsObject,
//     code_realm: &CodeRealm,
//     func:&JsObjectType,
//     formals:,
//     arguments_list: Vec<JsValue>,
//     env:&EnvironmentRecordType
// ) {
// }
