use crate::runner::ds::error::JErrorType;
use crate::runner::ds::object::{
    ordinary_define_own_property, JsObject, JsObjectType, ObjectBase, ObjectType,
};
use crate::runner::ds::object_property::{
    PropertyDescriptor, PropertyDescriptorData, PropertyDescriptorSetter, PropertyKey,
};
use crate::runner::ds::operations::type_conversion::{
    canonical_numeric_index_string, to_number, to_string, to_string_int, to_unit_32,
};
use crate::runner::ds::value::{JsNumberType, JsValue};
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::rc::Rc;

lazy_static! {
    pub static ref ARRAY_LENGTH_PROP: PropertyKey = PropertyKey::Str("length".to_string());
}

pub trait JsArrayObject: JsObject {
    fn as_js_array_object(&self) -> &dyn JsArrayObject;

    fn as_js_array_object_mut(&mut self) -> &mut dyn JsArrayObject;

    fn get_own_length_property(&self) -> Result<&PropertyDescriptorData, JErrorType> {
        if let Some(len_desc) = self.get_own_property(&*ARRAY_LENGTH_PROP)? {
            if let PropertyDescriptor::Data(d) = len_desc {
                Ok(d)
            } else {
                panic!("Array.length should have been data descriptor");
            }
        } else {
            panic!("Array.length should have been set");
        }
    }

    fn define_own_property(
        &mut self,
        mut property: PropertyKey,
        mut descriptor_setter: PropertyDescriptorSetter,
    ) -> Result<bool, JErrorType> {
        if let PropertyKey::Str(s) = &property {
            if s == "length" {
                return array_set_length(
                    self.as_js_array_object_mut(),
                    property,
                    descriptor_setter,
                );
            } else if let Some(idx) = canonical_numeric_index_string(s) {
                let len_desc = self.get_own_length_property()?;
                let old_len = to_unit_32(&len_desc.value)?;
                if idx >= old_len && !len_desc.writable {
                    return Ok(false);
                }
                if !ordinary_define_own_property(
                    self.as_js_object_mut(),
                    property,
                    descriptor_setter,
                )? {
                    return Ok(false);
                }
                return if idx >= old_len {
                    ordinary_define_own_property(
                        self.as_js_object_mut(),
                        ARRAY_LENGTH_PROP.clone(),
                        PropertyDescriptorSetter {
                            honour_value: true,
                            honour_writable: false,
                            honour_set: false,
                            honour_get: false,
                            honour_enumerable: false,
                            honour_configurable: false,
                            descriptor: PropertyDescriptor::Data(PropertyDescriptorData {
                                value: JsValue::Number(JsNumberType::Integer((idx + 1) as i64)),
                                writable: false,
                                enumerable: false,
                                configurable: false,
                            }),
                        },
                    )
                } else {
                    Ok(true)
                };
            }
        }
        ordinary_define_own_property(self.as_js_object_mut(), property, descriptor_setter)
    }
}

pub struct CoreArrayObject {
    base: ObjectBase,
}
impl CoreArrayObject {
    fn new(length: u32, proto: Option<JsObjectType>) -> Self {
        let mut obj = CoreArrayObject {
            base: ObjectBase::new(),
        };
        finish_array_create(&mut obj, length, proto);
        obj
    }
}
impl JsArrayObject for CoreArrayObject {
    fn as_js_array_object(&self) -> &dyn JsArrayObject {
        self
    }

    fn as_js_array_object_mut(&mut self) -> &mut dyn JsArrayObject {
        self
    }
}
impl JsObject for CoreArrayObject {
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

pub fn array_set_length(
    array: &mut dyn JsArrayObject,
    property: PropertyKey,
    mut descriptor_setter: PropertyDescriptorSetter,
) -> Result<bool, JErrorType> {
    if !descriptor_setter.honour_value {
        ordinary_define_own_property(array, ARRAY_LENGTH_PROP.clone(), descriptor_setter)
    } else {
        if let PropertyDescriptor::Data(new_descriptor) = &mut descriptor_setter.descriptor {
            let new_length = to_unit_32(&new_descriptor.value)?;
            let new_length_in_js_number = to_number(&new_descriptor.value)?;
            if match new_length_in_js_number {
                JsNumberType::Integer(i) => i != new_length as i64,
                JsNumberType::Float(f) => f != new_length as f64,
                JsNumberType::NaN => true,
                JsNumberType::PositiveInfinity => true,
                JsNumberType::NegativeInfinity => true,
            } {
                return Err(JErrorType::RangeError(new_length_in_js_number.to_string()));
            }

            let old_descriptor = array.get_own_length_property()?;

            new_descriptor.value = JsValue::Number(JsNumberType::Integer(new_length as i64));
            descriptor_setter.honour_value = true;

            let new_writable = !descriptor_setter.honour_writable || new_descriptor.writable;

            let old_length = to_unit_32(&old_descriptor.value)?;
            if new_length >= old_length {
                ordinary_define_own_property(array, ARRAY_LENGTH_PROP.clone(), descriptor_setter)
            } else {
                if old_descriptor.writable {
                    if !new_writable {
                        descriptor_setter.honour_writable = true;
                        new_descriptor.writable = true;
                    }

                    if ordinary_define_own_property(
                        array,
                        ARRAY_LENGTH_PROP.clone(),
                        descriptor_setter,
                    )? {
                        if new_length < old_length {
                            let mut idx = old_length;
                            loop {
                                idx -= 1;
                                if !array.delete(&PropertyKey::Str(to_string_int(idx as i64)))? {
                                    ordinary_define_own_property(
                                        array,
                                        ARRAY_LENGTH_PROP.clone(),
                                        PropertyDescriptorSetter {
                                            honour_value: true,
                                            honour_writable: false,
                                            honour_set: false,
                                            honour_get: false,
                                            honour_enumerable: false,
                                            honour_configurable: false,
                                            descriptor: PropertyDescriptor::Data(
                                                PropertyDescriptorData {
                                                    value: JsValue::Number(JsNumberType::Integer(
                                                        (idx + 1) as i64,
                                                    )),
                                                    writable: false,
                                                    enumerable: false,
                                                    configurable: false,
                                                },
                                            ),
                                        },
                                    );
                                    return Ok(false);
                                } else {
                                    if idx <= new_length {
                                        break;
                                    }
                                }
                            }
                        }
                        if !new_writable {
                            ordinary_define_own_property(
                                array,
                                ARRAY_LENGTH_PROP.clone(),
                                PropertyDescriptorSetter {
                                    honour_value: false,
                                    honour_writable: true,
                                    honour_set: false,
                                    honour_get: false,
                                    honour_enumerable: false,
                                    honour_configurable: false,
                                    descriptor: PropertyDescriptor::Data(PropertyDescriptorData {
                                        value: JsValue::Undefined,
                                        writable: false,
                                        enumerable: false,
                                        configurable: false,
                                    }),
                                },
                            )
                        } else {
                            Ok(true)
                        }
                    } else {
                        Ok(false)
                    }
                } else {
                    Ok(false)
                }
            }
        } else {
            panic!("Provided new descriptor for Array.length is not a data descriptor");
        }
    }
}

pub fn finish_array_create(
    array_obj: &mut dyn JsArrayObject,
    length: u32,
    proto: Option<JsObjectType>,
) {
    let final_proto = if let Some(proto) = proto {
        proto
    } else {
        IntrinsicArrayPrototype
    };
    array_obj.get_object_base_mut().prototype = Some(final_proto);
    array_obj.get_object_base_mut().is_extensible = true;
    ordinary_define_own_property(
        array_obj,
        ARRAY_LENGTH_PROP.clone(),
        PropertyDescriptorSetter::new_from_property_descriptor(PropertyDescriptor::Data(
            PropertyDescriptorData {
                value: JsValue::Number(JsNumberType::Integer(length as i64)),
                writable: true,
                enumerable: false,
                configurable: false,
            },
        )),
    );
}

pub fn array_create(length: u32, proto: Option<JsObjectType>) -> impl JsArrayObject {
    CoreArrayObject::new(length, proto)
}
