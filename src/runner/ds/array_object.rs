use crate::runner::ds::object::{ordinary_define_own_property, JsObject};
use crate::runner::ds::object_property::{
    PropertyDescriptor, PropertyDescriptorSetter, PropertyKey,
};

const ARRAY_LENGTH_PROP: PropertyKey = PropertyKey::Str("length".to_string());

pub trait JsArrayObject: JsObject {
    fn define_own_property(
        &mut self,
        property: PropertyKey,
        mut descriptor_setter: PropertyDescriptorSetter,
    ) -> bool {
    }
}

pub fn array_set_length(
    array: &mut dyn JsArrayObject,
    property: PropertyKey,
    mut descriptor_setter: PropertyDescriptorSetter,
) -> bool {
    if !descriptor_setter.honour_value {
        ordinary_define_own_property(array, ARRAY_LENGTH_PROP, descriptor_setter)
    } else {
        if let PropertyDescriptor::Data { value, .. } = &descriptor_setter.descriptor {
            let new_length = to_unit_32(value);
        }
    }
}
