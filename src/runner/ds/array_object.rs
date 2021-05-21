use crate::runner::ds::object::{ordinary_define_own_property, JsObject};
use crate::runner::ds::object_property::{
    PropertyDescriptor, PropertyDescriptorSetter, PropertyKey,
};
use crate::runner::ds::operations::type_conversion::to_unit_32;
use crate::runner::ds::value::JErrorType;

const ARRAY_LENGTH_PROP: PropertyKey = PropertyKey::Str("length".to_string());

pub trait JsArrayObject<'code>: JsObject<'code> {
    fn define_own_property(
        &mut self,
        property: PropertyKey,
        mut descriptor_setter: PropertyDescriptorSetter,
    ) -> bool {
        todo!()
    }
}

pub fn array_set_length(
    array: &mut dyn JsArrayObject,
    mut descriptor_setter: PropertyDescriptorSetter,
) -> Result<bool, JErrorType> {
    if !descriptor_setter.honour_value {
        Ok(ordinary_define_own_property(
            array,
            ARRAY_LENGTH_PROP,
            descriptor_setter,
        ))
    } else {
        if let PropertyDescriptor::Data { value, .. } = &descriptor_setter.descriptor {
            let new_length = to_unit_32(value)?;
        }
        todo!()
    }
}
