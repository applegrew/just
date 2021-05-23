use crate::runner::ds::array_object::JsArrayObject;
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::function_object::JsFunctionObject;
use crate::runner::ds::iterator_object::JsIteratorObject;
use crate::runner::ds::object_property::{
    PropertyDescriptor, PropertyDescriptorSetter, PropertyKey,
};
use crate::runner::ds::operations::test_and_comparison::{same_js_object, same_object, same_value};
use crate::runner::ds::value::JsValue;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

pub enum ObjectType {
    Ordinary(Box<dyn JsObject>),
    Function(Box<dyn JsFunctionObject>),
    Array(Box<dyn JsArrayObject>),
}
impl PartialEq for ObjectType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ObjectType::Ordinary(o) => {
                if let ObjectType::Ordinary(o1) = other {
                    same_js_object(o.deref(), o1.deref())
                } else {
                    false
                }
            }
            ObjectType::Function(o) => {
                if let ObjectType::Function(o1) = other {
                    same_js_object(o.deref(), o1.deref())
                } else {
                    false
                }
            }
            ObjectType::Array(o) => {
                if let ObjectType::Array(o1) = other {
                    same_js_object(o.deref(), o1.deref())
                } else {
                    false
                }
            }
        }
    }
}
impl ObjectType {
    pub fn is_callable(&self) -> bool {
        match self {
            ObjectType::Function(_) => true,
            _ => false,
        }
    }

    pub fn as_js_object(&self) -> &dyn JsObject {
        match self {
            ObjectType::Ordinary(o) => o.as_super_trait(),
            ObjectType::Function(o) => o.as_super_trait(),
            ObjectType::Array(o) => o.as_super_trait(),
        }
    }

    pub fn as_js_object_mut(&mut self) -> &mut dyn JsObject {
        match self {
            ObjectType::Ordinary(o) => o.as_super_trait_mut(),
            ObjectType::Function(o) => o.as_super_trait_mut(),
            ObjectType::Array(o) => o.as_super_trait_mut(),
        }
    }
}

pub struct ObjectBase {
    properties: HashMap<PropertyKey, PropertyDescriptor>,
    is_extensible: bool,
    prototype: Option<Rc<RefCell<ObjectType>>>,
}
impl ObjectBase {
    pub fn new() -> Self {
        ObjectBase {
            properties: HashMap::new(),
            is_extensible: true,
            prototype: None,
        }
    }
}

pub trait JsObject {
    fn get_object_base_mut(&mut self) -> &mut ObjectBase;

    fn get_object_base(&self) -> &ObjectBase;

    fn as_super_trait(&self) -> &dyn JsObject;

    fn as_super_trait_mut(&mut self) -> &mut dyn JsObject;

    fn get_prototype_of(&self) -> Option<Rc<RefCell<ObjectType>>> {
        match &self.get_object_base().prototype {
            None => None,
            Some(j) => Some(j.clone()),
        }
    }

    fn set_prototype_of(&mut self, prototype: Option<Rc<RefCell<ObjectType>>>) -> bool {
        let current_value = &self.get_object_base().prototype;
        let new_value: Option<Rc<RefCell<ObjectType>>> = match prototype {
            None => {
                if current_value.is_none() {
                    return true;
                } else {
                    None
                }
            }
            Some(p) => {
                if let Some(current_p) = current_value {
                    if same_object(
                        p.deref().borrow().deref(),
                        current_p.deref().borrow().deref(),
                    ) {
                        return true;
                    } else {
                        Some(p.clone())
                    }
                } else {
                    None
                }
            }
        };
        if self.is_extensible() {
            if let Some(new_value) = new_value {
                let mut p = Some(new_value.clone());
                loop {
                    if let Some(some_p) = p {
                        if same_js_object(
                            self.as_super_trait(),
                            (*(*some_p).borrow()).as_js_object(),
                        ) {
                            // To prevent circular chain
                            return false;
                        } else {
                            let t1 = (*some_p).borrow();
                            let t2 = (*t1).as_js_object().get_prototype_of();
                            p = t2;
                        }
                    } else {
                        break;
                    }
                }
                self.get_object_base_mut().prototype = Some(new_value);
            } else {
                self.get_object_base_mut().prototype = None;
            }
            true
        } else {
            false
        }
    }

    fn is_extensible(&self) -> bool {
        self.get_object_base().is_extensible
    }

    fn prevent_extensions(&mut self) -> bool {
        self.get_object_base_mut().is_extensible = false;
        true
    }

    fn get_own_property(&self, property: &PropertyKey) -> Option<&PropertyDescriptor> {
        self.get_object_base().properties.get(property)
    }

    fn define_own_property(
        &mut self,
        property: PropertyKey,
        descriptor_setter: PropertyDescriptorSetter,
    ) -> bool {
        ordinary_define_own_property(self, property, descriptor_setter)
    }

    fn has_property(&self, property: &PropertyKey) -> bool {
        if self.get_object_base().properties.contains_key(property) {
            true
        } else {
            match &self.get_object_base().prototype {
                None => false,
                Some(o) => (**o).borrow().as_js_object().has_property(property),
            }
        }
    }

    fn get<'a>(
        &'a self,
        property: &'a PropertyKey,
        receiver: &'a JsValue,
    ) -> Result<JsValue, JErrorType> {
        match self.get_own_property(property) {
            None => match self.get_prototype_of() {
                None => Ok(JsValue::Undefined),
                Some(p) => (*(*p).borrow()).as_js_object().get(property, receiver),
            },
            Some(pd) => match pd {
                PropertyDescriptor::Data { value, .. } => Ok(value.clone()),
                PropertyDescriptor::Accessor { get, .. } => match get {
                    None => Ok(JsValue::Undefined),
                    Some(getter) => getter.call(receiver, Vec::new()),
                },
            },
        }
    }

    fn set<'a>(
        &'a mut self,
        property: PropertyKey,
        value: JsValue,
        receiver: &'a JsValue,
    ) -> Result<bool, JErrorType> {
        match self.get_own_property(&property) {
            None => match self.get_prototype_of() {
                None => {
                    self.get_object_base_mut().properties.insert(
                        property,
                        PropertyDescriptor::Data {
                            value,
                            writable: true,
                            enumerable: true,
                            configurable: true,
                        },
                    );
                    Ok(true)
                }
                Some(p) => (*p)
                    .borrow_mut()
                    .as_js_object_mut()
                    .set(property, value, receiver),
            },
            Some(pd) => match pd {
                PropertyDescriptor::Data {
                    value, writable, ..
                } => {
                    if *writable {
                        match receiver {
                            JsValue::Object(o) => {
                                let mut ot_obj = (**o).borrow_mut();
                                let obj = (*ot_obj).as_js_object_mut();
                                match obj.get_own_property(&property) {
                                    None => {
                                        let desc_setter =
                                            PropertyDescriptorSetter::new_from_property_descriptor(
                                                PropertyDescriptor::Data {
                                                    value: value.clone(),
                                                    writable: true,
                                                    enumerable: true,
                                                    configurable: true,
                                                },
                                            );
                                        obj.define_own_property(property, desc_setter);
                                        Ok(true)
                                    }
                                    Some(pd) => match pd {
                                        PropertyDescriptor::Data { writable, .. } => {
                                            if *writable {
                                                Ok(obj.define_own_property(
                                                    property,
                                                    PropertyDescriptorSetter {
                                                        honour_value: true,
                                                        honour_writable: false,
                                                        honour_set: false,
                                                        honour_get: false,
                                                        honour_enumerable: false,
                                                        honour_configurable: false,
                                                        descriptor: PropertyDescriptor::Data {
                                                            value: value.clone(),
                                                            writable: false,
                                                            enumerable: false,
                                                            configurable: false,
                                                        },
                                                    },
                                                ))
                                            } else {
                                                Ok(false)
                                            }
                                        }
                                        PropertyDescriptor::Accessor { .. } => Ok(false),
                                    },
                                }
                            }
                            _ => Ok(false),
                        }
                    } else {
                        Ok(false)
                    }
                }
                PropertyDescriptor::Accessor { set, .. } => match set {
                    None => Ok(false),
                    Some(setter) => {
                        setter.call(receiver, vec![value])?;
                        Ok(true)
                    }
                },
            },
        }
    }

    fn delete(&mut self, property: &PropertyKey) -> bool {
        match self.get_own_property(property) {
            None => true,
            Some(pd) => {
                if pd.is_configurable() {
                    self.get_object_base_mut().properties.remove(property);
                    true
                } else {
                    false
                }
            }
        }
    }

    fn enumerate(&self) -> JsIteratorObject {
        todo!()
    }

    fn own_property_keys(&self) -> Vec<PropertyKey> {
        let mut int_keys = vec![];
        let mut str_keys = vec![];
        let mut sym_keys = vec![];
        for (key, _) in &self.get_object_base().properties {
            match key {
                PropertyKey::Str(d) => {
                    str_keys.push(d.to_string());
                }
                PropertyKey::Int(d) => {
                    int_keys.push(d.clone());
                }
                PropertyKey::Sym(d) => {
                    sym_keys.push(d.clone());
                }
            }
        }
        int_keys.sort();

        let mut result = vec![];
        result.append(
            &mut int_keys
                .into_iter()
                .map(|v| PropertyKey::Int(v))
                .collect::<Vec<PropertyKey>>(),
        );
        result.append(
            &mut str_keys
                .into_iter()
                .map(|v| PropertyKey::Str(v))
                .collect::<Vec<PropertyKey>>(),
        );
        result.append(
            &mut sym_keys
                .into_iter()
                .map(|v| PropertyKey::Sym(v))
                .collect::<Vec<PropertyKey>>(),
        );
        result
    }

    fn to_string(&self) -> String {
        format!("object")
    }
}

pub fn ordinary_define_own_property<J: JsObject + ?Sized>(
    o: &mut J,
    property: PropertyKey,
    mut descriptor_setter: PropertyDescriptorSetter,
) -> bool {
    let current_descriptor = o.get_own_property(&property);
    if let Some(current_descriptor) = current_descriptor {
        if descriptor_setter.is_empty() {
            true
        } else if descriptor_setter.are_all_fields_set()
            && current_descriptor == &descriptor_setter.descriptor
        {
            true
        } else {
            let descriptor = &descriptor_setter.descriptor;
            if !current_descriptor.is_configurable() {
                if descriptor.is_configurable() {
                    return false;
                } else {
                    if (current_descriptor.is_enumerable() && !descriptor.is_enumerable())
                        || (!current_descriptor.is_enumerable() && descriptor.is_enumerable())
                    {
                        return false;
                    }
                }
            }
            if !descriptor_setter.is_generic_descriptor() {
                if (current_descriptor.is_data_descriptor() && !descriptor.is_data_descriptor())
                    || (!current_descriptor.is_data_descriptor() && descriptor.is_data_descriptor())
                {
                    if !current_descriptor.is_configurable() {
                        return false;
                    }
                    descriptor_setter.descriptor = match descriptor_setter.descriptor {
                        PropertyDescriptor::Data {
                            value, writable, ..
                        } => PropertyDescriptor::Data {
                            value,
                            writable,
                            enumerable: current_descriptor.is_enumerable(),
                            configurable: current_descriptor.is_configurable(),
                        },
                        PropertyDescriptor::Accessor {
                            set: setter,
                            get: getter,
                            ..
                        } => PropertyDescriptor::Accessor {
                            set: setter,
                            get: getter,
                            enumerable: current_descriptor.is_enumerable(),
                            configurable: current_descriptor.is_configurable(),
                        },
                    };
                } else if current_descriptor.is_data_descriptor() && descriptor.is_data_descriptor()
                {
                    if !current_descriptor.is_configurable() {
                        if let PropertyDescriptor::Data {
                            writable: current_writable,
                            value: current_value,
                            ..
                        } = current_descriptor
                        {
                            if let PropertyDescriptor::Data {
                                writable: desc_writable,
                                value: desc_value,
                                ..
                            } = &descriptor
                            {
                                if !*current_writable && *desc_writable {
                                    return false;
                                }
                                if !*current_writable {
                                    if descriptor_setter.honour_value
                                        && !same_value(current_value, desc_value)
                                    {
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                } else if current_descriptor.is_accessor_descriptor()
                    && descriptor.is_accessor_descriptor()
                {
                    if !current_descriptor.is_configurable() {
                        if let PropertyDescriptor::Accessor {
                            set: current_set,
                            get: current_get,
                            ..
                        } = current_descriptor
                        {
                            if let PropertyDescriptor::Accessor {
                                set: desc_set,
                                get: desc_get,
                                ..
                            } = &descriptor
                            {
                                if !(current_set.is_none() && desc_set.is_none())
                                    && descriptor_setter.honour_set
                                    && ((!current_set.is_none() && desc_set.is_none())
                                        || (current_set.is_none() && !desc_set.is_none())
                                        || !same_js_object::<dyn JsFunctionObject>(
                                            current_set.as_ref().unwrap().borrow(),
                                            desc_set.as_ref().unwrap().borrow(),
                                        ))
                                {
                                    return false;
                                }

                                if !(current_get.is_none() && desc_get.is_none())
                                    && descriptor_setter.honour_get
                                    && ((!current_get.is_none() && desc_get.is_none())
                                        || (current_get.is_none() && !desc_get.is_none())
                                        || !same_js_object::<dyn JsFunctionObject>(
                                            current_get.as_ref().unwrap().borrow(),
                                            desc_get.as_ref().unwrap().borrow(),
                                        ))
                                {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
            o.get_object_base_mut().properties.insert(
                property,
                PropertyDescriptor::new_from_property_descriptor_setter(descriptor_setter),
            );
            true
        }
    } else {
        if o.is_extensible() {
            o.get_object_base_mut().properties.insert(
                property,
                PropertyDescriptor::new_from_property_descriptor_setter(descriptor_setter),
            );
            true
        } else {
            false
        }
    }
}
