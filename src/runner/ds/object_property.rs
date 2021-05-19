use crate::runner::ds::function_object::JsFunctionObject;
use crate::runner::ds::operations::test_and_comparison::same_value;
use crate::runner::ds::symbol::SymbolData;
use crate::runner::ds::value::JsValue;

pub enum PropertyKey {
    Str(String),
    Int(u32),
    Sym(SymbolData),
}

pub struct PropertyDescriptorSetter {
    pub honour_value: bool,
    pub honour_writable: bool,
    pub honour_set: bool,
    pub honour_get: bool,
    pub honour_enumerable: bool,
    pub honour_configurable: bool,
    pub descriptor: PropertyDescriptor,
}
impl PropertyDescriptorSetter {
    pub(crate) fn new_from_property_descriptor(desc: PropertyDescriptor) -> Self {
        match desc {
            PropertyDescriptor::Data { .. } => PropertyDescriptorSetter {
                honour_value: true,
                honour_writable: true,
                honour_configurable: true,
                honour_enumerable: true,
                descriptor: desc,
                honour_set: false,
                honour_get: false,
            },
            PropertyDescriptor::Accessor { .. } => PropertyDescriptorSetter {
                honour_set: true,
                honour_get: true,
                honour_configurable: true,
                honour_enumerable: true,
                descriptor: desc,
                honour_value: false,
                honour_writable: false,
            },
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        !self.honour_configurable
            && !self.honour_enumerable
            && !self.honour_get
            && !self.honour_set
            && !self.honour_value
            && !self.honour_writable
    }

    pub(crate) fn are_all_fields_set(&self) -> bool {
        if self.descriptor.is_data_descriptor() {
            self.honour_configurable
                && self.honour_enumerable
                && self.honour_value
                && self.honour_writable
        } else {
            self.honour_configurable && self.honour_enumerable && self.honour_get && self.honour_set
        }
    }

    pub(crate) fn is_generic_descriptor(&self) -> bool {
        !self.honour_get && !self.honour_set && !self.honour_value && !self.honour_writable
    }
}

pub enum PropertyDescriptor {
    Data {
        value: JsValue,
        writable: bool,
        enumerable: bool,
        configurable: bool,
    },
    Accessor {
        set: Option<Box<dyn JsFunctionObject>>,
        get: Option<Box<dyn JsFunctionObject>>,
        enumerable: bool,
        configurable: bool,
    },
}
impl PropertyDescriptor {
    pub(crate) fn new_from_property_descriptor_setter(
        desc_setter: PropertyDescriptorSetter,
    ) -> Self {
        if desc_setter.is_generic_descriptor() {
            if desc_setter.descriptor.is_data_descriptor() {
                PropertyDescriptor::Data {
                    value: JsValue::Undefined,
                    writable: false,
                    enumerable: if desc_setter.honour_enumerable {
                        desc_setter.descriptor.is_enumerable()
                    } else {
                        false
                    },
                    configurable: if desc_setter.honour_configurable {
                        desc_setter.descriptor.is_configurable()
                    } else {
                        false
                    },
                }
            } else {
                PropertyDescriptor::Accessor {
                    set: None,
                    get: None,
                    enumerable: if desc_setter.honour_enumerable {
                        desc_setter.descriptor.is_enumerable()
                    } else {
                        false
                    },
                    configurable: if desc_setter.honour_configurable {
                        desc_setter.descriptor.is_configurable()
                    } else {
                        false
                    },
                }
            }
        } else if desc_setter.descriptor.is_data_descriptor() {
            if let PropertyDescriptor::Data {
                writable, value, ..
            } = desc_setter.descriptor
            {
                PropertyDescriptor::Data {
                    value: if desc_setter.honour_value {
                        value
                    } else {
                        JsValue::Undefined
                    },
                    writable: if desc_setter.honour_writable {
                        writable
                    } else {
                        false
                    },
                    enumerable: if desc_setter.honour_enumerable {
                        desc_setter.descriptor.is_enumerable()
                    } else {
                        false
                    },
                    configurable: if desc_setter.honour_configurable {
                        desc_setter.descriptor.is_configurable()
                    } else {
                        false
                    },
                }
            } else {
                unreachable!()
            }
        } else if desc_setter.descriptor.is_accessor_descriptor() {
            if let PropertyDescriptor::Accessor { set, get, .. } = desc_setter.descriptor {
                PropertyDescriptor::Accessor {
                    set: if desc_setter.honour_set { set } else { None },
                    get: if desc_setter.honour_get { get } else { None },
                    enumerable: if desc_setter.honour_enumerable {
                        desc_setter.descriptor.is_enumerable()
                    } else {
                        false
                    },
                    configurable: if desc_setter.honour_configurable {
                        desc_setter.descriptor.is_configurable()
                    } else {
                        false
                    },
                }
            } else {
                unreachable!()
            }
        }
    }

    pub(crate) fn is_enumerable(&self) -> bool {
        match self {
            PropertyDescriptor::Data { enumerable, .. } => *enumerable,
            PropertyDescriptor::Accessor { enumerable, .. } => *enumerable,
        }
    }

    pub(crate) fn is_configurable(&self) -> bool {
        match self {
            PropertyDescriptor::Data { configurable, .. } => *configurable,
            PropertyDescriptor::Accessor { configurable, .. } => *configurable,
        }
    }

    pub(crate) fn is_data_descriptor(&self) -> bool {
        match self {
            PropertyDescriptor::Data { .. } => true,
            PropertyDescriptor::Accessor { .. } => false,
        }
    }

    pub(crate) fn is_accessor_descriptor(&self) -> bool {
        match self {
            PropertyDescriptor::Data { .. } => false,
            PropertyDescriptor::Accessor { .. } => true,
        }
    }
}
impl PartialEq for PropertyDescriptor {
    fn eq(&self, other: &Self) -> bool {
        match self {
            PropertyDescriptor::Data {
                value,
                writable,
                enumerable,
                configurable,
            } => {
                if let PropertyDescriptor::Data {
                    value: other_value,
                    writable: other_writable,
                    enumerable: other_enumerable,
                    configurable: other_configurable,
                } = other
                {
                    same_value(value, other_value)
                        && writable == other_writable
                        && enumerable == other_enumerable
                        && configurable == other_configurable
                } else {
                    false
                }
            }
            PropertyDescriptor::Accessor {
                set: setter,
                get: getter,
                enumerable,
                configurable,
            } => {
                if let PropertyDescriptor::Accessor {
                    set: other_setter,
                    get: other_getter,
                    enumerable: other_enumerable,
                    configurable: other_configurable,
                } = other
                {
                    setter == other_setter
                        && getter == other_getter
                        && enumerable == other_enumerable
                        && configurable == other_configurable
                } else {
                    false
                }
            }
        }
    }
}
