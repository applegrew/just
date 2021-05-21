use crate::runner::ds::object::{JsObject, ObjectType};
use crate::runner::ds::object_property::PropertyKey;
use crate::runner::ds::operations::type_conversion::to_object;
use crate::runner::ds::value::{JErrorType, JsValue};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub fn get<'code>(o: &Rc<RefCell<ObjectType>>, p: &PropertyKey) -> JsValue<'code> {
    let o1 = o.deref().borrow().deref();
    o1.get(p, &JsValue::Object(o.clone()))
}

pub fn get_v<'code>(v: &JsValue<'code>, p: &PropertyKey) -> JsValue<'code> {
    let o = to_object(v);
    if let JsValue::Object(o) = o {
        let o = (*o).borrow();
        o.get(p, v)
    } else {
        v.clone()
    }
}

pub fn get_method(v: &JsValue, p: &PropertyKey) -> JsValue {
    let f = get_v(v, p);
    match &f {
        JsValue::Undefined => JsValue::Undefined,
        JsValue::Null => JsValue::Undefined,
        JsValue::Object(o) => {
            if (*o).borrow().is_callable() {
                f
            } else {
                JsValue::Error(JErrorType::TypeError(format!("'{}' is not a function", p)))
            }
        }
        JsValue::Error(_) => f,
        _ => JsValue::Error(JErrorType::TypeError(format!("'{}' is not a function", p))),
    }
}
