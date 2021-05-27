use std::cell::RefCell;
use std::rc::Rc;

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::object::{JsObject, ObjectType};
use crate::runner::ds::object_property::PropertyKey;
use crate::runner::ds::operations::type_conversion::to_object;
use crate::runner::ds::value::JsValue;

pub fn get(o: &Rc<RefCell<ObjectType>>, p: &PropertyKey) -> Result<JsValue, JErrorType> {
    let o1 = (**o).borrow();
    o1.as_js_object().get(p, &JsValue::Object(o.clone()))
}

pub fn get_v(v: &JsValue, p: &PropertyKey) -> Result<JsValue, JErrorType> {
    let o = to_object(v)?;
    if let JsValue::Object(o) = o {
        let o = (*o).borrow();
        o.as_js_object().get(p, v)
    } else {
        Ok(v.clone())
    }
}

pub fn get_method(v: &JsValue, p: &PropertyKey) -> Result<JsValue, JErrorType> {
    let f = get_v(v, p)?;
    match &f {
        JsValue::Undefined => Ok(JsValue::Undefined),
        JsValue::Null => Ok(JsValue::Undefined),
        JsValue::Object(o) => {
            if (*o).borrow().is_callable() {
                Ok(f)
            } else {
                Err(JErrorType::TypeError(format!("'{}' is not a function", p)))
            }
        }
        _ => Err(JErrorType::TypeError(format!("'{}' is not a function", p))),
    }
}
