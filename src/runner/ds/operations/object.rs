use crate::runner::ds::object::{JsObject, ObjectType};
use crate::runner::ds::object_property::PropertyKey;
use crate::runner::ds::value::{JErrorType, JsValue};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub fn get(o: &Rc<RefCell<dyn JsObject>>, p: PropertyKey) -> JsValue {
    let o = o.deref().borrow().as_js_object();
    o.get(&p, JsValue::Object(o.clone()))
}

pub fn get_v(v: &JsValue, p: PropertyKey) -> JsValue {
    let o = to_object(v);
}
