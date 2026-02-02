//! Object built-in.
//!
//! Provides Object constructor and prototype methods.

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::JsValue;
use crate::runner::plugin::registry::BuiltInRegistry;
use crate::runner::plugin::types::{BuiltInObject, EvalContext};

/// Register the Object built-in with the registry.
pub fn register(registry: &mut BuiltInRegistry) {
    let object = BuiltInObject::new("Object")
        .with_no_prototype()
        .with_constructor(object_constructor)
        .add_method("toString", object_to_string)
        .add_method("valueOf", object_value_of)
        .add_method("hasOwnProperty", object_has_own_property)
        .add_method("keys", object_keys)
        .add_method("values", object_values)
        .add_method("entries", object_entries)
        .add_method("assign", object_assign);

    registry.register_object(object);
}

/// Object constructor.
fn object_constructor(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // If called with a value, convert it to object
    // If called with null/undefined or no args, create empty object
    if args.is_empty() {
        // TODO: Create a new empty object when object creation is implemented
        Ok(JsValue::Undefined)
    } else {
        let arg = &args[0];
        match arg {
            JsValue::Null | JsValue::Undefined => {
                // Create empty object
                Ok(JsValue::Undefined)
            }
            JsValue::Object(_) => {
                // Return the object itself
                Ok(arg.clone())
            }
            _ => {
                // TODO: Box primitives (create wrapper objects)
                Ok(arg.clone())
            }
        }
    }
}

/// Object.prototype.toString
fn object_to_string(
    _ctx: &mut EvalContext,
    this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    let tag = match &this {
        JsValue::Undefined => "Undefined",
        JsValue::Null => "Null",
        JsValue::Boolean(_) => "Boolean",
        JsValue::Number(_) => "Number",
        JsValue::String(_) => "String",
        JsValue::Symbol(_) => "Symbol",
        JsValue::Object(_) => "Object",
    };
    Ok(JsValue::String(format!("[object {}]", tag)))
}

/// Object.prototype.valueOf
fn object_value_of(
    _ctx: &mut EvalContext,
    this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // Returns the primitive value of the object (for primitives, returns itself)
    Ok(this)
}

/// Object.prototype.hasOwnProperty
fn object_has_own_property(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    if args.is_empty() {
        return Ok(JsValue::Boolean(false));
    }

    // TODO: Implement actual property lookup when object properties are implemented
    // For now, always return false
    Ok(JsValue::Boolean(false))
}

/// Object.keys - Get all enumerable own property names.
fn object_keys(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    if args.is_empty() {
        return Err(JErrorType::TypeError(
            "Cannot convert undefined or null to object".to_string(),
        ));
    }

    // TODO: Return array of property names when object/array structures are implemented
    Ok(JsValue::Undefined)
}

/// Object.values - Get all enumerable own property values.
fn object_values(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    if args.is_empty() {
        return Err(JErrorType::TypeError(
            "Cannot convert undefined or null to object".to_string(),
        ));
    }

    // TODO: Return array of property values when object/array structures are implemented
    Ok(JsValue::Undefined)
}

/// Object.entries - Get all enumerable own property [key, value] pairs.
fn object_entries(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    if args.is_empty() {
        return Err(JErrorType::TypeError(
            "Cannot convert undefined or null to object".to_string(),
        ));
    }

    // TODO: Return array of [key, value] pairs when object/array structures are implemented
    Ok(JsValue::Undefined)
}

/// Object.assign - Copy properties from source objects to target.
fn object_assign(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    if args.is_empty() {
        return Err(JErrorType::TypeError(
            "Cannot convert undefined or null to object".to_string(),
        ));
    }

    // TODO: Implement property copying when object properties are implemented
    Ok(args[0].clone())
}
