//! Array built-in.
//!
//! Provides Array constructor and prototype methods.

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::JsValue;
use crate::runner::plugin::registry::BuiltInRegistry;
use crate::runner::plugin::types::{BuiltInObject, EvalContext};

/// Register the Array built-in with the registry.
pub fn register(registry: &mut BuiltInRegistry) {
    let array = BuiltInObject::new("Array")
        .with_constructor(array_constructor)
        .add_method("push", array_push)
        .add_method("pop", array_pop)
        .add_method("shift", array_shift)
        .add_method("unshift", array_unshift)
        .add_method("slice", array_slice)
        .add_method("splice", array_splice)
        .add_method("indexOf", array_index_of)
        .add_method("includes", array_includes)
        .add_method("forEach", array_for_each)
        .add_method("map", array_map)
        .add_method("filter", array_filter)
        .add_method("reduce", array_reduce)
        .add_method("find", array_find)
        .add_method("every", array_every)
        .add_method("some", array_some)
        .add_method("join", array_join)
        .add_method("concat", array_concat)
        .add_method("reverse", array_reverse)
        .add_method("sort", array_sort)
        .add_method("isArray", is_array);

    registry.register_object(array);
}

/// Array constructor.
fn array_constructor(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement array creation
    Err(JErrorType::TypeError("Array constructor not yet implemented".to_string()))
}

/// Array.prototype.push
fn array_push(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.push not yet implemented".to_string()))
}

/// Array.prototype.pop
fn array_pop(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.pop not yet implemented".to_string()))
}

/// Array.prototype.shift
fn array_shift(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.shift not yet implemented".to_string()))
}

/// Array.prototype.unshift
fn array_unshift(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.unshift not yet implemented".to_string()))
}

/// Array.prototype.slice
fn array_slice(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.slice not yet implemented".to_string()))
}

/// Array.prototype.splice
fn array_splice(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.splice not yet implemented".to_string()))
}

/// Array.prototype.indexOf
fn array_index_of(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.indexOf not yet implemented".to_string()))
}

/// Array.prototype.includes
fn array_includes(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.includes not yet implemented".to_string()))
}

/// Array.prototype.forEach
fn array_for_each(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.forEach not yet implemented".to_string()))
}

/// Array.prototype.map
fn array_map(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.map not yet implemented".to_string()))
}

/// Array.prototype.filter
fn array_filter(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.filter not yet implemented".to_string()))
}

/// Array.prototype.reduce
fn array_reduce(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.reduce not yet implemented".to_string()))
}

/// Array.prototype.find
fn array_find(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.find not yet implemented".to_string()))
}

/// Array.prototype.every
fn array_every(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.every not yet implemented".to_string()))
}

/// Array.prototype.some
fn array_some(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.some not yet implemented".to_string()))
}

/// Array.prototype.join
fn array_join(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.join not yet implemented".to_string()))
}

/// Array.prototype.concat
fn array_concat(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.concat not yet implemented".to_string()))
}

/// Array.prototype.reverse
fn array_reverse(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals are available
    Err(JErrorType::TypeError("Array.reverse not yet implemented".to_string()))
}

/// Array.prototype.sort
fn array_sort(
    _ctx: &mut EvalContext,
    _this: JsValue,
    _args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    // TODO: Implement when array internals and function calls are available
    Err(JErrorType::TypeError("Array.sort not yet implemented".to_string()))
}

/// Array.isArray - Check if value is an array.
fn is_array(
    _ctx: &mut EvalContext,
    _this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType> {
    if args.is_empty() {
        return Ok(JsValue::Boolean(false));
    }

    // TODO: Check actual array type when available
    // For now, return false since we don't have full array support
    Ok(JsValue::Boolean(false))
}
