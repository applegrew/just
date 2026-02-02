//! Core built-ins registration.
//!
//! This module provides the function to register all core built-in objects
//! with the BuiltInRegistry.

use crate::runner::plugin::registry::BuiltInRegistry;

use super::console;
use super::object;
use super::array;
use super::string;
use super::number;
use super::math;
use super::json;
use super::error;

/// Register all core built-in objects with the registry.
pub fn register_core_builtins(registry: &mut BuiltInRegistry) {
    // Register in order (some may depend on Object)
    object::register(registry);
    array::register(registry);
    string::register(registry);
    number::register(registry);
    math::register(registry);
    json::register(registry);
    error::register(registry);
    console::register(registry);
}
