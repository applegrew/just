//! Plugin architecture for built-in objects.
//!
//! This module provides a flexible plugin system for extending the JavaScript runtime
//! with custom built-in objects and methods.

pub mod types;
pub mod registry;
pub mod config;

pub use types::{BuiltInFn, BuiltInObject, NativeFn, PluginInfo};
pub use registry::BuiltInRegistry;
pub use config::PluginConfig;
