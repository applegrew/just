//! Plugin architecture for built-in objects.
//!
//! This module provides a flexible plugin system for extending the JavaScript runtime
//! with custom built-in objects and methods.

pub mod types;
pub mod registry;
pub mod config;
pub mod resolver;
pub mod core_resolver;
pub mod super_global;

pub use types::{BuiltInFn, BuiltInObject, NativeFn, PluginInfo};
pub use registry::BuiltInRegistry;
pub use config::PluginConfig;
pub use resolver::PluginResolver;
pub use core_resolver::CorePluginResolver;
pub use super_global::SuperGlobalEnvironment;
