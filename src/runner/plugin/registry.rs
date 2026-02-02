//! Built-in registry for managing built-in objects and plugins.

use std::collections::HashMap;
use std::path::Path;

use super::types::{BuiltInFn, BuiltInObject, PluginInfo};
use super::config::PluginConfig;
use crate::runner::std_lib::register_core_builtins;

/// Error type for plugin operations.
#[derive(Debug)]
pub enum PluginError {
    /// Plugin not found at the specified path.
    NotFound(String),
    /// Plugin failed to load.
    LoadError(String),
    /// Plugin registration failed.
    RegistrationError(String),
    /// Configuration error.
    ConfigError(String),
    /// Object not found in registry.
    ObjectNotFound(String),
    /// Method not found on object.
    MethodNotFound(String, String),
}

impl std::fmt::Display for PluginError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PluginError::NotFound(path) => write!(f, "Plugin not found: {}", path),
            PluginError::LoadError(msg) => write!(f, "Plugin load error: {}", msg),
            PluginError::RegistrationError(msg) => write!(f, "Plugin registration error: {}", msg),
            PluginError::ConfigError(msg) => write!(f, "Plugin config error: {}", msg),
            PluginError::ObjectNotFound(name) => write!(f, "Object not found: {}", name),
            PluginError::MethodNotFound(obj, method) => {
                write!(f, "Method not found: {}.{}", obj, method)
            }
        }
    }
}

impl std::error::Error for PluginError {}

/// Loaded plugin information.
pub struct LoadedPlugin {
    pub info: PluginInfo,
    pub source: PluginSource,
}

/// Source of a loaded plugin.
pub enum PluginSource {
    /// Built-in (compiled with the runtime).
    BuiltIn,
    /// Native dynamic library.
    Native(String),
    /// JavaScript file.
    JavaScript(String),
}

/// Registry for built-in objects.
/// Manages all built-in objects and their methods, supports plugin loading and overrides.
pub struct BuiltInRegistry {
    /// All registered built-in objects.
    objects: HashMap<String, BuiltInObject>,

    /// Loaded plugins.
    plugins: Vec<LoadedPlugin>,

    /// Override chain (object.method -> list of override sources).
    overrides: HashMap<String, Vec<String>>,
}

impl BuiltInRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        BuiltInRegistry {
            objects: HashMap::new(),
            plugins: Vec::new(),
            overrides: HashMap::new(),
        }
    }

    /// Create a registry with core built-ins (console, Object, Array basics).
    pub fn with_core() -> Self {
        let mut registry = Self::new();

        // Register core built-ins from std_lib
        register_core_builtins(&mut registry);

        // Mark that we have core built-ins loaded
        registry.plugins.push(LoadedPlugin {
            info: PluginInfo::new("core", "0.1.0")
                .with_provides(vec![
                    "console".to_string(),
                    "Object".to_string(),
                    "Array".to_string(),
                    "String".to_string(),
                    "Number".to_string(),
                    "Math".to_string(),
                    "JSON".to_string(),
                    "Error".to_string(),
                ]),
            source: PluginSource::BuiltIn,
        });

        registry
    }

    /// Register a built-in object (programmatic API).
    pub fn register_object(&mut self, obj: BuiltInObject) {
        self.objects.insert(obj.name.clone(), obj);
    }

    /// Get a registered object by name.
    pub fn get_object(&self, name: &str) -> Option<&BuiltInObject> {
        self.objects.get(name)
    }

    /// Get a mutable reference to a registered object.
    pub fn get_object_mut(&mut self, name: &str) -> Option<&mut BuiltInObject> {
        self.objects.get_mut(name)
    }

    /// Override an existing built-in method.
    pub fn override_method(
        &mut self,
        object: &str,
        method: &str,
        func: BuiltInFn,
    ) -> Result<(), PluginError> {
        let obj = self
            .objects
            .get_mut(object)
            .ok_or_else(|| PluginError::ObjectNotFound(object.to_string()))?;

        obj.methods.insert(method.to_string(), func);

        // Track the override
        let key = format!("{}.{}", object, method);
        self.overrides
            .entry(key)
            .or_insert_with(Vec::new)
            .push("manual".to_string());

        Ok(())
    }

    /// Get a built-in function for execution.
    pub fn get_method(&self, object: &str, method: &str) -> Option<&BuiltInFn> {
        self.objects
            .get(object)
            .and_then(|obj| obj.methods.get(method))
    }

    /// Get a constructor function for an object.
    pub fn get_constructor(&self, object: &str) -> Option<&BuiltInFn> {
        self.objects
            .get(object)
            .and_then(|obj| obj.constructor.as_ref())
    }

    /// Check if an object exists in the registry.
    pub fn has_object(&self, name: &str) -> bool {
        self.objects.contains_key(name)
    }

    /// Check if a method exists on an object.
    pub fn has_method(&self, object: &str, method: &str) -> bool {
        self.objects
            .get(object)
            .map(|obj| obj.methods.contains_key(method))
            .unwrap_or(false)
    }

    /// Load native plugin from dynamic library.
    /// Note: This is a placeholder - actual implementation requires unsafe FFI code
    /// and the `libloading` crate for cross-platform support.
    pub fn load_native_plugin(&mut self, path: &Path) -> Result<PluginInfo, PluginError> {
        // TODO: Implement using libloading crate
        // The plugin must export:
        // - plugin_info() -> PluginInfo
        // - plugin_register(&mut BuiltInRegistry)
        Err(PluginError::LoadError(format!(
            "Native plugin loading not yet implemented: {}",
            path.display()
        )))
    }

    /// Load JavaScript plugin from file.
    /// Note: This requires the runtime to be functional to execute the JS code.
    pub fn load_js_plugin(&mut self, path: &Path) -> Result<PluginInfo, PluginError> {
        // TODO: Implement after runtime is complete
        // The JS file should export __plugin__ metadata and object definitions
        Err(PluginError::LoadError(format!(
            "JavaScript plugin loading not yet implemented: {}",
            path.display()
        )))
    }

    /// Load plugins from config file.
    pub fn load_from_config(&mut self, config_path: &Path) -> Result<(), PluginError> {
        let config = PluginConfig::load(config_path)?;

        for native_plugin in &config.native_plugins {
            if native_plugin.enabled {
                self.load_native_plugin(Path::new(&native_plugin.path))?;
            }
        }

        for js_plugin in &config.js_plugins {
            if js_plugin.enabled {
                self.load_js_plugin(Path::new(&js_plugin.path))?;
            }
        }

        Ok(())
    }

    /// Get list of all registered object names.
    pub fn object_names(&self) -> Vec<&String> {
        self.objects.keys().collect()
    }

    /// Get list of all loaded plugins.
    pub fn loaded_plugins(&self) -> &[LoadedPlugin] {
        &self.plugins
    }
}

impl Default for BuiltInRegistry {
    fn default() -> Self {
        Self::with_core()
    }
}
