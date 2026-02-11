//! Super-global environment — the bottom of the scope chain.
//!
//! This environment sits below the global scope and lazily resolves
//! built-in and plugin-provided objects on first access. Objects are
//! cached after first resolution so each name is materialized at most once.

use std::collections::HashMap;

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::JsValue;
use crate::runner::plugin::resolver::PluginResolver;
use crate::runner::plugin::types::EvalContext;

/// An environment record that lazily resolves bindings from plugin resolvers.
///
/// JS code cannot create bindings here — it is read-only from the JS perspective.
/// Resolvers are queried in registration order; the first to claim a name wins.
/// Resolved values are cached so each resolver is consulted at most once per name.
pub struct SuperGlobalEnvironment {
    /// Registered plugin resolvers, queried in order.
    resolvers: Vec<Box<dyn PluginResolver>>,
    /// Cache of already-resolved bindings (name → value).
    cache: HashMap<String, JsValue>,
    /// Cache of which resolver index owns which name.
    resolver_map: HashMap<String, usize>,
}

impl SuperGlobalEnvironment {
    pub fn new() -> Self {
        SuperGlobalEnvironment {
            resolvers: Vec::new(),
            cache: HashMap::new(),
            resolver_map: HashMap::new(),
        }
    }

    /// Register a plugin resolver. Resolvers are queried in registration order.
    pub fn add_resolver(&mut self, resolver: Box<dyn PluginResolver>) {
        self.resolvers.push(resolver);
    }

    /// Find which resolver (if any) provides the given name.
    fn find_resolver_index(&self, name: &str) -> Option<usize> {
        // Check the cache first
        if let Some(&idx) = self.resolver_map.get(name) {
            return Some(idx);
        }
        // Query resolvers in order
        for (i, resolver) in self.resolvers.iter().enumerate() {
            if resolver.has_binding(name) {
                return Some(i);
            }
        }
        None
    }

    /// Call a method on a super-global object, dispatching to the owning resolver.
    ///
    /// Returns `None` if no resolver owns `object_name` or the resolver
    /// doesn't provide the method (allowing fallback to property lookup).
    pub fn call_method(
        &self,
        object_name: &str,
        method_name: &str,
        ctx: &mut EvalContext,
        this: JsValue,
        args: Vec<JsValue>,
    ) -> Option<Result<JsValue, JErrorType>> {
        let idx = if let Some(&idx) = self.resolver_map.get(object_name) {
            idx
        } else {
            self.find_resolver_index(object_name)?
        };
        self.resolvers[idx].call_method(object_name, method_name, ctx, this, args)
    }

    /// Call a constructor on a super-global object.
    pub fn call_constructor(
        &self,
        object_name: &str,
        ctx: &mut EvalContext,
        args: Vec<JsValue>,
    ) -> Option<Result<JsValue, JErrorType>> {
        let idx = if let Some(&idx) = self.resolver_map.get(object_name) {
            idx
        } else {
            self.find_resolver_index(object_name)?
        };
        self.resolvers[idx].call_constructor(object_name, ctx, args)
    }

    /// Check if any resolver provides the given name.
    pub fn has_name(&self, name: &str) -> bool {
        self.cache.contains_key(name) || self.find_resolver_index(name).is_some()
    }

    /// Resolve a name, caching the result.
    /// `ctx` is needed because some resolvers may need it during materialization.
    pub fn resolve_binding(
        &mut self,
        name: &str,
        ctx: &mut EvalContext,
    ) -> Result<JsValue, JErrorType> {
        // Return cached value if available
        if let Some(val) = self.cache.get(name) {
            return Ok(val.clone());
        }

        // Find the owning resolver
        if let Some(idx) = self.find_resolver_index(name) {
            let value = self.resolvers[idx].resolve(name, ctx)?;
            self.cache.insert(name.to_string(), value.clone());
            self.resolver_map.insert(name.to_string(), idx);
            Ok(value)
        } else {
            Err(JErrorType::ReferenceError(format!(
                "{} is not defined",
                name
            )))
        }
    }

    /// Get a reference to the resolvers (for inspection/testing).
    pub fn resolvers(&self) -> &[Box<dyn PluginResolver>] {
        &self.resolvers
    }
}
