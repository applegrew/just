//! Core types for the plugin architecture.

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::heap::{Heap, HeapConfig};
use crate::runner::ds::value::JsValue;
use crate::parser::ast::FunctionData;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Shared heap type for use across contexts.
pub type SharedHeap = Rc<RefCell<Heap>>;

/// Execution context passed to native functions.
/// This will be expanded as the runtime is implemented.
pub struct EvalContext {
    /// The global `this` value.
    pub global_this: Option<JsValue>,
    /// Shared heap for memory allocation tracking.
    pub heap: SharedHeap,
}

impl EvalContext {
    /// Create a new evaluation context with default heap configuration.
    pub fn new() -> Self {
        EvalContext {
            global_this: None,
            heap: Rc::new(RefCell::new(Heap::default())),
        }
    }

    /// Create a new evaluation context with a specific heap configuration.
    pub fn with_heap_config(config: HeapConfig) -> Self {
        EvalContext {
            global_this: None,
            heap: Rc::new(RefCell::new(Heap::new(config))),
        }
    }

    /// Track a heap allocation.
    pub fn allocate(&self, bytes: usize) -> Result<(), JErrorType> {
        self.heap.borrow_mut().allocate(bytes)
    }

    /// Track a heap deallocation.
    pub fn deallocate(&self, bytes: usize) {
        self.heap.borrow_mut().deallocate(bytes)
    }

    /// Get the current heap usage in bytes.
    pub fn heap_usage(&self) -> usize {
        self.heap.borrow().get_allocated()
    }
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Function signature for built-in methods.
/// Native functions receive the evaluation context, `this` value, and arguments.
pub type NativeFn = fn(
    ctx: &mut EvalContext,
    this: JsValue,
    args: Vec<JsValue>,
) -> Result<JsValue, JErrorType>;

/// Built-in function - either compiled-in or plugin-provided.
pub enum BuiltInFn {
    /// Direct function pointer - zero overhead for compiled-in functions.
    Native(NativeFn),

    /// Plugin-provided function - small vtable indirection cost.
    Plugin(Box<dyn Fn(&mut EvalContext, JsValue, Vec<JsValue>) -> Result<JsValue, JErrorType> + Send + Sync>),

    /// JavaScript implementation - interpreted at runtime.
    Script(Rc<FunctionData>),
}

impl BuiltInFn {
    /// Execute this built-in function.
    pub fn call(
        &self,
        ctx: &mut EvalContext,
        this: JsValue,
        args: Vec<JsValue>,
    ) -> Result<JsValue, JErrorType> {
        match self {
            BuiltInFn::Native(f) => f(ctx, this, args),
            BuiltInFn::Plugin(f) => f(ctx, this, args),
            BuiltInFn::Script(_f) => {
                // TODO: Implement script execution when runtime is ready
                Err(JErrorType::TypeError("Script built-ins not yet implemented".to_string()))
            }
        }
    }
}

/// Built-in object definition.
/// Represents a JavaScript built-in object like Array, Object, String, etc.
pub struct BuiltInObject {
    /// Name of the object (e.g., "Array", "Object", "Math").
    pub name: String,

    /// Parent prototype name, if any (e.g., "Object" for most built-ins).
    pub prototype: Option<String>,

    /// Methods defined on this object or its prototype.
    pub methods: HashMap<String, BuiltInFn>,

    /// Static properties.
    pub properties: HashMap<String, JsValue>,

    /// Constructor function, if this object is constructable.
    pub constructor: Option<BuiltInFn>,
}

impl BuiltInObject {
    /// Create a new built-in object with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        BuiltInObject {
            name: name.into(),
            prototype: Some("Object".to_string()),
            methods: HashMap::new(),
            properties: HashMap::new(),
            constructor: None,
        }
    }

    /// Set the prototype chain parent.
    pub fn with_prototype(mut self, prototype: impl Into<String>) -> Self {
        self.prototype = Some(prototype.into());
        self
    }

    /// Set no prototype (for objects like Object.prototype itself).
    pub fn with_no_prototype(mut self) -> Self {
        self.prototype = None;
        self
    }

    /// Add a native method.
    pub fn add_method(mut self, name: impl Into<String>, func: NativeFn) -> Self {
        self.methods.insert(name.into(), BuiltInFn::Native(func));
        self
    }

    /// Add a property.
    pub fn add_property(mut self, name: impl Into<String>, value: JsValue) -> Self {
        self.properties.insert(name.into(), value);
        self
    }

    /// Set the constructor function.
    pub fn with_constructor(mut self, constructor: NativeFn) -> Self {
        self.constructor = Some(BuiltInFn::Native(constructor));
        self
    }
}

/// Plugin metadata.
/// Contains information about a loaded plugin.
#[derive(Debug, Clone)]
pub struct PluginInfo {
    /// Plugin name.
    pub name: String,

    /// Plugin version.
    pub version: String,

    /// List of object names this plugin provides.
    pub provides: Vec<String>,
}

impl PluginInfo {
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        PluginInfo {
            name: name.into(),
            version: version.into(),
            provides: Vec::new(),
        }
    }

    pub fn with_provides(mut self, provides: Vec<String>) -> Self {
        self.provides = provides;
        self
    }
}
