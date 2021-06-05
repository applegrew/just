use std::cell::RefCell;
use std::rc::Rc;

use crate::parser::ast::{FunctionBodyData, HasMeta, PatternType};
use crate::runner::ds::env_record::{new_function_environment, EnvironmentRecordType};
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::execution_context::{ExecutionContext, ExecutionContextStack};
use crate::runner::ds::lex_env::{JsLexEnvironmentType, LexEnvironment};
use crate::runner::ds::object::{JsObject, JsObjectType, ObjectBase, ObjectType};
use crate::runner::ds::realm::{CodeRealm, JsCodeRealmType};
use crate::runner::ds::value::{JsValue, JsValueOrSelf};
use std::ptr;

pub enum FunctionKind {
    Normal,
    ClassConstructor,
    Generator,
}

pub enum ConstructorKind {
    Base,
    Derived,
    None,
}

pub struct FunctionObjectBase {
    pub name: String,
    pub environment: JsLexEnvironmentType,
    pub formal_parameters: Vec<Rc<PatternType>>,
    pub body_code: Rc<FunctionBodyData>,
    pub function_kind: FunctionKind,
    pub constructor_kind: ConstructorKind,
    pub is_lexical: bool,
    pub home_object: Option<JsObjectType>,
    pub realm: JsCodeRealmType,
    pub object_base: ObjectBase,
}
impl FunctionObjectBase {
    pub fn new_normal_function(
        name: String,
        environment: Rc<RefCell<LexEnvironment>>,
        formal_parameters: Rc<Vec<Box<PatternType>>>,
        body_code: Rc<FunctionBodyData>,
        home_object: JsObjectType,
    ) -> Self {
        FunctionObjectBase {
            name,
            environment,
            formal_parameters,
            body_code,
            home_object: Some(home_object),
            function_kind: FunctionKind::Normal,
            constructor_kind: ConstructorKind::Base,
            is_lexical: false,
            object_base: ObjectBase::new(),
        }
    }

    pub fn new_generator_function(
        name: String,
        environment: Rc<RefCell<LexEnvironment>>,
        formal_parameters: Rc<Vec<Box<PatternType>>>,
        body_code: Rc<FunctionBodyData>,
        home_object: JsObjectType,
    ) -> Self {
        FunctionObjectBase {
            name,
            environment,
            formal_parameters,
            body_code,
            home_object: Some(home_object),
            function_kind: FunctionKind::Generator,
            constructor_kind: ConstructorKind::None,
            is_lexical: false,
            object_base: ObjectBase::new(),
        }
    }

    pub fn new_arrow_function(
        environment: Rc<RefCell<LexEnvironment>>,
        formal_parameters: Rc<Vec<Box<PatternType>>>,
        body_code: Rc<FunctionBodyData>,
    ) -> Self {
        FunctionObjectBase {
            name: String::new(),
            environment,
            formal_parameters,
            body_code,
            home_object: None,
            function_kind: FunctionKind::Normal,
            constructor_kind: ConstructorKind::None,
            is_lexical: true,
            object_base: ObjectBase::new(),
        }
    }

    pub fn new_constructor_function(
        name: String,
        environment: Rc<RefCell<LexEnvironment>>,
        formal_parameters: Rc<Vec<Box<PatternType>>>,
        body_code: Rc<FunctionBodyData>,
        home_object: JsObjectType,
        constructor_kind: ConstructorKind,
    ) -> Self {
        FunctionObjectBase {
            name,
            environment,
            formal_parameters,
            body_code,
            home_object: Some(home_object),
            function_kind: FunctionKind::ClassConstructor,
            constructor_kind,
            is_lexical: false,
            object_base: ObjectBase::new(),
        }
    }

    pub fn get_object_base_mut(&mut self) -> &mut ObjectBase {
        &mut self.object_base
    }

    pub fn get_object_base(&self) -> &ObjectBase {
        &self.object_base
    }
}

pub trait JsFunctionObject: JsObject {
    fn get_function_object_base_mut(&mut self) -> &mut FunctionObjectBase;

    fn get_function_object_base(&self) -> &FunctionObjectBase;

    fn as_js_function_object(&self) -> &dyn JsFunctionObject;

    fn as_js_function_object_mut(&mut self) -> &mut dyn JsFunctionObject;

    fn call(
        &self,
        fat_self: &JsObjectType,
        ctx_stack: &mut ExecutionContextStack,
        this: JsValueOrSelf,
        args: Vec<JsValue>,
    ) -> Result<JsValue, JErrorType> {
        debug_assert!(ptr::eq(
            fat_self.borrow().as_js_object(),
            self.as_js_object()
        ));

        if let FunctionKind::ClassConstructor = self.get_function_object_base().function_kind {
            Err(JErrorType::TypeError(format!(
                "'{}' is a class constructor",
                self.get_function_object_base().name
            )))
        } else {
            let caller_ctx = ctx_stack.get_running_execution_ctx().unwrap();

            todo!()
        }
    }

    fn construct(&self, args: Vec<JsValue>, o: JsObjectType) -> Result<JsValue, JErrorType> {
        todo!()
    }
}

pub struct BoundFunctionObject {
    bound_target_function: Rc<RefCell<dyn JsFunctionObject>>,
    bound_this: JsValue,
    bound_arguments: Vec<JsValue>,
    function_object: FunctionObjectBase,
}
impl JsObject for BoundFunctionObject {
    fn get_object_base_mut(&mut self) -> &mut ObjectBase {
        self.function_object.get_object_base_mut()
    }

    fn get_object_base(&self) -> &ObjectBase {
        self.function_object.get_object_base()
    }

    fn as_js_object(&self) -> &dyn JsObject {
        self
    }

    fn as_js_object_mut(&mut self) -> &mut dyn JsObject {
        self
    }

    fn to_string(&self) -> String {
        format!(
            "function [bounded function] ({}) {{ [native code] }}",
            (*self.get_function_object_base().formal_parameters)
                .iter()
                .map(|a| { a.get_meta().to_formatted_code() })
                .collect::<Vec<String>>()
                .join(",")
        )
    }
}
impl JsFunctionObject for BoundFunctionObject {
    fn get_function_object_base_mut(&mut self) -> &mut FunctionObjectBase {
        &mut self.function_object
    }

    fn get_function_object_base(&self) -> &FunctionObjectBase {
        &self.function_object
    }

    fn as_js_function_object(&self) -> &dyn JsFunctionObject {
        self
    }

    fn as_js_function_object_mut(&mut self) -> &mut dyn JsFunctionObject {
        self
    }

    fn call(&self, _this: JsValueOrSelf, args: Vec<JsValue>) -> Result<JsValue, JErrorType> {
        let mut input_args = args;
        let mut new_args = self.bound_arguments.clone();
        new_args.append(&mut input_args);
        (*self.bound_target_function)
            .borrow()
            .call(JsValueOrSelf::ValueRef(&self.bound_this), new_args)
    }

    fn construct(&self, args: Vec<JsValue>, o: JsObjectType) -> Result<JsValue, JErrorType> {
        todo!()
    }
}

pub fn prepare_for_ordinary_call(
    f: &JsObjectType,
    new_target: Option<JsObjectType>,
) -> ExecutionContext {
    let local_env = new_function_environment(f.clone(), new_target);
    ExecutionContext {
        function: Some(f.clone()),
        realm: (**f)
            .borrow()
            .as_js_function_object()
            .get_function_object_base()
            .realm
            .clone(),
        lex_env: local_env,
        var_env: local_env.clone(),
    }
}

pub fn ordinary_call_bind_this(
    f: &dyn JsFunctionObject,
    callee_context: &ExecutionContext,
    this_argument: JsValue,
) -> Result<bool, JErrorType> {
    if !f.get_function_object_base().is_lexical {
        if let EnvironmentRecordType::Function(f_env) =
            &mut callee_context.lex_env.borrow_mut().inner
        {
            return f_env.bind_this_value(this_argument);
        }
    }
    Ok(false)
}

///
/// When an execution context is established for evaluating an ECMAScript function a new
/// function Environment Record is created and bindings for each formal parameter are instantiated
/// in that Environment Record. Each declaration in the function body is also instantiated. If the
/// function’s formal parameters do not include any default value initializers then the body
/// declarations are instantiated in the same Environment Record as the parameters. If default
/// value parameter initializers exist, a second Environment Record is created for the body
/// declarations. Formal parameters and functions are initialized as part of
/// function_declaration_instantiation. All other bindings are initialized during evaluation of the
/// function body.
///
pub fn function_declaration_instantiation(
    f: &dyn JsFunctionObject,
    callee_context: &ExecutionContext,
    argument_list: Vec<JsValue>,
) {
    let env = &callee_context.lex_env;
    let env_rec = env.borrow().inner.as_env_record();
    let formals = &f.get_function_object_base().formal_parameters;
}
