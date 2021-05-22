use crate::parser::ast::{FunctionBodyData, HasMeta, PatternType};
use crate::runner::ds::lex_env::LexEnvironment;
use crate::runner::ds::object::{JsObject, ObjectBase, ObjectType};
use crate::runner::ds::value::{JErrorType, JsValue};
use std::cell::RefCell;
use std::rc::Rc;

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
    name: String,
    environment: Rc<RefCell<LexEnvironment>>,
    formal_parameters: Rc<Vec<Box<PatternType>>>,
    body_code: Rc<FunctionBodyData>,
    function_kind: FunctionKind,
    constructor_kind: ConstructorKind,
    is_lexical: bool,
    home_object: Option<Rc<RefCell<ObjectType>>>,
    object_base: ObjectBase,
}
impl FunctionObjectBase {
    pub fn new_normal_function(
        name: String,
        environment: Rc<RefCell<LexEnvironment>>,
        formal_parameters: Rc<Vec<Box<PatternType>>>,
        body_code: Rc<FunctionBodyData>,
        home_object: Rc<RefCell<ObjectType>>,
    ) -> Self {
        FunctionObjectBase {
            name,
            environment,
            formal_parameters,
            body_code,
            home_object: Some(home_object),
            function_kind: FunctionKind::Normal,
            constructor_kind: ConstructorKind::None,
            is_lexical: false,
            object_base: ObjectBase::new(),
        }
    }

    pub fn new_generator_function(
        name: String,
        environment: Rc<RefCell<LexEnvironment>>,
        formal_parameters: Rc<Vec<Box<PatternType>>>,
        body_code: Rc<FunctionBodyData>,
        home_object: Rc<RefCell<ObjectType>>,
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
        home_object: Rc<RefCell<ObjectType>>,
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

    fn call<'a>(&'a self, this: &'a JsValue, args: Vec<JsValue>) -> JsValue {
        if let FunctionKind::ClassConstructor = self.get_function_object_base().function_kind {
            JsValue::Error(JErrorType::TypeError(format!(
                "'{}' is a class constructor",
                self.get_function_object_base().name
            )))
        } else {
            todo!()
        }
    }

    fn construct(&self, args: Vec<JsValue>, o: Rc<RefCell<ObjectType>>) -> JsValue {
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

    fn as_super_trait(&self) -> &dyn JsObject {
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

    fn call(&self, _this: &JsValue, args: Vec<JsValue>) -> JsValue {
        let mut input_args = args;
        let mut new_args = self.bound_arguments.clone();
        new_args.append(&mut input_args);
        (*self.bound_target_function)
            .borrow()
            .call(&self.bound_this, new_args)
    }

    fn construct(&self, args: Vec<JsValue>, o: Rc<RefCell<ObjectType>>) -> JsValue {
        todo!()
    }
}
