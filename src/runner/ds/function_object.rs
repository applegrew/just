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

pub struct FunctionObjectBase<'code> {
    name: String,
    environment: Rc<RefCell<LexEnvironment<'code>>>,
    formal_parameters: Rc<Vec<Box<PatternType<'code>>>>,
    body_code: Rc<FunctionBodyData<'code>>,
    function_kind: FunctionKind,
    constructor_kind: ConstructorKind,
    is_lexical: bool,
    home_object: Option<Rc<RefCell<ObjectType<'code>>>>,
    object_base: ObjectBase<'code>,
}
impl<'code> FunctionObjectBase<'code> {
    pub fn new_normal_function(
        name: String,
        environment: Rc<RefCell<LexEnvironment<'code>>>,
        formal_parameters: Rc<Vec<Box<PatternType<'code>>>>,
        body_code: Rc<FunctionBodyData<'code>>,
        home_object: Rc<RefCell<ObjectType<'code>>>,
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
        environment: Rc<RefCell<LexEnvironment<'code>>>,
        formal_parameters: Rc<Vec<Box<PatternType<'code>>>>,
        body_code: Rc<FunctionBodyData<'code>>,
        home_object: Rc<RefCell<ObjectType<'code>>>,
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
        environment: Rc<RefCell<LexEnvironment<'code>>>,
        formal_parameters: Rc<Vec<Box<PatternType<'code>>>>,
        body_code: Rc<FunctionBodyData<'code>>,
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
        environment: Rc<RefCell<LexEnvironment<'code>>>,
        formal_parameters: Rc<Vec<Box<PatternType<'code>>>>,
        body_code: Rc<FunctionBodyData<'code>>,
        home_object: Rc<RefCell<ObjectType<'code>>>,
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

pub trait JsFunctionObject<'code>: JsObject<'code> {
    fn get_function_object_base_mut(&mut self) -> &mut FunctionObjectBase<'code>;

    fn get_function_object_base(&self) -> &FunctionObjectBase;

    fn call(&self, this: &JsValue, args: Vec<JsValue>) -> JsValue {
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

pub struct BoundFunctionObject<'code> {
    bound_target_function: Rc<RefCell<dyn JsFunctionObject<'code>>>,
    bound_this: JsValue<'code>,
    bound_arguments: Vec<JsValue<'code>>,
    function_object: FunctionObjectBase<'code>,
}
impl<'code> JsObject<'code> for BoundFunctionObject<'code> {
    fn get_object_base_mut(&mut self) -> &mut ObjectBase<'code> {
        self.function_object.get_object_base_mut()
    }

    fn get_object_base(&self) -> &ObjectBase {
        self.function_object.get_object_base()
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
impl<'code> JsFunctionObject<'code> for BoundFunctionObject<'code> {
    fn get_function_object_base_mut(&mut self) -> &mut FunctionObjectBase<'code> {
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
