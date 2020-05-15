use std::iter::Map;
use std::rc::Weak;

pub enum Declarable {
    FunctionDef(CallableDefinition),
    GeneratorDef,
    ClassDef,
}

pub enum VariableData {
    String,
    Integer,
    Float,
    Boolean,
    Object(ObjectTypes),
}

pub enum ObjectTypes {
    FunctionObject,
    GeneratorObject,
    ArrowFunctionObject,
    ArrayObject,
    OtherObject,
}

pub struct Scope {
    parent: Weak<Scope>,
    declarations: Vec<Declarable>,
    variables: Map<String, VariableData>,
}

pub struct CallableDefinition {
    owned_scope: Scope,
    instructions: Vec<OpInstruction>,
}

pub struct AssignmentVariables {
    ids: Vec<String>,
    value: Box<OpInstruction>,
    capture: AssignmentCapture,
}

pub struct AssignmentIdx {
    index: u32,
    default: VariableData,
}

pub struct RestAssignmentIdx {
    start_index: u32,
    default: VariableData,
}

pub struct AssignmentPath {
    path: Vec<String>,
    default: VariableData,
}

pub enum AssignmentCapture {
    OneToOneCapture, // It is an error if ids has more than one element for this case
    ArrayDestructCapture {
        params: Vec<AssignmentIdx>,
        rest_param: Option<RestAssignmentIdx>,
    },
    ObjectDestructCapture(Vec<AssignmentPath>),
}

pub enum OpInstruction {
    ReturnOp(Box<OpInstruction>),
    VarOp(AssignmentVariables),
    LetOp(AssignmentVariables),
    ConstOp(AssignmentVariables),
    DefineFunction {
        id: String,
        arguments: Vec<AssignmentVariables>,
        instructions: Vec<Box<OpInstruction>>,
    },
}
