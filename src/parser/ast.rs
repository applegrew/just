// use std::iter::Map;
//
// pub struct AssignmentVariables {
//     ids: Vec<String>,
//     value: Box<OpInstruction>,
//     capture: AssignmentCapture,
// }
//
// pub struct AssignmentIdx {
//     index: u32,
//     default: VariableData,
// }
//
// pub struct RestAssignmentIdx {
//     start_index: u32,
//     default: VariableData,
// }
//
// pub struct AssignmentPath {
//     path: Vec<String>,
//     default: VariableData,
// }
//
// pub enum AssignmentCapture {
//     OneToOneCapture, // It is an error if ids has more than one element for this case
//     ArrayDestructCapture {
//         params: Vec<AssignmentIdx>,
//         rest_param: Option<RestAssignmentIdx>,
//     },
//     ObjectDestructCapture(Vec<AssignmentPath>),
// }
//
// pub enum OpInstruction {
//     ReturnOp(Box<OpInstruction>),
//     VarOp(AssignmentVariables),
//     LetOp(AssignmentVariables),
//     ConstOp(AssignmentVariables),
//     DefineFunction {
//         id: String,
//         arguments: Vec<AssignmentVariables>,
//         instructions: Vec<Box<OpInstruction>>,
//     },
// }

pub enum EqualityOp {
    StrictEqual(Box<Node>),
    StrictUnequal(Box<Node>),
    SoftEqual(Box<Node>),
    SoftUnequal(Box<Node>),
}

pub enum RelationalOp {
    LessThan(Box<Node>),
    LessThanOrEqual(Box<Node>),
    GreaterThan(Box<Node>),
    GreaterThanOrEqual(Box<Node>),
    InstanceOf(Box<Node>),
    In(Box<Node>),
}

pub enum ShiftOp {
    LeftShift(Box<Node>),
    RightShift(Box<Node>),
    UnsignedRightShift(Box<Node>),
}

pub enum AdditiveOp {
    Add(Box<Node>),
    Subtract(Box<Node>),
}

pub enum MultiplicativeOp {
    Multiply(Box<Node>),
    Divide(Box<Node>),
    Modulo(Box<Node>),
}

pub enum UnaryOp {
    Delete,
    Void,
    Typeof,
    PlusPlus,
    MinusMinus,
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
}

pub enum PostfixOp {
    PlusPlus,
    MinusMinus,
}

pub enum ArgumentType {
    Regular(Box<Node>),
    Rest(Box<Node>),
}

pub enum ArrayArgumentType {
    Regular(Box<Node>),
    Spread(Box<Node>),
}

pub enum PropertyType {
    ComputedNameValuePair {
        name: Box<Node>,
        value: Box<Node>,
    },
    SimpleNameValuePair {
        name: Data,
        value: Box<Node>,
    },
    MethodDefinition(Box<Node>),
    IdentifierRef {
        identifier: Data,
        default: Option<Box<Node>>,
    },
}

pub enum NodeType {
    TemplateLiteral(Vec<Box<Node>>),
    Expression(Vec<Box<Node>>),
    YieldExpression(Option<Box<Node>>),
    ConditionalExpression {
        condition: Box<Node>,
        if_true: Box<Node>,
        if_false: Box<Node>,
    },
    LogicalOrExpression(Vec<Box<Node>>),
    LogicalAndExpression(Vec<Box<Node>>),
    BitwiseOrExpression(Vec<Box<Node>>),
    BitwiseXorExpression(Vec<Box<Node>>),
    BitwiseAndExpression(Vec<Box<Node>>),
    EqualityExpression(Box<Node>, Vec<EqualityOp>),
    RelationalExpression(Box<Node>, Vec<RelationalOp>),
    ShiftExpression(Box<Node>, Vec<ShiftOp>),
    AdditiveExpression(Box<Node>, Vec<AdditiveOp>),
    MultiplicativeExpression(Box<Node>, Vec<MultiplicativeOp>),
    UnaryExpression(Vec<UnaryOp>, Box<Node>),
    PostfixExpression(Box<Node>, PostfixOp),
    NewExpression(Box<Node>),
    InstantiateObject {
        constructor: Box<Node>,
        arguments: Vec<ArgumentType>,
    },
    MemberAccessBracket {
        object: Box<Node>,
        member: Box<Node>,
    },
    MemberAccessDot {
        object: Box<Node>,
        member: Data,
    },
    TaggedLiteral {
        f: Box<Node>,
        template: Box<Node>,
    },
    ArrayLiteral(Vec<ArrayArgumentType>),
    ObjectLiteral(Vec<PropertyType>),
    CallExpression {
        f: Box<Node>,
        arguments: Vec<ArgumentType>,
    },
}

pub enum Node {
    Group(NodeType),
    Terminal(Data),
}

pub enum Data {
    IntegerLiteral(i32),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    NullLiteral,
    IdentifierName(String),
    New_Dot_Target,
    Super,
    This,
    Yield,
}
