use std::rc::{Rc, Weak};

#[derive(Debug)]
pub enum EqualityOp {
    StrictEqual(Box<Node>),
    StrictUnequal(Box<Node>),
    SoftEqual(Box<Node>),
    SoftUnequal(Box<Node>),
}

#[derive(Debug)]
pub enum RelationalOp {
    LessThan(Box<Node>),
    LessThanOrEqual(Box<Node>),
    GreaterThan(Box<Node>),
    GreaterThanOrEqual(Box<Node>),
    InstanceOf(Box<Node>),
    In(Box<Node>),
}

#[derive(Debug)]
pub enum ShiftOp {
    LeftShift(Box<Node>),
    RightShift(Box<Node>),
    UnsignedRightShift(Box<Node>),
}

#[derive(Debug)]
pub enum AdditiveOp {
    Add(Box<Node>),
    Subtract(Box<Node>),
}

#[derive(Debug)]
pub enum MultiplicativeOp {
    Multiply(Box<Node>),
    Divide(Box<Node>),
    Modulo(Box<Node>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum PostfixOp {
    PlusPlus,
    MinusMinus,
}

#[derive(Debug)]
pub enum ArgumentType {
    Regular(Box<Node>),
    Rest(Box<Node>),
}

#[derive(Debug)]
pub enum FunctionArgumentType {
    Regular {
        identifier: Data,
        default: Option<Box<Node>>,
    },
    Rest(Data),
}

#[derive(Debug)]
pub enum ArrayArgumentType {
    Regular(Box<Node>),
    Spread(Box<Node>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ArrayBindingIdentifier {
    pub identifier: Data,
    pub index: u32,
    pub is_rest: bool,
}

#[derive(Debug)]
pub struct ObjectBindingIdentifier {
    pub identifier: Data,
    pub path: String,
}

#[derive(Debug)]
pub enum BindingType {
    SimpleBinding {
        identifier: Data,
        initializer: Option<Box<Node>>,
    },
    ArrayBinding {
        identifiers: Vec<ArrayBindingIdentifier>,
        initialize: Box<Node>,
    },
    ObjectBinding {
        identifiers: Vec<ObjectBindingIdentifier>,
        initialize: Box<Node>,
    },
}

#[derive(Debug)]
pub enum DeclarationType {
    ClassDeclaration,
    VarDeclaration(Vec<BindingType>),
    LetDeclaration(Vec<BindingType>),
    ConstDeclaration(Vec<BindingType>),
    GeneratorDeclaration {
        name: String,
        arguments: Vec<FunctionArgumentType>,
        body: Box<Node>,
    },
    FunctionDeclaration {
        name: String,
        arguments: Vec<FunctionArgumentType>,
        body: Box<Node>,
    },
}

#[derive(Debug)]
pub enum Node {
    Terminal(Data),
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
    Statements {
        all_hoisted_declarations: Vec<Weak<Node>>,
        statements: Vec<Rc<Node>>,
    },
    Declaration(DeclarationType),
}

#[derive(Debug)]
pub enum Data {
    IntegerLiteral(i32),
    FloatLiteral(f64),
    StringLiteral(String),
    BooleanLiteral(bool),
    NullLiteral,
    IdentifierName(String),
    NewDotTarget,
    Super,
    This,
    Yield,
}
