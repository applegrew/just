pub struct JsError {
    pub message: String,
}

#[derive(Debug)]
pub struct Script {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug)]
pub enum Statement {
    VariableStatement(Vec<BindingType>),
}

#[derive(Debug)]
pub enum Declaration {
    HoistableDeclaration,
    LexicalDeclaration,
}

#[derive(Debug)]
pub enum BindingType {
    SimpleBinding {
        identifier: String,
        initializer: Option<AssignmentExpression>,
    },
    ArrayBinding {
        identifiers: Vec<ArrayBindingIdentifier>,
        initialize: AssignmentExpression,
    },
    ObjectBinding {
        identifiers: Vec<ObjectBindingIdentifier>,
        initialize: AssignmentExpression,
    },
}

#[derive(Debug)]
pub struct ArrayBindingIdentifier {
    pub identifier: String,
    pub index: u32,
    pub is_rest: bool,
}

#[derive(Debug)]
pub struct ObjectBindingIdentifier {
    pub identifier: String,
    pub path: String,
}

#[derive(Debug)]
pub enum AssignmentExpression {
    ArrowFunctionExpression,
    LhsRhsAssignmentExpression(LhsRhsAssignmentExpressionType),
    YieldExpression {
        is_star: bool,
        assignment_expression: Option<Box<AssignmentExpression>>,
    },
    ConditionalExpression(LogicAndMathExpression),
}

#[derive(Debug)]
pub enum LogicAndMathExpression {
    ConditionOperatorExpression {
        condition: Box<LogicAndMathExpression>,
        if_true: Box<AssignmentExpression>,
        if_false: Box<AssignmentExpression>,
    },
    LogicalOrExpression(Box<Vec<LogicAndMathExpression>>),
    LogicalAndExpression(Box<Vec<LogicAndMathExpression>>),
    BitwiseOrExpression(Box<Vec<LogicAndMathExpression>>),
    BitwiseXorExpression(Box<Vec<LogicAndMathExpression>>),
    BitwiseAndExpression(Box<Vec<LogicAndMathExpression>>),
    LogicalEqualityExpression {
        operands: Box<Vec<LogicAndMathExpression>>,
        operators: Vec<LogicalEqualityOperator>,
    },
    RelationalExpression {
        operands: Box<Vec<LogicAndMathExpression>>,
        operators: Vec<RelationalOperator>,
    },
    ShiftExpression {
        operands: Box<Vec<LogicAndMathExpression>>,
        operators: Vec<ShiftOperator>,
    },
    AdditiveExpression {
        operands: Box<Vec<LogicAndMathExpression>>,
        operators: Vec<AdditiveOperator>,
    },
    MultiplicativeExpression {
        operands: Box<Vec<LogicAndMathExpression>>,
        operators: Vec<MultiplicativeOperator>,
    },
    UnaryExpression {
        operand: Box<LogicAndMathExpression>,
        operator: UnaryOperator,
    },
    PostfixExpression {
        operand: Box<LogicAndMathExpression>,
        operator: PostfixOperator,
    },
    LhsExpressionWrapper(LhsExpressionType),
}

#[derive(Debug)]
pub enum LogicalEqualityOperator {
    StrictlyEqual,
    StrictlyUnequal,
    LooselyEqual,
    LooselyUnequal,
}

#[derive(Debug)]
pub enum RelationalOperator {
    LessThanEqual,
    GreaterThanEqual,
    LessThan,
    GreaterThan,
    InstanceOf,
    In,
}

#[derive(Debug)]
pub enum ShiftOperator {
    LeftShift,
    RightShift,
    UnsignedRightShift,
}

#[derive(Debug)]
pub enum AdditiveOperator {
    Add,
    Subtract,
}

#[derive(Debug)]
pub enum MultiplicativeOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Delete,
    Void,
    Typeof,
    PrefixIncrement,
    PrefixDecrement,
    Plus,
    Minus,
    BitwiseNot,
    LogicalNot,
}

#[derive(Debug)]
pub enum PostfixOperator {
    PostfixIncrement,
    PostfixDecrement,
}

#[derive(Debug)]
pub struct Expression {
    pub assignment_expressions: Vec<AssignmentExpression>,
}

#[derive(Debug)]
pub struct LhsRhsAssignmentExpressionType {
    pub lhs_expression: LhsExpressionType,
    pub assignment_operator: AssignmentOperator,
    pub rhs_expression: Box<AssignmentExpression>,
}

#[derive(Debug)]
pub enum LhsExpressionType {
    CallExpression(CallExpressionType),
    NewExpression(NewExpressionType),
}

#[derive(Debug)]
pub enum CallExpressionType {
    FunctionCallExpression(FunctionCallExpressionType),
    SimplePropertyAccessExpression(SimplePropertyAccessExpressionData<CallExpressionType>),
    ComplexPropertyAccessExpression(ComplexPropertyAccessExpressionData<CallExpressionType>),
    TaggedTemplateLiteral,
}

#[derive(Debug)]
pub struct SimplePropertyAccessExpressionData<E> {
    pub object: Box<E>,
    pub property_name: String,
}

#[derive(Debug)]
pub struct ComplexPropertyAccessExpressionData<E> {
    pub object: Box<E>,
    pub property_expression: Expression,
}

#[derive(Debug)]
pub enum FunctionCallExpressionType {
    CallOnMemberExpression {
        function_obj: Box<MemberExpression>,
        arguments: ArgumentList,
    },
    CallOnSuper {
        arguments: ArgumentList,
    },
    CallOnCallExpressionType {
        function_obj: Box<CallExpressionType>,
        arguments: ArgumentList,
    },
}

#[derive(Debug)]
pub enum AssignmentOperator {
    Equal,
    MultiplyEqual,
    DivideEqual,
    ModuloEqual,
    AddEqual,
    SubtractEqual,
    LeftShiftEqual,
    RightShiftEqual,
    TripleRightShiftEqual,
    BitwiseAndEqual,
    BitwiseOrEqual,
    BitwiseXorEqual,
}

#[derive(Debug)]
pub struct ArgumentList {
    pub arguments: Vec<Argument>,
}

#[derive(Debug)]
pub enum Argument {
    Regular(AssignmentExpression),
    Spread(AssignmentExpression),
}

#[derive(Debug)]
pub enum NewExpressionType {
    Member(MemberExpression),
    New(Box<NewExpressionType>),
}

#[derive(Debug)]
pub enum MemberExpression {
    MemberSimplePropertyAccessExpression(SimplePropertyAccessExpressionData<MemberExpression>),
    MemberComplexPropertyAccessExpression(ComplexPropertyAccessExpressionData<MemberExpression>),
    SuperSimplePropertyAccessExpression {
        property_name: String,
    },
    SuperComplexPropertyAccessExpression {
        property_expression: Expression,
    },
    NewDotTargetExpression,
    PrimaryExpression(PrimaryExpressionType),
    MemberTaggedTemplateLiteral,
    NewObjectFromFunction {
        function_obj: Box<MemberExpression>,
        arguments: ArgumentList,
    },
}

#[derive(Debug)]
pub enum PrimaryExpressionType {
    IdentifierReference(String),
    Literal(LiteralType),
    YieldKeyword,
    ThisKeyword,
    ArrayLiteral { items: Vec<ArrayItem>, length: u16 },
    ObjectLiteral,
    FunctionDefinition,
}

#[derive(Debug)]
pub enum LiteralType {
    NullLiteral,
    BoolLiteral(bool),
    NumericLiteral(NumericType),
    StringLiteral(String),
}

#[derive(Debug)]
pub enum NumericType {
    IntegerLiteral(i32),
    FloatLiteral(f64),
}

#[derive(Debug)]
pub struct ArrayItem {
    pub index: u16,
    pub item: Argument,
}
