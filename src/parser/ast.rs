use std::fmt::Debug;

#[derive(Debug)]
pub struct JsError {
    pub src: &'static str,
    pub message: String,
}

#[derive(Debug)]
pub struct Meta {
    pub start_index: usize,
    pub end_index: usize,
}

pub trait HasMeta {
    fn get_meta(&self) -> &Meta;
}

pub trait Expression: HasMeta + Debug {}
pub trait Pattern: HasMeta + Debug {}

#[derive(Debug)]
pub struct IdentifierData {
    pub name: String,
    pub meta: Meta,
}

#[derive(Debug)]
pub enum ExpressionPatternType {
    Identifier(IdentifierData),
    MemberExpression(MemberExpressionType),
}

impl Expression for ExpressionPatternType {}
impl Pattern for ExpressionPatternType {}
impl HasMeta for ExpressionPatternType {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionPatternType::Identifier(data) => &data.meta,
            ExpressionPatternType::MemberExpression(data) => data.get_meta(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionType {
    Literal(LiteralData),
    ThisExpression {
        meta: Meta,
    },
    ArrayExpression {
        meta: Meta,
        elements: Vec<Option<ExpressionOrSpreadElement>>,
    },
    ObjectExpression {
        meta: Meta,
        properties: Vec<PropertyData<Box<dyn Expression>>>,
    },
    FunctionExpression(FunctionData),
    UnaryExpression {
        meta: Meta,
        operator: UnaryOperator,
        argument: Box<dyn Expression>,
    },
    UpdateExpression {
        meta: Meta,
        operator: UpdateOperator,
        argument: Box<dyn Expression>,
        prefix: bool,
    },
    BinaryExpression {
        meta: Meta,
        operator: BinaryOperator,
        left: Box<dyn Expression>,
        right: Box<dyn Expression>,
    },
    AssignmentExpression {
        meta: Meta,
        operator: AssignmentOperator,
        left: PatternOrExpression,
        right: Box<dyn Expression>,
    },
    LogicalExpression {
        meta: Meta,
        operator: LogicalOperator,
        left: Box<dyn Expression>,
        right: Box<dyn Expression>,
    },
    ConditionalExpression {
        meta: Meta,
        test: Box<dyn Expression>,
        consequent: Box<dyn Expression>,
        alternate: Box<dyn Expression>,
    },
    CallExpression {
        //A function or method call expression.
        meta: Meta,
        callee: ExpressionOrSuper,
        arguments: Vec<ExpressionOrSpreadElement>,
    },
    NewExpression {
        meta: Meta,
        callee: Box<dyn Expression>,
        arguments: Vec<ExpressionOrSpreadElement>,
    },
    SequenceExpression {
        //A comma-separated sequence of expressions
        meta: Meta,
        expressions: Vec<Box<dyn Expression>>,
    },
    ArrowFunctionExpression {
        meta: Meta,
        body: FunctionBodyOrExpression,
        expression: bool,
    },
    YieldExpression {
        meta: Meta,
        argument: Option<Box<dyn Expression>>,
        delegate: bool,
    },
    TemplateLiteral(TemplateLiteralData),
    TaggedTemplateExpression {
        meta: Meta,
        tag: Box<dyn Expression>,
        quasi: TemplateLiteralData,
    },
    ClassExpression(ClassData),
    MetaProperty {
        //Represents new.target meta property in ES2015. In the future, it will represent other meta properties as well.
        meta: Meta,
        meta_object: IdentifierData,
        property: IdentifierData,
    },
}

impl Expression for ExpressionType {}
impl HasMeta for ExpressionType {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionType::Literal(data) => &data.meta,
            ExpressionType::ThisExpression { meta } => &meta,
            ExpressionType::ArrayExpression { meta, .. } => &meta,
            ExpressionType::ObjectExpression { meta, .. } => &meta,
            ExpressionType::FunctionExpression(data) => &data.meta,
            ExpressionType::UnaryExpression { meta, .. } => &meta,
            ExpressionType::UpdateExpression { meta, .. } => &meta,
            ExpressionType::BinaryExpression { meta, .. } => &meta,
            ExpressionType::AssignmentExpression { meta, .. } => &meta,
            ExpressionType::LogicalExpression { meta, .. } => &meta,
            ExpressionType::ConditionalExpression { meta, .. } => &meta,
            ExpressionType::CallExpression { meta, .. } => &meta,
            ExpressionType::NewExpression { meta, .. } => &meta,
            ExpressionType::SequenceExpression { meta, .. } => &meta,
            ExpressionType::ArrowFunctionExpression { meta, .. } => &meta,
            ExpressionType::YieldExpression { meta, .. } => &meta,
            ExpressionType::TemplateLiteral(data) => &data.meta,
            ExpressionType::TaggedTemplateExpression { meta, .. } => &meta,
            ExpressionType::ClassExpression(data) => &data.meta,
            ExpressionType::MetaProperty { meta, .. } => &meta,
        }
    }
}

#[derive(Debug)]
pub enum PatternType {
    ObjectPattern {
        meta: Meta,
        properties: Vec<AssignmentPropertyData>,
    },
    ArrayPattern {
        meta: Meta,
        elements: Vec<Option<Box<dyn Pattern>>>,
    },
    RestElement {
        meta: Meta,
        argument: Box<dyn Pattern>,
    },
    AssignmentPattern {
        meta: Meta,
        left: Box<dyn Pattern>,
        right: Box<dyn Expression>,
    },
}

impl Pattern for PatternType {}
impl HasMeta for PatternType {
    fn get_meta(&self) -> &Meta {
        match self {
            PatternType::ObjectPattern { meta, .. } => &meta,
            PatternType::ArrayPattern { meta, .. } => &meta,
            PatternType::RestElement { meta, .. } => &meta,
            PatternType::AssignmentPattern { meta, .. } => &meta,
        }
    }
}

#[derive(Debug)]
pub struct TemplateLiteralData {
    pub meta: Meta,
    pub quasis: Vec<TemplateElementData>,
    pub expressions: Vec<Box<dyn Expression>>,
}

impl HasMeta for TemplateLiteralData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct TemplateElementData {
    pub meta: Meta,
    pub tail: bool,
    pub cooked_value: String,
    pub raw_value: String,
}

impl HasMeta for TemplateElementData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub enum FunctionBodyOrExpression {
    FunctionBody(FunctionBodyData),
    Expression(Box<dyn Expression>),
}

#[derive(Debug)]
pub enum ExpressionOrSuper {
    Expression(Box<dyn Expression>),
    Super,
}

#[derive(Debug)]
pub enum MemberExpressionType {
    SimpleMemberExpression {
        meta: Meta,
        object: ExpressionOrSuper,
        property: IdentifierData,
    },
    ComputedMemberExpression {
        meta: Meta,
        object: ExpressionOrSuper,
        property: Box<dyn Expression>,
    },
}

impl Pattern for MemberExpressionType {}
impl HasMeta for MemberExpressionType {
    fn get_meta(&self) -> &Meta {
        match self {
            MemberExpressionType::SimpleMemberExpression { meta, .. } => &meta,
            MemberExpressionType::ComputedMemberExpression { meta, .. } => &meta,
        }
    }
}

#[derive(Debug)]
pub enum ExpressionOrSpreadElement {
    Expression(Box<dyn Expression>),
    SpreadElement(Box<dyn Expression>),
}

#[derive(Debug)]
pub struct MemberExpressionData {
    pub meta: Meta,
    pub object: Box<dyn Expression>,
    pub property: Box<dyn Expression>,
    pub computed: bool,
}

#[derive(Debug)]
pub enum PatternOrExpression {
    Pattern(Box<dyn Pattern>),
    Expression(Box<dyn Expression>),
}

#[derive(Debug)]
pub enum AssignmentOperator {
    Equals,
    AddEquals,
    SubtractEquals,
    MultiplyEquals,
    DivideEquals,
    ModuloEquals,
    BitwiseLeftShiftEquals,
    BitwiseRightShiftEquals,
    BitwiseUnsignedRightShiftEquals,
    BitwiseOrEquals,
    BitwiseAndEquals,
    BitwiseXorEquals,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
    Plus,
    LogicalNot,
    BitwiseNot,
    TypeOf,
    Void,
    Delete,
}

#[derive(Debug)]
pub enum UpdateOperator {
    PlusPlus,
    MinusMinus,
}

#[derive(Debug)]
pub enum BinaryOperator {
    LooselyEqual,
    LooselyUnequal,
    StrictlyEqual,
    StrictlyUnequal,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    BitwiseLeftShift,
    BitwiseRightShift,
    BitwiseUnsignedRightShift,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    In,
    InstanceOf,
}

#[derive(Debug)]
pub enum LogicalOperator {
    Or,
    And,
}

#[derive(Debug)]
pub struct LiteralData {
    pub meta: Meta,
    pub value: LiteralType,
}

#[derive(Debug)]
pub enum LiteralType {
    StringLiteral(String),
    BooleanLiteral(bool),
    NullLiteral,
    NumberLiteral(NumberLiteralType),
    RegExpLiteral(RegExpLiteralData),
}

#[derive(Debug)]
pub struct RegExpLiteralData {
    pub pattern: String,
    pub flags: String,
}

#[derive(Debug)]
pub enum NumberLiteralType {
    IntegerLiteral(i32),
    FloatLiteral(f64),
}

#[derive(Debug)]
pub struct ProgramData {
    pub meta: Meta,
    pub body: Vec<StatementType>,
}

impl HasMeta for ProgramData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct BlockStatementData {
    pub meta: Meta,
    pub body: Box<Vec<StatementType>>,
}

#[derive(Debug)]
pub enum StatementType {
    ExpressionStatement {
        meta: Meta,
        expression: Box<dyn Expression>,
    },
    BlockStatement(BlockStatementData),
    FunctionBody(FunctionBodyData),
    EmptyStatement {
        meta: Meta,
    },
    DebuggerStatement {
        meta: Meta,
    },
    ReturnStatement {
        meta: Meta,
        argument: Option<Box<dyn Expression>>,
    },
    //Label Statement not supported, hence break & continue with labels not supported
    BreakStatement {
        meta: Meta,
    },
    ContinueStatement {
        meta: Meta,
    },
    IfStatement {
        meta: Meta,
        test: Box<dyn Expression>,
        consequent: Box<StatementType>,
        alternate: Option<Box<StatementType>>,
    },
    SwitchStatement {
        meta: Meta,
        discriminant: Box<dyn Expression>,
        cases: Box<Vec<SwitchCaseData>>,
    },
    ThrowStatement {
        meta: Meta,
        argument: Box<dyn Expression>,
    },
    TryStatement {
        meta: Meta,
        block: BlockStatementData,
        handler: Option<CatchClauseData>,
        finalizer: Option<BlockStatementData>,
    },
    WhileStatement {
        meta: Meta,
        test: Box<dyn Expression>,
        body: Box<StatementType>,
    },
    DoWhileStatement {
        meta: Meta,
        test: Box<dyn Expression>,
        body: Box<StatementType>,
    },
    ForStatement {
        meta: Meta,
        init: Option<VariableDeclarationOrExpression>,
        test: Option<Box<dyn Expression>>,
        update: Option<Box<dyn Expression>>,
        body: Box<StatementType>,
    },
    ForInStatement(ForIteratorData),
    ForOfStatement(ForIteratorData),
    DeclarationStatement(DeclarationType),
}

impl HasMeta for StatementType {
    fn get_meta(&self) -> &Meta {
        match self {
            StatementType::ExpressionStatement { meta, .. } => &meta,
            StatementType::BlockStatement(data) => &data.meta,
            StatementType::FunctionBody(data) => data.get_meta(),
            StatementType::EmptyStatement { meta, .. } => &meta,
            StatementType::DebuggerStatement { meta, .. } => &meta,
            StatementType::ReturnStatement { meta, .. } => &meta,
            StatementType::BreakStatement { meta, .. } => &meta,
            StatementType::ContinueStatement { meta, .. } => &meta,
            StatementType::IfStatement { meta, .. } => &meta,
            StatementType::SwitchStatement { meta, .. } => &meta,
            StatementType::ThrowStatement { meta, .. } => &meta,
            StatementType::TryStatement { meta, .. } => &meta,
            StatementType::WhileStatement { meta, .. } => &meta,
            StatementType::DoWhileStatement { meta, .. } => &meta,
            StatementType::ForStatement { meta, .. } => &meta,
            StatementType::ForInStatement(data) => data.get_meta(),
            StatementType::ForOfStatement(data) => data.get_meta(),
            StatementType::DeclarationStatement(data) => data.get_meta(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionBodyData {
    pub meta: Meta,
    pub body: Box<Vec<StatementType>>, /*Actual code was -- Box<Vec<DirectiveOrStatement>> -- We do not support directives, like 'use strict'*/
}

impl HasMeta for FunctionBodyData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct ForIteratorData {
    meta: Meta,
    left: VariableDeclarationOrPattern,
    right: Box<dyn Expression>,
    body: Box<StatementType>,
}

impl HasMeta for ForIteratorData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub enum VariableDeclarationOrExpression {
    VariableDeclaration(VariableDeclarationData),
    Expression(Box<dyn Expression>),
}

#[derive(Debug)]
pub enum VariableDeclarationOrPattern {
    VariableDeclaration(VariableDeclarationData),
    Pattern(Box<dyn Pattern>),
}

#[derive(Debug)]
pub enum DeclarationType {
    FunctionDeclaration(FunctionData), //id is mandatory here
    VariableDeclaration(VariableDeclarationData),
    ClassDeclaration(ClassData),
}

impl HasMeta for DeclarationType {
    fn get_meta(&self) -> &Meta {
        match self {
            DeclarationType::FunctionDeclaration(data) => data.get_meta(),
            DeclarationType::VariableDeclaration(data) => data.get_meta(),
            DeclarationType::ClassDeclaration(data) => data.get_meta(),
        }
    }
}

#[derive(Debug)]
pub struct VariableDeclarationData {
    pub meta: Meta,
    pub declarations: Vec<VariableDeclaratorData>,
    pub kind: VariableDeclarationKind,
}

#[derive(Debug)]
pub enum VariableDeclarationKind {
    Var,
    Let,
    Const,
}

#[derive(Debug)]
pub struct SwitchCaseData {
    pub meta: Meta,
    pub test: Option<Box<dyn Expression>>,
    pub consequent: Box<Vec<StatementType>>,
}

impl HasMeta for SwitchCaseData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct FunctionData {
    pub meta: Meta,
    pub id: Option<IdentifierData>,
    pub params: Vec<Box<dyn Pattern>>,
    pub body: FunctionBodyData,
    pub generator: bool,
}

impl HasMeta for FunctionData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct CatchClauseData {
    pub meta: Meta,
    pub param: Box<dyn Pattern>,
    pub body: BlockStatementData,
}

impl HasMeta for CatchClauseData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct VariableDeclaratorData {
    pub meta: Meta,
    pub id: Box<dyn Pattern>,
    pub init: Option<Box<dyn Expression>>,
}

impl HasMeta for VariableDeclarationData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct PropertyData<V> {
    meta: Meta,
    key: LiteralOrIdentifier,
    value: V,
    kind: PropertyKind,
    method: bool,
    shorthand: bool,
    computed: bool,
}

impl HasMeta for PropertyData<Box<dyn Expression>> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

impl HasMeta for PropertyData<Box<dyn Pattern>> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub enum LiteralOrIdentifier {
    Literal(LiteralData),
    Identifier(IdentifierData),
}

#[derive(Debug)]
pub struct AssignmentPropertyData(PropertyData<Box<dyn Pattern>>);

impl AssignmentPropertyData {
    fn new(
        meta: Meta,
        key: LiteralOrIdentifier,
        value: Box<dyn Pattern>,
        shorthand: bool,
        computed: bool,
    ) -> Self {
        AssignmentPropertyData(PropertyData {
            meta,
            key,
            value,
            shorthand,
            computed,
            method: false,
            kind: PropertyKind::Init,
        })
    }

    // fn new_from_variable_declarator_data(data: VariableDeclaratorData) -> Self {
    //     if let ExpressionPatternType::Identifier(id) = *data.id {
    //         Self::new(
    //             data.meta,
    //             LiteralOrIdentifier::Identifier(id),
    //             data.init.unwrap(),
    //             false,
    //             false,
    //         )
    //     } else {
    //         panic!("Method called with unexpected data");
    //     }
    // }
}

impl HasMeta for AssignmentPropertyData {
    fn get_meta(&self) -> &Meta {
        &self.0.meta
    }
}

#[derive(Debug)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug)]
pub struct ClassData {
    pub meta: Meta,
    pub id: Option<IdentifierData>,
    pub super_class: Option<Box<dyn Expression>>,
    pub body: ClassBodyData,
}

impl HasMeta for ClassData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct ClassBodyData {
    pub meta: Meta,
    pub body: Vec<MethodDefinitionData>,
}

impl HasMeta for ClassBodyData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub struct MethodDefinitionData {
    pub meta: Meta,
    pub key: Box<dyn Expression>,
    pub value: FunctionData,
    pub kind: MethodDefinitionKind,
    pub computed: bool,
    pub static_flag: bool,
}

impl HasMeta for MethodDefinitionData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }
}

#[derive(Debug)]
pub enum MethodDefinitionKind {
    Constructor,
    Method,
    Get,
    Set,
}
