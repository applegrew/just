use crate::parser::util::{format_has_meta_option, format_option, format_struct, format_vec};
use pest::error::Error;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug)]
pub struct JsError<Rule> {
    pub kind: JsErrorType<Rule>,
    pub message: String,
}

#[derive(Debug)]
pub enum JsErrorType<R> {
    Unexpected(&'static str),
    ParserValidation(Error<R>),
    AstBuilderValidation(Meta),
}

#[derive(Debug)]
pub struct Meta {
    pub start_index: usize,
    pub end_index: usize,
    pub script: Rc<String>,
}
impl Meta {
    pub fn to_formatted_string(&self, script: &str) -> String {
        format!(
            "Meta {{ \"{}\" }}",
            &script[self.start_index..self.end_index].replace("\n", "░")
        )
    }

    pub fn to_formatted_code(&self) -> String {
        if self.script.len() <= self.end_index {
            self.script[self.start_index..self.end_index].to_string()
        } else {
            "[unknown]".to_string()
        }
    }
}
impl Clone for Meta {
    fn clone(&self) -> Self {
        Meta {
            start_index: self.start_index,
            end_index: self.end_index,
            script: self.script.clone(),
        }
    }
}

pub trait HasMeta {
    fn get_meta(&self) -> &Meta;
    fn to_formatted_string(&self, script: &str) -> String;
}

#[derive(Debug)]
pub struct IdentifierData {
    pub name: String,
    pub meta: Meta,
}

impl HasMeta for IdentifierData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("IdentifierData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("name", self.name.to_string())
            .to_string()
    }
}

impl PartialEq for IdentifierData {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for IdentifierData {}

impl Hash for IdentifierData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

#[derive(Debug)]
pub enum ExpressionPatternType {
    Identifier(IdentifierData),
    MemberExpression(MemberExpressionType),
}

impl ExpressionPatternType {
    pub fn convert_to_pattern(self) -> PatternType {
        PatternType::PatternWhichCanBeExpression(self)
    }

    pub fn convert_to_expression(self) -> ExpressionType {
        ExpressionType::ExpressionWhichCanBePattern(self)
    }
}

impl HasMeta for ExpressionPatternType {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionPatternType::Identifier(data) => &data.meta,
            ExpressionPatternType::MemberExpression(data) => data.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            ExpressionPatternType::Identifier(data) => {
                format!(
                    "ExpressionPatternType::Identifier({})",
                    data.to_formatted_string(script)
                )
            }
            ExpressionPatternType::MemberExpression(data) => format!(
                "ExpressionPatternType::MemberExpression({})",
                data.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionType {
    ExpressionWhichCanBePattern(ExpressionPatternType),
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
        properties: Vec<PropertyData<Box<ExpressionType>>>,
    },
    FunctionExpression(FunctionData),
    UnaryExpression {
        meta: Meta,
        operator: UnaryOperator,
        argument: Box<ExpressionType>,
    },
    UpdateExpression {
        meta: Meta,
        operator: UpdateOperator,
        argument: Box<ExpressionType>,
        prefix: bool,
    },
    BinaryExpression {
        meta: Meta,
        operator: BinaryOperator,
        left: Box<ExpressionType>,
        right: Box<ExpressionType>,
    },
    AssignmentExpression {
        meta: Meta,
        operator: AssignmentOperator,
        left: PatternOrExpression,
        right: Box<ExpressionType>,
    },
    LogicalExpression {
        meta: Meta,
        operator: LogicalOperator,
        left: Box<ExpressionType>,
        right: Box<ExpressionType>,
    },
    ConditionalExpression {
        meta: Meta,
        test: Box<ExpressionType>,
        consequent: Box<ExpressionType>,
        alternate: Box<ExpressionType>,
    },
    CallExpression {
        //A function or method call expression.
        meta: Meta,
        callee: ExpressionOrSuper,
        arguments: Vec<ExpressionOrSpreadElement>,
    },
    NewExpression {
        meta: Meta,
        callee: Box<ExpressionType>,
        arguments: Vec<ExpressionOrSpreadElement>,
    },
    SequenceExpression {
        //A comma-separated sequence of expressions
        meta: Meta,
        expressions: Vec<Box<ExpressionType>>,
    },
    ArrowFunctionExpression {
        meta: Meta,
        body: FunctionBodyOrExpression,
        expression: bool,
    },
    YieldExpression {
        meta: Meta,
        argument: Option<Box<ExpressionType>>,
        delegate: bool,
    },
    TemplateLiteral(TemplateLiteralData),
    TaggedTemplateExpression {
        meta: Meta,
        tag: Box<ExpressionType>,
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
            ExpressionType::ExpressionWhichCanBePattern(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            ExpressionType::Literal(data) => format!(
                "ExpressionType::Literal({})",
                data.to_formatted_string(script)
            ),
            ExpressionType::ThisExpression { meta } => {
                format_struct("ExpressionType::ThisExpression")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .to_string()
            }
            ExpressionType::ArrayExpression { meta, elements } => {
                format_struct("ExpressionType::ArrayExpression")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields(
                        "elements",
                        format_vec(elements, |p| format_has_meta_option(&p, script)),
                    )
                    .to_string()
            }
            ExpressionType::ObjectExpression { meta, properties } => {
                format_struct("ExpressionType::ObjectExpression")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields(
                        "properties",
                        format_vec(properties, |p| p.to_formatted_string(script)),
                    )
                    .to_string()
            }
            ExpressionType::FunctionExpression(data) => format!(
                "ExpressionType::FunctionExpression({})",
                data.to_formatted_string(script)
            ),
            ExpressionType::UnaryExpression {
                meta,
                operator,
                argument,
            } => format_struct("ExpressionType::UnaryExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("argument", argument.to_formatted_string(script))
                .to_string(),
            ExpressionType::UpdateExpression {
                meta,
                operator,
                argument,
                prefix,
            } => format_struct("ExpressionType::UpdateExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("argument", argument.to_formatted_string(script))
                .add_fields("prefix", format!("{}", prefix))
                .to_string(),
            ExpressionType::BinaryExpression {
                meta,
                operator,
                left,
                right,
            } => format_struct("ExpressionType::BinaryExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("left", left.to_formatted_string(script))
                .add_fields("right", right.to_formatted_string(script))
                .to_string(),
            ExpressionType::AssignmentExpression {
                meta,
                operator,
                left,
                right,
            } => format_struct("ExpressionType::AssignmentExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("left", left.to_formatted_string(script))
                .add_fields("right", right.to_formatted_string(script))
                .to_string(),
            ExpressionType::LogicalExpression {
                meta,
                operator,
                left,
                right,
            } => format_struct("ExpressionType::LogicalExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("left", left.to_formatted_string(script))
                .add_fields("right", right.to_formatted_string(script))
                .to_string(),
            ExpressionType::ConditionalExpression {
                meta,
                test,
                consequent,
                alternate,
            } => format_struct("ExpressionType::ConditionalExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("test", test.to_formatted_string(script))
                .add_fields("consequent", consequent.to_formatted_string(script))
                .add_fields("alternate", alternate.to_formatted_string(script))
                .to_string(),
            ExpressionType::CallExpression {
                meta,
                callee,
                arguments,
            } => format_struct("ExpressionType::CallExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("callee", callee.to_formatted_string(script))
                .add_fields(
                    "arguments",
                    format_vec(arguments, |p| p.to_formatted_string(script)),
                )
                .to_string(),
            ExpressionType::NewExpression {
                meta,
                callee,
                arguments,
            } => format_struct("ExpressionType::NewExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("callee", callee.to_formatted_string(script))
                .add_fields(
                    "arguments",
                    format_vec(arguments, |p| p.to_formatted_string(script)),
                )
                .to_string(),
            ExpressionType::SequenceExpression { meta, expressions } => {
                format_struct("ExpressionType::SequenceExpression")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields(
                        "expressions",
                        format_vec(expressions, |p| p.to_formatted_string(script)),
                    )
                    .to_string()
            }
            ExpressionType::ArrowFunctionExpression {
                meta,
                body,
                expression,
            } => format_struct("ExpressionType::ArrowFunctionExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("body", body.to_formatted_string(script))
                .add_fields("expression", expression.to_string())
                .to_string(),
            ExpressionType::YieldExpression {
                meta,
                argument,
                delegate,
            } => format_struct("ExpressionType::YieldExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields(
                    "argument",
                    format_option(argument, |o| o.to_formatted_string(script)),
                )
                .add_fields("delegate", delegate.to_string())
                .to_string(),
            ExpressionType::TemplateLiteral(data) => format!(
                "ExpressionType::TemplateLiteral({})",
                data.to_formatted_string(script)
            ),
            ExpressionType::TaggedTemplateExpression { meta, tag, quasi } => {
                format_struct("ExpressionType::TaggedTemplateExpression")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("tag", tag.to_formatted_string(script))
                    .add_fields("quasi", quasi.to_formatted_string(script))
                    .to_string()
            }
            ExpressionType::ClassExpression(data) => format!(
                "ExpressionType::ClassExpression({})",
                data.to_formatted_string(script)
            ),
            ExpressionType::MetaProperty {
                meta,
                meta_object,
                property,
            } => format_struct("ExpressionType::TaggedTemplateExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("meta_object", meta_object.to_formatted_string(script))
                .add_fields("property", property.to_formatted_string(script))
                .to_string(),
            ExpressionType::ExpressionWhichCanBePattern(d) => format!(
                "ExpressionType::ExpressionWhichCanBePattern({})",
                d.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub enum PatternType {
    PatternWhichCanBeExpression(ExpressionPatternType),
    ObjectPattern {
        meta: Meta,
        properties: Vec<AssignmentPropertyData>,
    },
    ArrayPattern {
        meta: Meta,
        elements: Vec<Option<Box<PatternType>>>,
    },
    RestElement {
        meta: Meta,
        argument: Box<PatternType>,
    },
    AssignmentPattern {
        meta: Meta,
        left: Box<PatternType>,
        right: Box<ExpressionType>,
    },
}

impl HasMeta for PatternType {
    fn get_meta(&self) -> &Meta {
        match self {
            PatternType::ObjectPattern { meta, .. } => &meta,
            PatternType::ArrayPattern { meta, .. } => &meta,
            PatternType::RestElement { meta, .. } => &meta,
            PatternType::AssignmentPattern { meta, .. } => &meta,
            PatternType::PatternWhichCanBeExpression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            PatternType::ObjectPattern { meta, properties } => {
                format_struct("PatternType::ObjectPattern")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields(
                        "properties",
                        format_vec(properties, |p| p.to_formatted_string(script)),
                    )
                    .to_string()
            }
            PatternType::ArrayPattern { meta, elements } => {
                format_struct("PatternType::ArrayPattern")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields(
                        "elements",
                        format_vec(elements, |p| {
                            format_option(&p, |o| o.to_formatted_string(script))
                        }),
                    )
                    .to_string()
            }
            PatternType::RestElement { meta, argument } => {
                format_struct("PatternType::RestElement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("argument", argument.to_formatted_string(script))
                    .to_string()
            }
            PatternType::AssignmentPattern { meta, left, right } => {
                format_struct("ExpressionType::AssignmentPattern")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("left", left.to_formatted_string(script))
                    .add_fields("right", right.to_formatted_string(script))
                    .to_string()
            }
            PatternType::PatternWhichCanBeExpression(d) => format!(
                "PatternType::PatternWhichCanBeExpression({})",
                d.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub struct TemplateLiteralData {
    pub meta: Meta,
    pub quasis: Vec<TemplateElementData>,
    pub expressions: Vec<Box<ExpressionType>>,
}

impl HasMeta for TemplateLiteralData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("TemplateLiteralData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "quasis",
                format_vec(&self.quasis, |p| p.to_formatted_string(script)),
            )
            .add_fields(
                "expressions",
                format_vec(&self.expressions, |p| p.to_formatted_string(script)),
            )
            .to_string()
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

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("TemplateElementData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("tail", self.tail.to_string())
            .add_fields("cooked_value", self.cooked_value.to_string())
            .add_fields("raw_value", self.raw_value.to_string())
            .to_string()
    }
}

#[derive(Debug)]
pub enum FunctionBodyOrExpression {
    FunctionBody(FunctionBodyData),
    Expression(Box<ExpressionType>),
}

impl HasMeta for FunctionBodyOrExpression {
    fn get_meta(&self) -> &Meta {
        match self {
            FunctionBodyOrExpression::FunctionBody(d) => d.get_meta(),
            FunctionBodyOrExpression::Expression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            FunctionBodyOrExpression::FunctionBody(d) => format!(
                "FunctionBodyOrExpression::FunctionBody({})",
                d.to_formatted_string(script)
            ),
            FunctionBodyOrExpression::Expression(d) => format!(
                "FunctionBodyOrExpression::Expression({})",
                d.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionOrSuper {
    Expression(Box<ExpressionType>),
    Super,
}

impl HasMeta for ExpressionOrSuper {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionOrSuper::Expression(d) => d.get_meta(),
            ExpressionOrSuper::Super => panic!("Tried to get meta of ExpressionOrSuper::Super"),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            ExpressionOrSuper::Expression(d) => format!(
                "ExpressionOrSuper::Expression({})",
                d.to_formatted_string(script)
            ),
            ExpressionOrSuper::Super => format!("ExpressionOrSuper::Super"),
        }
    }
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
        property: Box<ExpressionType>,
    },
}

impl HasMeta for MemberExpressionType {
    fn get_meta(&self) -> &Meta {
        match self {
            MemberExpressionType::SimpleMemberExpression { meta, .. } => &meta,
            MemberExpressionType::ComputedMemberExpression { meta, .. } => &meta,
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            MemberExpressionType::SimpleMemberExpression {
                meta,
                object,
                property,
            } => format_struct("MemberExpressionType::SimpleMemberExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("object", object.to_formatted_string(script))
                .add_fields("property", property.to_formatted_string(script))
                .to_string(),
            MemberExpressionType::ComputedMemberExpression {
                meta,
                object,
                property,
            } => format_struct("MemberExpressionType::ComputedMemberExpression")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("object", object.to_formatted_string(script))
                .add_fields("property", property.to_formatted_string(script))
                .to_string(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionOrSpreadElement {
    Expression(Box<ExpressionType>),
    SpreadElement(Box<ExpressionType>),
}

impl HasMeta for ExpressionOrSpreadElement {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionOrSpreadElement::Expression(data) => data.get_meta(),
            ExpressionOrSpreadElement::SpreadElement(data) => data.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            ExpressionOrSpreadElement::Expression(data) => format!(
                "ExpressionOrSpreadElement::Expression({})",
                data.to_formatted_string(script)
            ),
            ExpressionOrSpreadElement::SpreadElement(data) => format!(
                "ExpressionOrSpreadElement::SpreadElement({})",
                data.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub struct MemberExpressionData {
    pub meta: Meta,
    pub object: Box<ExpressionType>,
    pub property: Box<ExpressionType>,
    pub computed: bool,
}

#[derive(Debug)]
pub enum PatternOrExpression {
    Pattern(Box<PatternType>),
    Expression(Box<ExpressionType>),
}

impl HasMeta for PatternOrExpression {
    fn get_meta(&self) -> &Meta {
        match self {
            PatternOrExpression::Pattern(d) => d.get_meta(),
            PatternOrExpression::Expression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            PatternOrExpression::Pattern(d) => d.to_formatted_string(script),
            PatternOrExpression::Expression(d) => d.to_formatted_string(script),
        }
    }
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

impl HasMeta for LiteralData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("LiteralData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("value", format!("{:?}", self.value))
            .to_string()
    }
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

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("ProgramData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "body",
                format_vec(&self.body, |p| p.to_formatted_string(script)),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct BlockStatementData {
    pub meta: Meta,
    pub body: Box<Vec<StatementType>>,
}

impl HasMeta for BlockStatementData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("BlockStatementData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "body",
                format_vec(&self.body, |p| p.to_formatted_string(script)),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub enum StatementType {
    ExpressionStatement {
        meta: Meta,
        expression: Box<ExpressionType>,
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
        argument: Option<Box<ExpressionType>>,
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
        test: Box<ExpressionType>,
        consequent: Box<StatementType>,
        alternate: Option<Box<StatementType>>,
    },
    SwitchStatement {
        meta: Meta,
        discriminant: Box<ExpressionType>,
        cases: Box<Vec<SwitchCaseData>>,
    },
    ThrowStatement {
        meta: Meta,
        argument: Box<ExpressionType>,
    },
    TryStatement {
        meta: Meta,
        block: BlockStatementData,
        handler: Option<CatchClauseData>,
        finalizer: Option<BlockStatementData>,
    },
    WhileStatement {
        meta: Meta,
        test: Box<ExpressionType>,
        body: Box<StatementType>,
    },
    DoWhileStatement {
        meta: Meta,
        test: Box<ExpressionType>,
        body: Box<StatementType>,
    },
    ForStatement {
        meta: Meta,
        init: Option<VariableDeclarationOrExpression>,
        test: Option<Box<ExpressionType>>,
        update: Option<Box<ExpressionType>>,
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

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            StatementType::ExpressionStatement { meta, expression } => {
                format_struct("StatementType::ExpressionStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("expression", expression.to_formatted_string(script))
                    .to_string()
            }
            StatementType::BlockStatement(data) => format!(
                "StatementType::BlockStatement({})",
                data.to_formatted_string(script)
            ),
            StatementType::FunctionBody(data) => format!(
                "StatementType::FunctionBody({})",
                data.to_formatted_string(script)
            ),
            StatementType::EmptyStatement { meta } => {
                format_struct("StatementType::EmptyStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .to_string()
            }
            StatementType::DebuggerStatement { meta } => {
                format_struct("StatementType::DebuggerStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .to_string()
            }
            StatementType::ReturnStatement { meta, argument } => {
                format_struct("StatementType::ReturnStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields(
                        "argument",
                        format_option(argument, |o| o.to_formatted_string(script)),
                    )
                    .to_string()
            }
            StatementType::BreakStatement { meta } => {
                format_struct("StatementType::BreakStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .to_string()
            }
            StatementType::ContinueStatement { meta } => {
                format_struct("StatementType::ContinueStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .to_string()
            }
            StatementType::IfStatement {
                meta,
                test,
                consequent,
                alternate,
            } => format_struct("StatementType::IfStatement")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("test", test.to_formatted_string(script))
                .add_fields("consequent", consequent.to_formatted_string(script))
                .add_fields(
                    "alternate",
                    format_option(alternate, |o| o.to_formatted_string(script)),
                )
                .to_string(),
            StatementType::SwitchStatement {
                meta,
                discriminant,
                cases,
            } => format_struct("StatementType::SwitchStatement")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("discriminant", discriminant.to_formatted_string(script))
                .add_fields(
                    "cases",
                    format_vec(cases, |p| p.to_formatted_string(script)),
                )
                .to_string(),
            StatementType::ThrowStatement { meta, argument } => {
                format_struct("StatementType::ThrowStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("argument", argument.to_formatted_string(script))
                    .to_string()
            }
            StatementType::TryStatement {
                meta,
                block,
                handler,
                finalizer,
            } => format_struct("StatementType::TryStatement")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields("block", block.to_formatted_string(script))
                .add_fields(
                    "handler",
                    format_option(handler, |o| o.to_formatted_string(script)),
                )
                .add_fields(
                    "finalizer",
                    format_option(finalizer, |o| o.to_formatted_string(script)),
                )
                .to_string(),
            StatementType::WhileStatement { meta, test, body } => {
                format_struct("StatementType::WhileStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("test", test.to_formatted_string(script))
                    .add_fields("body", body.to_formatted_string(script))
                    .to_string()
            }
            StatementType::DoWhileStatement { meta, test, body } => {
                format_struct("StatementType::DoWhileStatement")
                    .add_fields("meta", meta.to_formatted_string(script))
                    .add_fields("test", test.to_formatted_string(script))
                    .add_fields("body", body.to_formatted_string(script))
                    .to_string()
            }
            StatementType::ForStatement {
                meta,
                init,
                test,
                update,
                body,
            } => format_struct("StatementType::ForStatement")
                .add_fields("meta", meta.to_formatted_string(script))
                .add_fields(
                    "init",
                    format_option(init, |o| o.to_formatted_string(script)),
                )
                .add_fields(
                    "test",
                    format_option(test, |o| o.to_formatted_string(script)),
                )
                .add_fields(
                    "update",
                    format_option(update, |o| o.to_formatted_string(script)),
                )
                .add_fields("body", body.to_formatted_string(script))
                .to_string(),
            StatementType::ForInStatement(data) => format!(
                "StatementType::ForInStatement({})",
                data.to_formatted_string(script)
            ),
            StatementType::ForOfStatement(data) => format!(
                "StatementType::ForOfStatement({})",
                data.to_formatted_string(script)
            ),
            StatementType::DeclarationStatement(data) => format!(
                "StatementType::DeclarationStatement({})",
                data.to_formatted_string(script)
            ),
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

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("FunctionBodyData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "body",
                format_vec(&self.body, |p| p.to_formatted_string(script)),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct ForIteratorData {
    pub meta: Meta,
    // As per ESTree spec (https://github.com/estree/estree/blob/master/es5.md#forinstatement) it
    // should be VariableDeclarationOrPattern. However, as per rules we left_hand_side_expression
    // which is an expression.
    pub left: VariableDeclarationOrExpression,
    pub right: Box<ExpressionType>,
    pub body: Box<StatementType>,
}

impl HasMeta for ForIteratorData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("ForIteratorData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("left", self.left.to_formatted_string(script))
            .add_fields("right", self.right.to_formatted_string(script))
            .add_fields("body", self.body.to_formatted_string(script))
            .to_string()
    }
}

#[derive(Debug)]
pub enum VariableDeclarationOrExpression {
    VariableDeclaration(VariableDeclarationData),
    Expression(Box<ExpressionType>),
}

impl HasMeta for VariableDeclarationOrExpression {
    fn get_meta(&self) -> &Meta {
        match self {
            VariableDeclarationOrExpression::VariableDeclaration(d) => d.get_meta(),
            VariableDeclarationOrExpression::Expression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            VariableDeclarationOrExpression::VariableDeclaration(d) => format!(
                "VariableDeclarationOrExpression::VariableDeclaration({})",
                d.to_formatted_string(script)
            ),
            VariableDeclarationOrExpression::Expression(d) => format!(
                "VariableDeclarationOrExpression::Expression({})",
                d.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub enum VariableDeclarationOrPattern {
    VariableDeclaration(VariableDeclarationData),
    Pattern(Box<PatternType>),
}

impl HasMeta for VariableDeclarationOrPattern {
    fn get_meta(&self) -> &Meta {
        match self {
            VariableDeclarationOrPattern::VariableDeclaration(d) => d.get_meta(),
            VariableDeclarationOrPattern::Pattern(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            VariableDeclarationOrPattern::VariableDeclaration(d) => format!(
                "VariableDeclarationOrPattern::VariableDeclaration({})",
                d.to_formatted_string(script)
            ),
            VariableDeclarationOrPattern::Pattern(d) => format!(
                "VariableDeclarationOrPattern::Pattern({})",
                d.to_formatted_string(script)
            ),
        }
    }
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

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            DeclarationType::FunctionDeclaration(data) => format!(
                "DeclarationType::FunctionDeclaration({})",
                data.to_formatted_string(script)
            ),
            DeclarationType::VariableDeclaration(data) => format!(
                "DeclarationType::VariableDeclaration({})",
                data.to_formatted_string(script)
            ),
            DeclarationType::ClassDeclaration(data) => format!(
                "DeclarationType::ClassDeclaration({})",
                data.to_formatted_string(script)
            ),
        }
    }
}

#[derive(Debug)]
pub struct VariableDeclarationData {
    pub meta: Meta,
    pub declarations: Vec<VariableDeclaratorData>,
    pub kind: VariableDeclarationKind,
}

impl HasMeta for VariableDeclarationData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("VariableDeclarationData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "declarations",
                format_vec(&self.declarations, |p| p.to_formatted_string(script)),
            )
            .add_fields("kind", format!("{:?}", self.kind))
            .to_string()
    }
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
    pub test: Option<Box<ExpressionType>>,
    pub consequent: Box<Vec<StatementType>>,
}

impl HasMeta for SwitchCaseData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("SwitchCaseData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "test",
                format_option(&self.test, |o| o.to_formatted_string(script)),
            )
            .add_fields(
                "consequent",
                format_vec(&self.consequent, |p| p.to_formatted_string(script)),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct FunctionData {
    pub meta: Meta,
    pub id: Option<IdentifierData>,
    pub params: Vec<Box<PatternType>>,
    pub body: FunctionBodyData,
    pub generator: bool,
}

impl HasMeta for FunctionData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("FunctionData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "id",
                format_option(&self.id, |o| o.to_formatted_string(script)),
            )
            .add_fields(
                "params",
                format_vec(&self.params, |p| p.to_formatted_string(script)),
            )
            .add_fields("body", self.body.to_formatted_string(script))
            .add_fields("generator", self.generator.to_string())
            .to_string()
    }
}

#[derive(Debug)]
pub struct CatchClauseData {
    pub meta: Meta,
    pub param: Box<PatternType>,
    pub body: BlockStatementData,
}

impl HasMeta for CatchClauseData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("CatchClauseData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("param", self.param.to_formatted_string(script))
            .add_fields("body", self.body.to_formatted_string(script))
            .to_string()
    }
}

#[derive(Debug)]
pub struct VariableDeclaratorData {
    pub meta: Meta,
    pub id: Box<PatternType>,
    pub init: Option<Box<ExpressionType>>,
}

impl HasMeta for VariableDeclaratorData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("VariableDeclaratorData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("id", self.id.to_formatted_string(script))
            .add_fields(
                "init",
                format_option(&self.init, |o| o.to_formatted_string(script)),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct PropertyData<V> {
    pub meta: Meta,
    pub key: LiteralOrIdentifier,
    pub value: V,
    pub kind: PropertyKind,
    pub method: bool,
    pub shorthand: bool,
    pub computed: bool,
}

impl HasMeta for PropertyData<Box<ExpressionType>> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("PropertyData<for Expression>")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("key", self.key.to_formatted_string(script))
            .add_fields("kind", format!("{:?}", self.kind))
            .add_fields("method", self.method.to_string())
            .add_fields("value", self.value.to_formatted_string(script))
            .to_string()
    }
}

impl HasMeta for PropertyData<Box<PatternType>> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("PropertyData<for Pattern>")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("key", self.key.to_formatted_string(script))
            .add_fields("kind", format!("{:?}", self.kind))
            .add_fields("method", self.method.to_string())
            .add_fields("value", self.value.to_formatted_string(script))
            .to_string()
    }
}

#[derive(Debug)]
pub enum LiteralOrIdentifier {
    Literal(LiteralData),
    Identifier(IdentifierData),
}

impl HasMeta for LiteralOrIdentifier {
    fn get_meta(&self) -> &Meta {
        match self {
            LiteralOrIdentifier::Literal(d) => d.get_meta(),
            LiteralOrIdentifier::Identifier(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self, script: &str) -> String {
        match self {
            LiteralOrIdentifier::Literal(d) => d.to_formatted_string(script),
            LiteralOrIdentifier::Identifier(d) => d.to_formatted_string(script),
        }
    }
}

#[derive(Debug)]
pub struct AssignmentPropertyData(pub PropertyData<Box<PatternType>>);

impl AssignmentPropertyData {
    fn new(
        meta: Meta,
        key: LiteralOrIdentifier,
        value: Box<PatternType>,
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

    fn to_formatted_string(&self, script: &str) -> String {
        format!(
            "AssignmentPropertyData({})",
            self.0.to_formatted_string(script)
        )
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
    pub super_class: Option<Box<ExpressionType>>,
    pub body: ClassBodyData,
}

impl HasMeta for ClassData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("ClassData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("id", format_has_meta_option(&self.id, script))
            .add_fields(
                "super_class",
                format_option(&self.super_class, |o| o.to_formatted_string(script)),
            )
            .add_fields("body", self.body.to_formatted_string(script))
            .to_string()
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

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("ClassBodyData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields(
                "body",
                format_vec(&self.body, |p| p.to_formatted_string(script)),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct MethodDefinitionData {
    pub meta: Meta,
    pub key: Box<ExpressionType>,
    pub value: FunctionData,
    pub kind: MethodDefinitionKind,
    pub computed: bool,
    pub static_flag: bool,
}

impl HasMeta for MethodDefinitionData {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self, script: &str) -> String {
        format_struct("MethodDefinitionData")
            .add_fields("meta", self.meta.to_formatted_string(script))
            .add_fields("key", self.key.to_formatted_string(script))
            .add_fields("value", self.value.to_formatted_string(script))
            .add_fields("kind", format!("{:?}", self.kind))
            .add_fields("computed", self.computed.to_string())
            .add_fields("static_flag", self.static_flag.to_string())
            .to_string()
    }
}

#[derive(Debug)]
pub enum MethodDefinitionKind {
    Constructor,
    Method,
    Get,
    Set,
}
