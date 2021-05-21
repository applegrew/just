use crate::parser::util::{format_has_meta_option, format_option, format_struct, format_vec};
use pest::error::Error;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

#[derive(Debug)]
pub struct JsError<'code, Rule> {
    pub kind: JsErrorType<'code, Rule>,
    pub message: String,
}

#[derive(Debug)]
pub enum JsErrorType<'code, R> {
    Unexpected(&'static str),
    ParserValidation(Error<R>),
    AstBuilderValidation(Meta<'code>),
}

#[derive(Debug)]
pub struct Meta<'code> {
    pub script: Vec<&'code str>,
}
impl<'code> Meta<'code> {
    pub fn new_by_concat(args: Vec<&'code Self>) -> Self {
        let mut script = vec![];
        for a in args {
            for s in a.script {
                script.push(s);
            }
        }
        Meta { script }
    }

    pub fn to_formatted_string(&self) -> String {
        format!("Meta {{ \"{}\" }}", self.script.join("").replace("\n", "░"))
    }

    pub fn to_formatted_code(&self) -> String {
        self.script.join("").to_string()
    }
}
impl<'code> Clone for Meta<'code> {
    fn clone(&self) -> Self {
        Meta {
            script: self.script,
        }
    }
}

pub trait HasMeta {
    fn get_meta(&self) -> &Meta;
    fn to_formatted_string(&self) -> String;
}

#[derive(Debug)]
pub struct IdentifierData<'code> {
    pub name: String,
    pub meta: Meta<'code>,
}
impl<'code> HasMeta for IdentifierData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("IdentifierData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("name", self.name.to_string())
            .to_string()
    }
}
impl<'code> PartialEq for IdentifierData<'code> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl<'code> Eq for IdentifierData<'code> {}
impl<'code> Hash for IdentifierData<'code> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

#[derive(Debug)]
pub enum ExpressionPatternType<'code> {
    Identifier(IdentifierData<'code>),
    MemberExpression(MemberExpressionType<'code>),
}
impl<'code> ExpressionPatternType<'code> {
    pub fn convert_to_pattern(self) -> PatternType<'code> {
        PatternType::PatternWhichCanBeExpression(self)
    }

    pub fn convert_to_expression(self) -> ExpressionType<'code> {
        ExpressionType::ExpressionWhichCanBePattern(self)
    }
}
impl<'code> HasMeta for ExpressionPatternType<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionPatternType::Identifier(data) => &data.meta,
            ExpressionPatternType::MemberExpression(data) => data.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            ExpressionPatternType::Identifier(data) => {
                format!(
                    "ExpressionPatternType::Identifier({})",
                    data.to_formatted_string()
                )
            }
            ExpressionPatternType::MemberExpression(data) => format!(
                "ExpressionPatternType::MemberExpression({})",
                data.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionType<'code> {
    ExpressionWhichCanBePattern(ExpressionPatternType<'code>),
    Literal(LiteralData<'code>),
    ThisExpression {
        meta: Meta<'code>,
    },
    ArrayExpression {
        meta: Meta<'code>,
        elements: Vec<Option<ExpressionOrSpreadElement<'code>>>,
    },
    ObjectExpression {
        meta: Meta<'code>,
        properties: Vec<PropertyData<'code, Box<ExpressionType<'code>>>>,
    },
    FunctionExpression(FunctionData<'code>),
    UnaryExpression {
        meta: Meta<'code>,
        operator: UnaryOperator,
        argument: Box<ExpressionType<'code>>,
    },
    UpdateExpression {
        meta: Meta<'code>,
        operator: UpdateOperator,
        argument: Box<ExpressionType<'code>>,
        prefix: bool,
    },
    BinaryExpression {
        meta: Meta<'code>,
        operator: BinaryOperator,
        left: Box<ExpressionType<'code>>,
        right: Box<ExpressionType<'code>>,
    },
    AssignmentExpression {
        meta: Meta<'code>,
        operator: AssignmentOperator,
        left: PatternOrExpression<'code>,
        right: Box<ExpressionType<'code>>,
    },
    LogicalExpression {
        meta: Meta<'code>,
        operator: LogicalOperator,
        left: Box<ExpressionType<'code>>,
        right: Box<ExpressionType<'code>>,
    },
    ConditionalExpression {
        meta: Meta<'code>,
        test: Box<ExpressionType<'code>>,
        consequent: Box<ExpressionType<'code>>,
        alternate: Box<ExpressionType<'code>>,
    },
    CallExpression {
        //A function or method call expression.
        meta: Meta<'code>,
        callee: ExpressionOrSuper<'code>,
        arguments: Vec<ExpressionOrSpreadElement<'code>>,
    },
    NewExpression {
        meta: Meta<'code>,
        callee: Box<ExpressionType<'code>>,
        arguments: Vec<ExpressionOrSpreadElement<'code>>,
    },
    SequenceExpression {
        //A comma-separated sequence of expressions
        meta: Meta<'code>,
        expressions: Vec<Box<ExpressionType<'code>>>,
    },
    ArrowFunctionExpression {
        meta: Meta<'code>,
        body: FunctionBodyOrExpression<'code>,
        expression: bool,
    },
    YieldExpression {
        meta: Meta<'code>,
        argument: Option<Box<ExpressionType<'code>>>,
        delegate: bool,
    },
    TemplateLiteral(TemplateLiteralData<'code>),
    TaggedTemplateExpression {
        meta: Meta<'code>,
        tag: Box<ExpressionType<'code>>,
        quasi: TemplateLiteralData<'code>,
    },
    ClassExpression(ClassData<'code>),
    MetaProperty {
        //Represents new.target meta property in ES2015. In the future, it will represent other meta properties as well.
        meta: Meta<'code>,
        meta_object: IdentifierData<'code>,
        property: IdentifierData<'code>,
    },
}
impl<'code> HasMeta for ExpressionType<'code> {
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

    fn to_formatted_string(&self) -> String {
        match self {
            ExpressionType::Literal(data) => {
                format!("ExpressionType::Literal({})", data.to_formatted_string())
            }
            ExpressionType::ThisExpression { meta } => {
                format_struct("ExpressionType::ThisExpression")
                    .add_fields("meta", meta.to_formatted_string())
                    .to_string()
            }
            ExpressionType::ArrayExpression { meta, elements } => {
                format_struct("ExpressionType::ArrayExpression")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields(
                        "elements",
                        format_vec(elements, |p| format_has_meta_option(&p)),
                    )
                    .to_string()
            }
            ExpressionType::ObjectExpression { meta, properties } => {
                format_struct("ExpressionType::ObjectExpression")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields(
                        "properties",
                        format_vec(properties, |p| p.to_formatted_string()),
                    )
                    .to_string()
            }
            ExpressionType::FunctionExpression(data) => format!(
                "ExpressionType::FunctionExpression({})",
                data.to_formatted_string()
            ),
            ExpressionType::UnaryExpression {
                meta,
                operator,
                argument,
            } => format_struct("ExpressionType::UnaryExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("argument", argument.to_formatted_string())
                .to_string(),
            ExpressionType::UpdateExpression {
                meta,
                operator,
                argument,
                prefix,
            } => format_struct("ExpressionType::UpdateExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("argument", argument.to_formatted_string())
                .add_fields("prefix", format!("{}", prefix))
                .to_string(),
            ExpressionType::BinaryExpression {
                meta,
                operator,
                left,
                right,
            } => format_struct("ExpressionType::BinaryExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("left", left.to_formatted_string())
                .add_fields("right", right.to_formatted_string())
                .to_string(),
            ExpressionType::AssignmentExpression {
                meta,
                operator,
                left,
                right,
            } => format_struct("ExpressionType::AssignmentExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("left", left.to_formatted_string())
                .add_fields("right", right.to_formatted_string())
                .to_string(),
            ExpressionType::LogicalExpression {
                meta,
                operator,
                left,
                right,
            } => format_struct("ExpressionType::LogicalExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("operator", format!("{:?}", operator))
                .add_fields("left", left.to_formatted_string())
                .add_fields("right", right.to_formatted_string())
                .to_string(),
            ExpressionType::ConditionalExpression {
                meta,
                test,
                consequent,
                alternate,
            } => format_struct("ExpressionType::ConditionalExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("test", test.to_formatted_string())
                .add_fields("consequent", consequent.to_formatted_string())
                .add_fields("alternate", alternate.to_formatted_string())
                .to_string(),
            ExpressionType::CallExpression {
                meta,
                callee,
                arguments,
            } => format_struct("ExpressionType::CallExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("callee", callee.to_formatted_string())
                .add_fields(
                    "arguments",
                    format_vec(arguments, |p| p.to_formatted_string()),
                )
                .to_string(),
            ExpressionType::NewExpression {
                meta,
                callee,
                arguments,
            } => format_struct("ExpressionType::NewExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("callee", callee.to_formatted_string())
                .add_fields(
                    "arguments",
                    format_vec(arguments, |p| p.to_formatted_string()),
                )
                .to_string(),
            ExpressionType::SequenceExpression { meta, expressions } => {
                format_struct("ExpressionType::SequenceExpression")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields(
                        "expressions",
                        format_vec(expressions, |p| p.to_formatted_string()),
                    )
                    .to_string()
            }
            ExpressionType::ArrowFunctionExpression {
                meta,
                body,
                expression,
            } => format_struct("ExpressionType::ArrowFunctionExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("body", body.to_formatted_string())
                .add_fields("expression", expression.to_string())
                .to_string(),
            ExpressionType::YieldExpression {
                meta,
                argument,
                delegate,
            } => format_struct("ExpressionType::YieldExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields(
                    "argument",
                    format_option(argument, |o| o.to_formatted_string()),
                )
                .add_fields("delegate", delegate.to_string())
                .to_string(),
            ExpressionType::TemplateLiteral(data) => format!(
                "ExpressionType::TemplateLiteral({})",
                data.to_formatted_string()
            ),
            ExpressionType::TaggedTemplateExpression { meta, tag, quasi } => {
                format_struct("ExpressionType::TaggedTemplateExpression")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("tag", tag.to_formatted_string())
                    .add_fields("quasi", quasi.to_formatted_string())
                    .to_string()
            }
            ExpressionType::ClassExpression(data) => format!(
                "ExpressionType::ClassExpression({})",
                data.to_formatted_string()
            ),
            ExpressionType::MetaProperty {
                meta,
                meta_object,
                property,
            } => format_struct("ExpressionType::TaggedTemplateExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("meta_object", meta_object.to_formatted_string())
                .add_fields("property", property.to_formatted_string())
                .to_string(),
            ExpressionType::ExpressionWhichCanBePattern(d) => format!(
                "ExpressionType::ExpressionWhichCanBePattern({})",
                d.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub enum PatternType<'code> {
    PatternWhichCanBeExpression(ExpressionPatternType<'code>),
    ObjectPattern {
        meta: Meta<'code>,
        properties: Vec<AssignmentPropertyData<'code>>,
    },
    ArrayPattern {
        meta: Meta<'code>,
        elements: Vec<Option<Box<PatternType<'code>>>>,
    },
    RestElement {
        meta: Meta<'code>,
        argument: Box<PatternType<'code>>,
    },
    AssignmentPattern {
        meta: Meta<'code>,
        left: Box<PatternType<'code>>,
        right: Box<ExpressionType<'code>>,
    },
}
impl<'code> HasMeta for PatternType<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            PatternType::ObjectPattern { meta, .. } => &meta,
            PatternType::ArrayPattern { meta, .. } => &meta,
            PatternType::RestElement { meta, .. } => &meta,
            PatternType::AssignmentPattern { meta, .. } => &meta,
            PatternType::PatternWhichCanBeExpression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            PatternType::ObjectPattern { meta, properties } => {
                format_struct("PatternType::ObjectPattern")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields(
                        "properties",
                        format_vec(properties, |p| p.to_formatted_string()),
                    )
                    .to_string()
            }
            PatternType::ArrayPattern { meta, elements } => {
                format_struct("PatternType::ArrayPattern")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields(
                        "elements",
                        format_vec(elements, |p| format_option(&p, |o| o.to_formatted_string())),
                    )
                    .to_string()
            }
            PatternType::RestElement { meta, argument } => {
                format_struct("PatternType::RestElement")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("argument", argument.to_formatted_string())
                    .to_string()
            }
            PatternType::AssignmentPattern { meta, left, right } => {
                format_struct("ExpressionType::AssignmentPattern")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("left", left.to_formatted_string())
                    .add_fields("right", right.to_formatted_string())
                    .to_string()
            }
            PatternType::PatternWhichCanBeExpression(d) => format!(
                "PatternType::PatternWhichCanBeExpression({})",
                d.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub struct TemplateLiteralData<'code> {
    pub meta: Meta<'code>,
    pub quasis: Vec<TemplateElementData<'code>>,
    pub expressions: Vec<Box<ExpressionType<'code>>>,
}
impl<'code> HasMeta for TemplateLiteralData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("TemplateLiteralData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields(
                "quasis",
                format_vec(&self.quasis, |p| p.to_formatted_string()),
            )
            .add_fields(
                "expressions",
                format_vec(&self.expressions, |p| p.to_formatted_string()),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct TemplateElementData<'code> {
    pub meta: Meta<'code>,
    pub tail: bool,
    pub cooked_value: String,
    pub raw_value: String,
}
impl<'code> HasMeta for TemplateElementData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("TemplateElementData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("tail", self.tail.to_string())
            .add_fields("cooked_value", self.cooked_value.to_string())
            .add_fields("raw_value", self.raw_value.to_string())
            .to_string()
    }
}

#[derive(Debug)]
pub enum FunctionBodyOrExpression<'code> {
    FunctionBody(FunctionBodyData<'code>),
    Expression(Box<ExpressionType<'code>>),
}
impl<'code> HasMeta for FunctionBodyOrExpression<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            FunctionBodyOrExpression::FunctionBody(d) => d.get_meta(),
            FunctionBodyOrExpression::Expression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            FunctionBodyOrExpression::FunctionBody(d) => format!(
                "FunctionBodyOrExpression::FunctionBody({})",
                d.to_formatted_string()
            ),
            FunctionBodyOrExpression::Expression(d) => format!(
                "FunctionBodyOrExpression::Expression({})",
                d.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionOrSuper<'code> {
    Expression(Box<ExpressionType<'code>>),
    Super,
}
impl<'code> HasMeta for ExpressionOrSuper<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionOrSuper::Expression(d) => d.get_meta(),
            ExpressionOrSuper::Super => panic!("Tried to get meta of ExpressionOrSuper::Super"),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            ExpressionOrSuper::Expression(d) => {
                format!("ExpressionOrSuper::Expression({})", d.to_formatted_string())
            }
            ExpressionOrSuper::Super => format!("ExpressionOrSuper::Super"),
        }
    }
}

#[derive(Debug)]
pub enum MemberExpressionType<'code> {
    SimpleMemberExpression {
        meta: Meta<'code>,
        object: ExpressionOrSuper<'code>,
        property: IdentifierData<'code>,
    },
    ComputedMemberExpression {
        meta: Meta<'code>,
        object: ExpressionOrSuper<'code>,
        property: Box<ExpressionType<'code>>,
    },
}
impl<'code> HasMeta for MemberExpressionType<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            MemberExpressionType::SimpleMemberExpression { meta, .. } => &meta,
            MemberExpressionType::ComputedMemberExpression { meta, .. } => &meta,
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            MemberExpressionType::SimpleMemberExpression {
                meta,
                object,
                property,
            } => format_struct("MemberExpressionType::SimpleMemberExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("object", object.to_formatted_string())
                .add_fields("property", property.to_formatted_string())
                .to_string(),
            MemberExpressionType::ComputedMemberExpression {
                meta,
                object,
                property,
            } => format_struct("MemberExpressionType::ComputedMemberExpression")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("object", object.to_formatted_string())
                .add_fields("property", property.to_formatted_string())
                .to_string(),
        }
    }
}

#[derive(Debug)]
pub enum ExpressionOrSpreadElement<'code> {
    Expression(Box<ExpressionType<'code>>),
    SpreadElement(Box<ExpressionType<'code>>),
}
impl<'code> HasMeta for ExpressionOrSpreadElement<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            ExpressionOrSpreadElement::Expression(data) => data.get_meta(),
            ExpressionOrSpreadElement::SpreadElement(data) => data.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            ExpressionOrSpreadElement::Expression(data) => format!(
                "ExpressionOrSpreadElement::Expression({})",
                data.to_formatted_string()
            ),
            ExpressionOrSpreadElement::SpreadElement(data) => format!(
                "ExpressionOrSpreadElement::SpreadElement({})",
                data.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub struct MemberExpressionData<'code> {
    pub meta: Meta<'code>,
    pub object: Box<ExpressionType<'code>>,
    pub property: Box<ExpressionType<'code>>,
    pub computed: bool,
}

#[derive(Debug)]
pub enum PatternOrExpression<'code> {
    Pattern(Box<PatternType<'code>>),
    Expression(Box<ExpressionType<'code>>),
}
impl<'code> HasMeta for PatternOrExpression<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            PatternOrExpression::Pattern(d) => d.get_meta(),
            PatternOrExpression::Expression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            PatternOrExpression::Pattern(d) => d.to_formatted_string(),
            PatternOrExpression::Expression(d) => d.to_formatted_string(),
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
pub struct LiteralData<'code> {
    pub meta: Meta<'code>,
    pub value: LiteralType,
}
impl<'code> HasMeta for LiteralData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("LiteralData")
            .add_fields("meta", self.meta.to_formatted_string())
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
pub struct ProgramData<'code> {
    pub meta: Meta<'code>,
    pub body: Vec<StatementType<'code>>,
}
impl<'code> HasMeta for ProgramData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("ProgramData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("body", format_vec(&self.body, |p| p.to_formatted_string()))
            .to_string()
    }
}

#[derive(Debug)]
pub struct BlockStatementData<'code> {
    pub meta: Meta<'code>,
    pub body: Box<Vec<StatementType<'code>>>,
}
impl<'code> HasMeta for BlockStatementData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("BlockStatementData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("body", format_vec(&self.body, |p| p.to_formatted_string()))
            .to_string()
    }
}

#[derive(Debug)]
pub enum StatementType<'code> {
    ExpressionStatement {
        meta: Meta<'code>,
        expression: Box<ExpressionType<'code>>,
    },
    BlockStatement(BlockStatementData<'code>),
    FunctionBody(FunctionBodyData<'code>),
    EmptyStatement {
        meta: Meta<'code>,
    },
    DebuggerStatement {
        meta: Meta<'code>,
    },
    ReturnStatement {
        meta: Meta<'code>,
        argument: Option<Box<ExpressionType<'code>>>,
    },
    //Label Statement not supported, hence break & continue with labels not supported
    BreakStatement {
        meta: Meta<'code>,
    },
    ContinueStatement {
        meta: Meta<'code>,
    },
    IfStatement {
        meta: Meta<'code>,
        test: Box<ExpressionType<'code>>,
        consequent: Box<StatementType<'code>>,
        alternate: Option<Box<StatementType<'code>>>,
    },
    SwitchStatement {
        meta: Meta<'code>,
        discriminant: Box<ExpressionType<'code>>,
        cases: Box<Vec<SwitchCaseData<'code>>>,
    },
    ThrowStatement {
        meta: Meta<'code>,
        argument: Box<ExpressionType<'code>>,
    },
    TryStatement {
        meta: Meta<'code>,
        block: BlockStatementData<'code>,
        handler: Option<CatchClauseData<'code>>,
        finalizer: Option<BlockStatementData<'code>>,
    },
    WhileStatement {
        meta: Meta<'code>,
        test: Box<ExpressionType<'code>>,
        body: Box<StatementType<'code>>,
    },
    DoWhileStatement {
        meta: Meta<'code>,
        test: Box<ExpressionType<'code>>,
        body: Box<StatementType<'code>>,
    },
    ForStatement {
        meta: Meta<'code>,
        init: Option<VariableDeclarationOrExpression<'code>>,
        test: Option<Box<ExpressionType<'code>>>,
        update: Option<Box<ExpressionType<'code>>>,
        body: Box<StatementType<'code>>,
    },
    ForInStatement(ForIteratorData<'code>),
    ForOfStatement(ForIteratorData<'code>),
    DeclarationStatement(DeclarationType<'code>),
}
impl<'code> HasMeta for StatementType<'code> {
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

    fn to_formatted_string(&self) -> String {
        match self {
            StatementType::ExpressionStatement { meta, expression } => {
                format_struct("StatementType::ExpressionStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("expression", expression.to_formatted_string())
                    .to_string()
            }
            StatementType::BlockStatement(data) => format!(
                "StatementType::BlockStatement({})",
                data.to_formatted_string()
            ),
            StatementType::FunctionBody(data) => format!(
                "StatementType::FunctionBody({})",
                data.to_formatted_string()
            ),
            StatementType::EmptyStatement { meta } => {
                format_struct("StatementType::EmptyStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .to_string()
            }
            StatementType::DebuggerStatement { meta } => {
                format_struct("StatementType::DebuggerStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .to_string()
            }
            StatementType::ReturnStatement { meta, argument } => {
                format_struct("StatementType::ReturnStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields(
                        "argument",
                        format_option(argument, |o| o.to_formatted_string()),
                    )
                    .to_string()
            }
            StatementType::BreakStatement { meta } => {
                format_struct("StatementType::BreakStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .to_string()
            }
            StatementType::ContinueStatement { meta } => {
                format_struct("StatementType::ContinueStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .to_string()
            }
            StatementType::IfStatement {
                meta,
                test,
                consequent,
                alternate,
            } => format_struct("StatementType::IfStatement")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("test", test.to_formatted_string())
                .add_fields("consequent", consequent.to_formatted_string())
                .add_fields(
                    "alternate",
                    format_option(alternate, |o| o.to_formatted_string()),
                )
                .to_string(),
            StatementType::SwitchStatement {
                meta,
                discriminant,
                cases,
            } => format_struct("StatementType::SwitchStatement")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("discriminant", discriminant.to_formatted_string())
                .add_fields("cases", format_vec(cases, |p| p.to_formatted_string()))
                .to_string(),
            StatementType::ThrowStatement { meta, argument } => {
                format_struct("StatementType::ThrowStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("argument", argument.to_formatted_string())
                    .to_string()
            }
            StatementType::TryStatement {
                meta,
                block,
                handler,
                finalizer,
            } => format_struct("StatementType::TryStatement")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("block", block.to_formatted_string())
                .add_fields(
                    "handler",
                    format_option(handler, |o| o.to_formatted_string()),
                )
                .add_fields(
                    "finalizer",
                    format_option(finalizer, |o| o.to_formatted_string()),
                )
                .to_string(),
            StatementType::WhileStatement { meta, test, body } => {
                format_struct("StatementType::WhileStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("test", test.to_formatted_string())
                    .add_fields("body", body.to_formatted_string())
                    .to_string()
            }
            StatementType::DoWhileStatement { meta, test, body } => {
                format_struct("StatementType::DoWhileStatement")
                    .add_fields("meta", meta.to_formatted_string())
                    .add_fields("test", test.to_formatted_string())
                    .add_fields("body", body.to_formatted_string())
                    .to_string()
            }
            StatementType::ForStatement {
                meta,
                init,
                test,
                update,
                body,
            } => format_struct("StatementType::ForStatement")
                .add_fields("meta", meta.to_formatted_string())
                .add_fields("init", format_option(init, |o| o.to_formatted_string()))
                .add_fields("test", format_option(test, |o| o.to_formatted_string()))
                .add_fields("update", format_option(update, |o| o.to_formatted_string()))
                .add_fields("body", body.to_formatted_string())
                .to_string(),
            StatementType::ForInStatement(data) => format!(
                "StatementType::ForInStatement({})",
                data.to_formatted_string()
            ),
            StatementType::ForOfStatement(data) => format!(
                "StatementType::ForOfStatement({})",
                data.to_formatted_string()
            ),
            StatementType::DeclarationStatement(data) => format!(
                "StatementType::DeclarationStatement({})",
                data.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub struct FunctionBodyData<'code> {
    pub meta: Meta<'code>,
    pub body: Box<Vec<StatementType<'code>>>, /*Actual code was -- Box<Vec<DirectiveOrStatement>> -- We do not support directives, like 'use strict'*/
}
impl<'code> HasMeta for FunctionBodyData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("FunctionBodyData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("body", format_vec(&self.body, |p| p.to_formatted_string()))
            .to_string()
    }
}

#[derive(Debug)]
pub struct ForIteratorData<'code> {
    pub meta: Meta<'code>,
    // As per ESTree spec (https://github.com/estree/estree/blob/master/es5.md#forinstatement) it
    // should be VariableDeclarationOrPattern. However, as per rules we left_hand_side_expression
    // which is an expression.
    pub left: VariableDeclarationOrExpression<'code>,
    pub right: Box<ExpressionType<'code>>,
    pub body: Box<StatementType<'code>>,
}
impl<'code> HasMeta for ForIteratorData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("ForIteratorData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("left", self.left.to_formatted_string())
            .add_fields("right", self.right.to_formatted_string())
            .add_fields("body", self.body.to_formatted_string())
            .to_string()
    }
}

#[derive(Debug)]
pub enum VariableDeclarationOrExpression<'code> {
    VariableDeclaration(VariableDeclarationData<'code>),
    Expression(Box<ExpressionType<'code>>),
}
impl<'code> HasMeta for VariableDeclarationOrExpression<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            VariableDeclarationOrExpression::VariableDeclaration(d) => d.get_meta(),
            VariableDeclarationOrExpression::Expression(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            VariableDeclarationOrExpression::VariableDeclaration(d) => format!(
                "VariableDeclarationOrExpression::VariableDeclaration({})",
                d.to_formatted_string()
            ),
            VariableDeclarationOrExpression::Expression(d) => format!(
                "VariableDeclarationOrExpression::Expression({})",
                d.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub enum VariableDeclarationOrPattern<'code> {
    VariableDeclaration(VariableDeclarationData<'code>),
    Pattern(Box<PatternType<'code>>),
}
impl<'code> HasMeta for VariableDeclarationOrPattern<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            VariableDeclarationOrPattern::VariableDeclaration(d) => d.get_meta(),
            VariableDeclarationOrPattern::Pattern(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            VariableDeclarationOrPattern::VariableDeclaration(d) => format!(
                "VariableDeclarationOrPattern::VariableDeclaration({})",
                d.to_formatted_string()
            ),
            VariableDeclarationOrPattern::Pattern(d) => format!(
                "VariableDeclarationOrPattern::Pattern({})",
                d.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub enum DeclarationType<'code> {
    FunctionDeclaration(FunctionData<'code>), //id is mandatory here
    VariableDeclaration(VariableDeclarationData<'code>),
    ClassDeclaration(ClassData<'code>),
}
impl<'code> HasMeta for DeclarationType<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            DeclarationType::FunctionDeclaration(data) => data.get_meta(),
            DeclarationType::VariableDeclaration(data) => data.get_meta(),
            DeclarationType::ClassDeclaration(data) => data.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            DeclarationType::FunctionDeclaration(data) => format!(
                "DeclarationType::FunctionDeclaration({})",
                data.to_formatted_string()
            ),
            DeclarationType::VariableDeclaration(data) => format!(
                "DeclarationType::VariableDeclaration({})",
                data.to_formatted_string()
            ),
            DeclarationType::ClassDeclaration(data) => format!(
                "DeclarationType::ClassDeclaration({})",
                data.to_formatted_string()
            ),
        }
    }
}

#[derive(Debug)]
pub struct VariableDeclarationData<'code> {
    pub meta: Meta<'code>,
    pub declarations: Vec<VariableDeclaratorData<'code>>,
    pub kind: VariableDeclarationKind,
}
impl<'code> HasMeta for VariableDeclarationData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("VariableDeclarationData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields(
                "declarations",
                format_vec(&self.declarations, |p| p.to_formatted_string()),
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
pub struct SwitchCaseData<'code> {
    pub meta: Meta<'code>,
    pub test: Option<Box<ExpressionType<'code>>>,
    pub consequent: Box<Vec<StatementType<'code>>>,
}
impl<'code> HasMeta for SwitchCaseData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("SwitchCaseData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields(
                "test",
                format_option(&self.test, |o| o.to_formatted_string()),
            )
            .add_fields(
                "consequent",
                format_vec(&self.consequent, |p| p.to_formatted_string()),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct FunctionData<'code> {
    pub meta: Meta<'code>,
    pub id: Option<IdentifierData<'code>>,
    pub params: Vec<Box<PatternType<'code>>>,
    pub body: FunctionBodyData<'code>,
    pub generator: bool,
}
impl<'code> HasMeta for FunctionData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("FunctionData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("id", format_option(&self.id, |o| o.to_formatted_string()))
            .add_fields(
                "params",
                format_vec(&self.params, |p| p.to_formatted_string()),
            )
            .add_fields("body", self.body.to_formatted_string())
            .add_fields("generator", self.generator.to_string())
            .to_string()
    }
}

#[derive(Debug)]
pub struct CatchClauseData<'code> {
    pub meta: Meta<'code>,
    pub param: Box<PatternType<'code>>,
    pub body: BlockStatementData<'code>,
}
impl<'code> HasMeta for CatchClauseData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("CatchClauseData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("param", self.param.to_formatted_string())
            .add_fields("body", self.body.to_formatted_string())
            .to_string()
    }
}

#[derive(Debug)]
pub struct VariableDeclaratorData<'code> {
    pub meta: Meta<'code>,
    pub id: Box<PatternType<'code>>,
    pub init: Option<Box<ExpressionType<'code>>>,
}
impl<'code> HasMeta for VariableDeclaratorData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("VariableDeclaratorData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("id", self.id.to_formatted_string())
            .add_fields(
                "init",
                format_option(&self.init, |o| o.to_formatted_string()),
            )
            .to_string()
    }
}

#[derive(Debug)]
pub struct PropertyData<'code, V> {
    pub meta: Meta<'code>,
    pub key: LiteralOrIdentifier<'code>,
    pub value: V,
    pub kind: PropertyKind,
    pub method: bool,
    pub shorthand: bool,
    pub computed: bool,
}
impl<'code> HasMeta for PropertyData<'code, Box<ExpressionType<'code>>> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("PropertyData<for Expression>")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("key", self.key.to_formatted_string())
            .add_fields("kind", format!("{:?}", self.kind))
            .add_fields("method", self.method.to_string())
            .add_fields("value", self.value.to_formatted_string())
            .to_string()
    }
}
impl<'code> HasMeta for PropertyData<'code, Box<PatternType<'code>>> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("PropertyData<for Pattern>")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("key", self.key.to_formatted_string())
            .add_fields("kind", format!("{:?}", self.kind))
            .add_fields("method", self.method.to_string())
            .add_fields("value", self.value.to_formatted_string())
            .to_string()
    }
}

#[derive(Debug)]
pub enum LiteralOrIdentifier<'code> {
    Literal(LiteralData<'code>),
    Identifier(IdentifierData<'code>),
}
impl<'code> HasMeta for LiteralOrIdentifier<'code> {
    fn get_meta(&self) -> &Meta {
        match self {
            LiteralOrIdentifier::Literal(d) => d.get_meta(),
            LiteralOrIdentifier::Identifier(d) => d.get_meta(),
        }
    }

    fn to_formatted_string(&self) -> String {
        match self {
            LiteralOrIdentifier::Literal(d) => d.to_formatted_string(),
            LiteralOrIdentifier::Identifier(d) => d.to_formatted_string(),
        }
    }
}

#[derive(Debug)]
pub struct AssignmentPropertyData<'code>(pub PropertyData<'code, Box<PatternType<'code>>>);
impl<'code> AssignmentPropertyData<'code> {
    fn new(
        meta: Meta<'code>,
        key: LiteralOrIdentifier<'code>,
        value: Box<PatternType<'code>>,
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
impl<'code> HasMeta for AssignmentPropertyData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.0.meta
    }

    fn to_formatted_string(&self) -> String {
        format!("AssignmentPropertyData({})", self.0.to_formatted_string())
    }
}

#[derive(Debug)]
pub enum PropertyKind {
    Init,
    Get,
    Set,
}

#[derive(Debug)]
pub struct ClassData<'code> {
    pub meta: Meta<'code>,
    pub id: Option<IdentifierData<'code>>,
    pub super_class: Option<Box<ExpressionType<'code>>>,
    pub body: ClassBodyData<'code>,
}
impl<'code> HasMeta for ClassData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("ClassData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("id", format_has_meta_option(&self.id))
            .add_fields(
                "super_class",
                format_option(&self.super_class, |o| o.to_formatted_string()),
            )
            .add_fields("body", self.body.to_formatted_string())
            .to_string()
    }
}

#[derive(Debug)]
pub struct ClassBodyData<'code> {
    pub meta: Meta<'code>,
    pub body: Vec<MethodDefinitionData<'code>>,
}
impl<'code> HasMeta for ClassBodyData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("ClassBodyData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("body", format_vec(&self.body, |p| p.to_formatted_string()))
            .to_string()
    }
}

#[derive(Debug)]
pub struct MethodDefinitionData<'code> {
    pub meta: Meta<'code>,
    pub key: Box<ExpressionType<'code>>,
    pub value: FunctionData<'code>,
    pub kind: MethodDefinitionKind,
    pub computed: bool,
    pub static_flag: bool,
}
impl<'code> HasMeta for MethodDefinitionData<'code> {
    fn get_meta(&self) -> &Meta {
        &self.meta
    }

    fn to_formatted_string(&self) -> String {
        format_struct("MethodDefinitionData")
            .add_fields("meta", self.meta.to_formatted_string())
            .add_fields("key", self.key.to_formatted_string())
            .add_fields("value", self.value.to_formatted_string())
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
