enum Statement {
    DebuggerStatement,
    ContinueStatement,
    BreakStatement,
    ThrowStatement,
    IfStatement,
    WithStatement,
    TryStatement,
    VariableStatement,
    DoStatement,
    WhileStatement,
    ForStatement,
    SwitchStatement,
    BlockStatement,
    ExpressionStatement,
    // LabelledStatement,
    ReturnStatement,
}

enum Declaration {
    GeneratorDeclaration(CallableDeclarationBody),
    FunctionDeclaration(CallableDeclarationBody),
    ClassDeclaration(ClassDeclarationBody),
    LexicalDeclaration(LexicalDeclarationBody)
}

enum Expression {
    ArrowFunctionExpression,
    AssignmentExpression,
    ConditionalExpression,
    YieldExpression,
    FunctionExpression,
    ClassExpression,
    ParenthesisExpression,
    IdentifierRef,
    ThisRef,
    NullLiteral,
    BooleanLiteral,
    IntegerLiteral(i16),
    FloatLiteral(f64),
    StringLiteral(String),
    RegularExpressionLiteral(RegexContent),
    ArrayLiteral(Vec<Expression>),
    ObjectLiteral(Vec<ObjectItem>),
    TemplateLiteral(TemplateLiteralContent),
}

enum TemplateContent {
    TextContent(String),
    ScriptContent(Expression)
}

enum ObjectItem {
    FixedNameValuePair,
    ComputedNameValuePair,
    MethodDefinition,
    IdentifierRefOrAssignmentExpression(Expression),
}

struct ArrayBody {
    items: Vec<Expression>
}

struct CallableDeclarationBody {

}

struct TemplateLiteralContent {
    contents: Vec<TemplateContent>
}

struct RegexContent {
    expression: String,
    flags: Vec<char>
}

struct ClassDeclarationBody {

}

struct LexicalDeclarationBody {

}

pub struct Script {
    declarations: Vec<Declaration>,
    statements: Vec<Statement>
}
