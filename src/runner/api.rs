use crate::parser::ast::{
    DeclarationType, ExpressionPatternType, Pattern, ProgramData, StatementType,
    VariableDeclarationKind,
};
use crate::runner::ds::{JsExecutionError, JsObject, JsScope, Lambda};
use std::ops::Deref;

pub struct JsRunner {}

impl JsRunner {}

fn run_ast(ast: &ProgramData, mut ctx: Box<JsScope>) -> Result<JsScope, JsExecutionError> {
    Ok(run_block_of_statements(&ast.body, ctx)?)
}

fn run_block_of_statements(
    statements: &Vec<StatementType>,
    mut ctx: Box<JsScope>,
) -> Result<JsScope, JsExecutionError> {
    // Scan for declarations
    for statement in statements {
        if let StatementType::DeclarationStatement(declaration) = statement {
            match declaration {
                DeclarationType::FunctionDeclaration(f) => ctx.values.insert(
                    &f.id.unwrap(),
                    Some(JsObject::Function(Lambda {
                        def: f,
                        scope: &ctx,
                    })),
                ),
                DeclarationType::VariableDeclaration(v) => {
                    if v.kind == VariableDeclarationKind::Var {
                        for d in v.declarations {
                            if let Some(id) = d.id.as_any().downcast_ref::<ExpressionPatternType>()
                            {
                                if let Some(id) = id
                                    .as_any()
                                    .downcast_ref::<ExpressionPatternType::Identifier>()
                                {
                                }
                            }
                        }
                    }
                }
                DeclarationType::ClassDeclaration(c) => unimplemented!(),
            }
        }
    }
    Ok(ctx)
}
