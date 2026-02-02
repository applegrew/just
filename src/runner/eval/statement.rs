//! Statement execution.
//!
//! This module provides statement execution logic for the JavaScript interpreter.

use crate::parser::ast::{
    ExpressionPatternType, PatternType, StatementType, DeclarationType, VariableDeclarationData,
    VariableDeclarationKind, BlockStatementData, ExpressionType,
};
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::JsValue;
use crate::runner::plugin::types::EvalContext;

use super::types::{Completion, CompletionType, EvalResult};
use super::expression::{evaluate_expression, to_boolean};

/// Execute a statement and return its completion.
pub fn execute_statement(
    stmt: &StatementType,
    ctx: &mut EvalContext,
) -> EvalResult {
    match stmt {
        StatementType::EmptyStatement { .. } => {
            Ok(Completion::normal())
        }

        StatementType::ExpressionStatement { expression, .. } => {
            let value = evaluate_expression(expression, ctx)?;
            Ok(Completion::normal_with_value(value))
        }

        StatementType::BlockStatement(block) => {
            execute_block_statement(block, ctx)
        }

        StatementType::DeclarationStatement(decl) => {
            execute_declaration(decl, ctx)
        }

        StatementType::IfStatement { test, consequent, alternate, .. } => {
            execute_if_statement(test, consequent, alternate.as_ref().map(|a| a.as_ref()), ctx)
        }

        StatementType::WhileStatement { test, body, .. } => {
            execute_while_statement(test, body, ctx)
        }

        StatementType::DoWhileStatement { test, body, .. } => {
            execute_do_while_statement(body, test, ctx)
        }

        StatementType::ForStatement { init, test, update, body, .. } => {
            execute_for_statement(init.as_ref(), test.as_ref().map(|t| t.as_ref()), update.as_ref().map(|u| u.as_ref()), body, ctx)
        }

        StatementType::ForInStatement(_) => {
            Err(JErrorType::TypeError("for-in not yet implemented".to_string()))
        }

        StatementType::ForOfStatement(_) => {
            Err(JErrorType::TypeError("for-of not yet implemented".to_string()))
        }

        StatementType::SwitchStatement { .. } => {
            Err(JErrorType::TypeError("switch not yet implemented".to_string()))
        }

        StatementType::BreakStatement { .. } => {
            Ok(Completion::break_completion(None))
        }

        StatementType::ContinueStatement { .. } => {
            Ok(Completion::continue_completion(None))
        }

        StatementType::ReturnStatement { argument, .. } => {
            let value = if let Some(arg) = argument {
                evaluate_expression(arg, ctx)?
            } else {
                JsValue::Undefined
            };
            Ok(Completion::return_value(value))
        }

        StatementType::ThrowStatement { argument, .. } => {
            let value = evaluate_expression(argument, ctx)?;
            Ok(Completion {
                completion_type: CompletionType::Throw,
                value: Some(value),
                target: None,
            })
        }

        StatementType::TryStatement { .. } => {
            Err(JErrorType::TypeError("try-catch not yet implemented".to_string()))
        }

        StatementType::DebuggerStatement { .. } => {
            Ok(Completion::normal())
        }

        StatementType::FunctionBody(_) => {
            // Function body is handled separately
            Ok(Completion::normal())
        }
    }
}

/// Execute a block statement.
fn execute_block_statement(
    block: &BlockStatementData,
    ctx: &mut EvalContext,
) -> EvalResult {
    // Create a new block scope for let/const bindings
    ctx.push_block_scope();

    let mut completion = Completion::normal();

    for stmt in block.body.iter() {
        completion = execute_statement(stmt, ctx)?;

        if completion.is_abrupt() {
            ctx.pop_block_scope();
            return Ok(completion);
        }
    }

    // Pop the block scope
    ctx.pop_block_scope();

    Ok(completion)
}

/// Execute a declaration.
fn execute_declaration(
    decl: &DeclarationType,
    ctx: &mut EvalContext,
) -> EvalResult {
    match decl {
        DeclarationType::VariableDeclaration(var_decl) => {
            execute_variable_declaration(var_decl, ctx)
        }

        DeclarationType::FunctionOrGeneratorDeclaration(_) => {
            Ok(Completion::normal())
        }

        DeclarationType::ClassDeclaration(_) => {
            Err(JErrorType::TypeError("class declaration not yet implemented".to_string()))
        }
    }
}

/// Execute a variable declaration.
fn execute_variable_declaration(
    var_decl: &VariableDeclarationData,
    ctx: &mut EvalContext,
) -> EvalResult {
    let is_const = matches!(var_decl.kind, VariableDeclarationKind::Const);
    let is_var = matches!(var_decl.kind, VariableDeclarationKind::Var);

    for declarator in &var_decl.declarations {
        // Get the binding name from the pattern
        let name = get_binding_name(&declarator.id)?;

        // Create the binding
        if is_var {
            // var declarations go in the variable environment
            ctx.create_var_binding(&name)?;
        } else {
            // let/const declarations go in the lexical environment
            ctx.create_binding(&name, is_const)?;
        }

        // Evaluate initializer and initialize the binding
        let value = if let Some(init) = &declarator.init {
            evaluate_expression(init, ctx)?
        } else {
            // const must have an initializer (checked by parser), var/let default to undefined
            JsValue::Undefined
        };

        // Initialize the binding with the value
        if is_var {
            ctx.initialize_var_binding(&name, value)?;
        } else {
            ctx.initialize_binding(&name, value)?;
        }
    }

    Ok(Completion::normal())
}

/// Get the binding name from a pattern.
fn get_binding_name(pattern: &PatternType) -> Result<String, JErrorType> {
    match pattern {
        PatternType::PatternWhichCanBeExpression(ExpressionPatternType::Identifier(id)) => {
            Ok(id.name.clone())
        }
        PatternType::ObjectPattern { .. } => {
            Err(JErrorType::TypeError("Object destructuring not yet supported".to_string()))
        }
        PatternType::ArrayPattern { .. } => {
            Err(JErrorType::TypeError("Array destructuring not yet supported".to_string()))
        }
        PatternType::RestElement { .. } => {
            Err(JErrorType::TypeError("Rest element not yet supported".to_string()))
        }
        PatternType::AssignmentPattern { .. } => {
            Err(JErrorType::TypeError("Default value patterns not yet supported".to_string()))
        }
    }
}

/// Execute an if statement.
fn execute_if_statement(
    test: &ExpressionType,
    consequent: &StatementType,
    alternate: Option<&StatementType>,
    ctx: &mut EvalContext,
) -> EvalResult {
    let test_value = evaluate_expression(test, ctx)?;

    if to_boolean(&test_value) {
        execute_statement(consequent, ctx)
    } else if let Some(alt) = alternate {
        execute_statement(alt, ctx)
    } else {
        Ok(Completion::normal())
    }
}

/// Execute a while statement.
fn execute_while_statement(
    test: &ExpressionType,
    body: &StatementType,
    ctx: &mut EvalContext,
) -> EvalResult {
    let mut completion = Completion::normal();

    loop {
        let test_value = evaluate_expression(test, ctx)?;

        if !to_boolean(&test_value) {
            break;
        }

        completion = execute_statement(body, ctx)?;

        match completion.completion_type {
            CompletionType::Break => {
                return Ok(Completion::normal_with_value(completion.get_value()));
            }
            CompletionType::Continue => {
                continue;
            }
            CompletionType::Return | CompletionType::Throw => {
                return Ok(completion);
            }
            CompletionType::Normal => {}
        }
    }

    Ok(completion)
}

/// Execute a do-while statement.
fn execute_do_while_statement(
    body: &StatementType,
    test: &ExpressionType,
    ctx: &mut EvalContext,
) -> EvalResult {
    let mut completion = Completion::normal();

    loop {
        completion = execute_statement(body, ctx)?;

        match completion.completion_type {
            CompletionType::Break => {
                return Ok(Completion::normal_with_value(completion.get_value()));
            }
            CompletionType::Continue => {}
            CompletionType::Return | CompletionType::Throw => {
                return Ok(completion);
            }
            CompletionType::Normal => {}
        }

        let test_value = evaluate_expression(test, ctx)?;
        if !to_boolean(&test_value) {
            break;
        }
    }

    Ok(completion)
}

/// Execute a for statement.
fn execute_for_statement(
    init: Option<&crate::parser::ast::VariableDeclarationOrExpression>,
    test: Option<&ExpressionType>,
    update: Option<&ExpressionType>,
    body: &StatementType,
    ctx: &mut EvalContext,
) -> EvalResult {
    use crate::parser::ast::VariableDeclarationOrExpression;

    if let Some(init) = init {
        match init {
            VariableDeclarationOrExpression::VariableDeclaration(decl) => {
                execute_variable_declaration(decl, ctx)?;
            }
            VariableDeclarationOrExpression::Expression(expr) => {
                evaluate_expression(expr, ctx)?;
            }
        }
    }

    let mut completion = Completion::normal();

    loop {
        if let Some(test) = test {
            let test_value = evaluate_expression(test, ctx)?;
            if !to_boolean(&test_value) {
                break;
            }
        }

        completion = execute_statement(body, ctx)?;

        match completion.completion_type {
            CompletionType::Break => {
                return Ok(Completion::normal_with_value(completion.get_value()));
            }
            CompletionType::Continue => {}
            CompletionType::Return | CompletionType::Throw => {
                return Ok(completion);
            }
            CompletionType::Normal => {}
        }

        if let Some(update) = update {
            evaluate_expression(update, ctx)?;
        }
    }

    Ok(completion)
}
