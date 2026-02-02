//! Expression evaluation.
//!
//! This module provides the core expression evaluation logic for the JavaScript interpreter.
//! It handles all expression types defined in the AST.

use crate::parser::ast::{
    AssignmentOperator, BinaryOperator, ExpressionOrSpreadElement, ExpressionOrSuper,
    ExpressionType, ExpressionPatternType, LiteralData, LiteralType, MemberExpressionType,
    PatternOrExpression, PatternType, PropertyData, PropertyKind, UnaryOperator, UpdateOperator,
    LogicalOperator,
};
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::object::{JsObjectType, ObjectType};
use crate::runner::ds::object_property::{PropertyDescriptor, PropertyDescriptorData, PropertyKey};
use crate::runner::ds::value::{JsValue, JsNumberType};
use crate::runner::plugin::types::{EvalContext, SimpleObject};

use std::cell::RefCell;
use std::rc::Rc;

use super::types::ValueResult;

/// Evaluate an expression and return its value.
pub fn evaluate_expression(
    expr: &ExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    match expr {
        ExpressionType::Literal(lit) => evaluate_literal(lit),

        ExpressionType::ExpressionWhichCanBePattern(pattern) => {
            evaluate_expression_pattern(pattern, ctx)
        }

        ExpressionType::ThisExpression { .. } => {
            Ok(ctx.global_this.clone().unwrap_or(JsValue::Undefined))
        }

        ExpressionType::ArrayExpression { elements, .. } => {
            evaluate_array_expression(elements, ctx)
        }

        ExpressionType::ObjectExpression { properties, .. } => {
            evaluate_object_expression(properties, ctx)
        }

        ExpressionType::FunctionOrGeneratorExpression(_) => {
            Err(JErrorType::TypeError("Function expression not yet implemented".to_string()))
        }

        ExpressionType::UnaryExpression { operator, argument, .. } => {
            evaluate_unary_expression(operator, argument, ctx)
        }

        ExpressionType::BinaryExpression { operator, left, right, .. } => {
            evaluate_binary_expression(operator, left, right, ctx)
        }

        ExpressionType::LogicalExpression { operator, left, right, .. } => {
            evaluate_logical_expression(operator, left, right, ctx)
        }

        ExpressionType::UpdateExpression { operator, argument, prefix, .. } => {
            evaluate_update_expression(operator, argument, *prefix, ctx)
        }

        ExpressionType::AssignmentExpression { operator, left, right, .. } => {
            evaluate_assignment_expression(operator, left, right, ctx)
        }

        ExpressionType::ConditionalExpression { test, consequent, alternate, .. } => {
            evaluate_conditional_expression(test, consequent, alternate, ctx)
        }

        ExpressionType::CallExpression { callee, arguments, .. } => {
            evaluate_call_expression(callee, arguments, ctx)
        }

        ExpressionType::NewExpression { .. } => {
            Err(JErrorType::TypeError("New expression not yet implemented".to_string()))
        }

        ExpressionType::SequenceExpression { expressions, .. } => {
            evaluate_sequence_expression(expressions, ctx)
        }

        ExpressionType::TemplateLiteral(_) => {
            Err(JErrorType::TypeError("Template literal not yet implemented".to_string()))
        }

        ExpressionType::TaggedTemplateExpression { .. } => {
            Err(JErrorType::TypeError("Tagged template expression not yet implemented".to_string()))
        }

        ExpressionType::ClassExpression(_) => {
            Err(JErrorType::TypeError("Class expression not yet implemented".to_string()))
        }

        ExpressionType::YieldExpression { .. } => {
            Err(JErrorType::TypeError("Yield expression not yet implemented".to_string()))
        }

        ExpressionType::MetaProperty { .. } => {
            Err(JErrorType::TypeError("Meta property not yet implemented".to_string()))
        }

        ExpressionType::ArrowFunctionExpression { .. } => {
            Err(JErrorType::TypeError("Arrow function expression not yet implemented".to_string()))
        }

        ExpressionType::MemberExpression(member_expr) => {
            evaluate_member_expression(member_expr, ctx)
        }
    }
}

/// Evaluate a literal and return its value.
fn evaluate_literal(lit: &LiteralData) -> ValueResult {
    Ok(match &lit.value {
        LiteralType::NullLiteral => JsValue::Null,
        LiteralType::BooleanLiteral(b) => JsValue::Boolean(*b),
        LiteralType::StringLiteral(s) => JsValue::String(s.clone()),
        LiteralType::NumberLiteral(n) => {
            use crate::parser::ast::NumberLiteralType;
            match n {
                NumberLiteralType::IntegerLiteral(i) => JsValue::Number(JsNumberType::Integer(*i)),
                NumberLiteralType::FloatLiteral(f) => JsValue::Number(JsNumberType::Float(*f)),
            }
        }
        LiteralType::RegExpLiteral { .. } => {
            return Err(JErrorType::TypeError("RegExp literal not yet implemented".to_string()));
        }
    })
}

/// Evaluate an array expression and return an array object.
fn evaluate_array_expression(
    elements: &[Option<ExpressionOrSpreadElement>],
    ctx: &mut EvalContext,
) -> ValueResult {
    use crate::runner::ds::object::JsObject;

    // Create a new array object
    let mut array_obj = SimpleObject::new();

    let mut index = 0;
    for element in elements {
        if let Some(elem) = element {
            let value = match elem {
                ExpressionOrSpreadElement::Expression(expr) => evaluate_expression(expr, ctx)?,
                ExpressionOrSpreadElement::SpreadElement(_) => {
                    return Err(JErrorType::TypeError(
                        "Spread in arrays not yet supported".to_string(),
                    ));
                }
            };

            // Define the element as a property
            let key = PropertyKey::Str(index.to_string());
            array_obj.get_object_base_mut().properties.insert(
                key,
                PropertyDescriptor::Data(PropertyDescriptorData {
                    value,
                    writable: true,
                    enumerable: true,
                    configurable: true,
                }),
            );
        }
        // Holes (None elements) are left as undefined/missing
        index += 1;
    }

    // Set the length property
    array_obj.get_object_base_mut().properties.insert(
        PropertyKey::Str("length".to_string()),
        PropertyDescriptor::Data(PropertyDescriptorData {
            value: JsValue::Number(JsNumberType::Integer(index as i64)),
            writable: true,
            enumerable: false,
            configurable: false,
        }),
    );

    // Wrap in JsObjectType
    let obj: JsObjectType = Rc::new(RefCell::new(ObjectType::Ordinary(Box::new(array_obj))));
    Ok(JsValue::Object(obj))
}

/// Evaluate an object expression and return an object.
fn evaluate_object_expression(
    properties: &[PropertyData<Box<ExpressionType>>],
    ctx: &mut EvalContext,
) -> ValueResult {
    use crate::runner::ds::object::JsObject;

    // Create a new object
    let mut obj = SimpleObject::new();

    for prop in properties {
        // Get the property key
        let key = get_object_property_key(&prop.key, prop.computed, ctx)?;

        // Only handle init properties for now (not getters/setters)
        match prop.kind {
            PropertyKind::Init => {
                // Evaluate the value
                let value = evaluate_expression(&prop.value, ctx)?;

                // Define the property
                obj.get_object_base_mut().properties.insert(
                    PropertyKey::Str(key),
                    PropertyDescriptor::Data(PropertyDescriptorData {
                        value,
                        writable: true,
                        enumerable: true,
                        configurable: true,
                    }),
                );
            }
            PropertyKind::Get | PropertyKind::Set => {
                return Err(JErrorType::TypeError(
                    "Getter/setter properties not yet supported".to_string(),
                ));
            }
        }
    }

    // Wrap in JsObjectType
    let obj_ref: JsObjectType = Rc::new(RefCell::new(ObjectType::Ordinary(Box::new(obj))));
    Ok(JsValue::Object(obj_ref))
}

/// Get the property key from an object literal property.
fn get_object_property_key(
    key_expr: &ExpressionType,
    computed: bool,
    ctx: &mut EvalContext,
) -> Result<String, JErrorType> {
    if computed {
        // Computed property: [expr]
        let key_value = evaluate_expression(key_expr, ctx)?;
        Ok(to_string(&key_value))
    } else {
        // Static property key
        match key_expr {
            ExpressionType::ExpressionWhichCanBePattern(ExpressionPatternType::Identifier(id)) => {
                Ok(id.name.clone())
            }
            ExpressionType::Literal(lit) => match &lit.value {
                LiteralType::StringLiteral(s) => Ok(s.clone()),
                LiteralType::NumberLiteral(n) => {
                    use crate::parser::ast::NumberLiteralType;
                    match n {
                        NumberLiteralType::IntegerLiteral(i) => Ok(i.to_string()),
                        NumberLiteralType::FloatLiteral(f) => Ok(f.to_string()),
                    }
                }
                _ => Err(JErrorType::TypeError("Invalid property key".to_string())),
            },
            _ => Err(JErrorType::TypeError("Invalid property key".to_string())),
        }
    }
}

/// Evaluate an expression pattern (identifier).
fn evaluate_expression_pattern(
    pattern: &ExpressionPatternType,
    ctx: &mut EvalContext,
) -> ValueResult {
    match pattern {
        ExpressionPatternType::Identifier(id) => {
            // Look up the identifier in the environment chain
            ctx.get_binding(&id.name)
        }
    }
}

/// Evaluate a member expression (property access).
fn evaluate_member_expression(
    member_expr: &MemberExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    match member_expr {
        MemberExpressionType::SimpleMemberExpression { object, property, .. } => {
            // Evaluate the object
            let obj_value = evaluate_expression_or_super(object, ctx)?;
            let prop_name = &property.name;

            // Get the property
            get_property(&obj_value, prop_name)
        }
        MemberExpressionType::ComputedMemberExpression { object, property, .. } => {
            // Evaluate the object
            let obj_value = evaluate_expression_or_super(object, ctx)?;

            // Evaluate the property expression to get the key
            let prop_value = evaluate_expression(property, ctx)?;
            let prop_name = to_property_key(&prop_value);

            // Get the property
            get_property(&obj_value, &prop_name)
        }
    }
}

/// Evaluate an ExpressionOrSuper.
fn evaluate_expression_or_super(
    expr_or_super: &ExpressionOrSuper,
    ctx: &mut EvalContext,
) -> ValueResult {
    match expr_or_super {
        ExpressionOrSuper::Expression(expr) => evaluate_expression(expr, ctx),
        ExpressionOrSuper::Super => {
            Err(JErrorType::TypeError("super not yet supported".to_string()))
        }
    }
}

/// Evaluate a call expression (function call).
fn evaluate_call_expression(
    callee: &ExpressionOrSuper,
    arguments: &[ExpressionOrSpreadElement],
    ctx: &mut EvalContext,
) -> ValueResult {
    // Evaluate the callee to get the function
    let callee_value = evaluate_expression_or_super(callee, ctx)?;

    // Get the 'this' value for the call
    let this_value = get_call_this_value(callee, ctx);

    // Evaluate the arguments
    let args = evaluate_arguments(arguments, ctx)?;

    // Call the function
    call_value(&callee_value, this_value, args, ctx)
}

/// Get the 'this' value for a call expression.
fn get_call_this_value(callee: &ExpressionOrSuper, ctx: &mut EvalContext) -> JsValue {
    match callee {
        ExpressionOrSuper::Expression(expr) => {
            match expr.as_ref() {
                // For member expressions, 'this' is the object
                ExpressionType::MemberExpression(member) => {
                    match member {
                        MemberExpressionType::SimpleMemberExpression { object, .. } |
                        MemberExpressionType::ComputedMemberExpression { object, .. } => {
                            match object {
                                ExpressionOrSuper::Expression(obj_expr) => {
                                    evaluate_expression(obj_expr, ctx).unwrap_or(JsValue::Undefined)
                                }
                                ExpressionOrSuper::Super => JsValue::Undefined,
                            }
                        }
                    }
                }
                // For other expressions, 'this' is undefined (or global in non-strict)
                _ => ctx.global_this.clone().unwrap_or(JsValue::Undefined),
            }
        }
        ExpressionOrSuper::Super => JsValue::Undefined,
    }
}

/// Evaluate call arguments.
fn evaluate_arguments(
    arguments: &[ExpressionOrSpreadElement],
    ctx: &mut EvalContext,
) -> Result<Vec<JsValue>, JErrorType> {
    let mut args = Vec::with_capacity(arguments.len());
    for arg in arguments {
        match arg {
            ExpressionOrSpreadElement::Expression(expr) => {
                args.push(evaluate_expression(expr, ctx)?);
            }
            ExpressionOrSpreadElement::SpreadElement(_) => {
                return Err(JErrorType::TypeError("Spread in calls not yet supported".to_string()));
            }
        }
    }
    Ok(args)
}

/// Call a value as a function.
fn call_value(
    callee: &JsValue,
    this_value: JsValue,
    args: Vec<JsValue>,
    ctx: &mut EvalContext,
) -> ValueResult {
    match callee {
        JsValue::Object(obj) => {
            let obj_ref = obj.borrow();
            if obj_ref.is_callable() {
                drop(obj_ref);
                // For now, we'll call through our execution context stack
                // This is a simplified implementation that doesn't fully support
                // user-defined functions yet, but will work for native functions
                // stored in NativeFunctionObject
                call_function_object(obj, this_value, args, ctx)
            } else {
                Err(JErrorType::TypeError(format!(
                    "{} is not a function",
                    callee
                )))
            }
        }
        _ => Err(JErrorType::TypeError(format!(
            "{} is not a function",
            callee
        ))),
    }
}

/// Call a function object.
fn call_function_object(
    func: &crate::runner::ds::object::JsObjectType,
    _this_value: JsValue,
    args: Vec<JsValue>,
    ctx: &mut EvalContext,
) -> ValueResult {
    let func_ref = func.borrow();

    // Check if it's a function object
    if let ObjectType::Function(func_obj) = &*func_ref {
        // Get function metadata (these are Rc so cloning is cheap)
        let body = func_obj.get_function_object_base().body_code.clone();
        let params = func_obj.get_function_object_base().formal_parameters.clone();
        let func_env = func_obj.get_function_object_base().environment.clone();

        drop(func_ref);

        // Create a new function scope
        let saved_lex_env = ctx.lex_env.clone();
        let saved_var_env = ctx.var_env.clone();

        // Create new environment for the function, with the function's closure as outer
        use crate::runner::ds::env_record::new_declarative_environment;
        let func_scope = new_declarative_environment(Some(func_env));
        ctx.lex_env = func_scope.clone();
        ctx.var_env = func_scope;

        // Bind parameters to arguments
        for (i, param) in params.iter().enumerate() {
            if let crate::parser::ast::PatternType::PatternWhichCanBeExpression(
                ExpressionPatternType::Identifier(id)
            ) = param {
                let arg_value = args.get(i).cloned().unwrap_or(JsValue::Undefined);
                ctx.create_binding(&id.name, false)?;
                ctx.initialize_binding(&id.name, arg_value)?;
            }
        }

        // Execute each statement in the function body
        use super::statement::execute_statement;
        use super::types::CompletionType;

        let mut result_completion = super::types::Completion::normal();

        for stmt in body.body.iter() {
            let completion = execute_statement(stmt, ctx)?;

            match completion.completion_type {
                CompletionType::Return => {
                    result_completion = completion;
                    break;
                }
                CompletionType::Throw => {
                    // Restore environment before returning error
                    ctx.lex_env = saved_lex_env;
                    ctx.var_env = saved_var_env;
                    return Err(JErrorType::TypeError(format!(
                        "Uncaught exception: {:?}",
                        completion.value
                    )));
                }
                CompletionType::Break | CompletionType::Continue => {
                    // These shouldn't escape function body
                    result_completion = completion;
                    break;
                }
                CompletionType::Normal => {
                    result_completion = completion;
                }
            }
        }

        // Restore the previous environment
        ctx.lex_env = saved_lex_env;
        ctx.var_env = saved_var_env;

        // Return the result
        match result_completion.completion_type {
            CompletionType::Return => Ok(result_completion.get_value()),
            _ => Ok(JsValue::Undefined),
        }
    } else {
        Err(JErrorType::TypeError("Object is not callable".to_string()))
    }
}

/// Get a property from a value.
fn get_property(value: &JsValue, prop_name: &str) -> ValueResult {
    match value {
        JsValue::Object(obj) => {
            use crate::runner::ds::object_property::PropertyKey;
            use crate::runner::ds::object::JsObject;

            let obj_ref = obj.borrow();
            let prop_key = PropertyKey::Str(prop_name.to_string());

            // Check own property
            if let Some(desc) = obj_ref.as_js_object().get_own_property(&prop_key)? {
                use crate::runner::ds::object_property::PropertyDescriptor;
                match desc {
                    PropertyDescriptor::Data(data) => Ok(data.value.clone()),
                    PropertyDescriptor::Accessor(_) => {
                        Err(JErrorType::TypeError("Accessor properties not yet supported".to_string()))
                    }
                }
            } else {
                // Check prototype chain
                if let Some(proto) = obj_ref.as_js_object().get_prototype_of() {
                    drop(obj_ref);  // Release borrow before recursive call
                    get_property(&JsValue::Object(proto), prop_name)
                } else {
                    Ok(JsValue::Undefined)
                }
            }
        }
        JsValue::String(s) => {
            // String primitive property access
            if prop_name == "length" {
                Ok(JsValue::Number(JsNumberType::Integer(s.len() as i64)))
            } else if let Ok(index) = prop_name.parse::<usize>() {
                // Access character at index
                if index < s.len() {
                    Ok(JsValue::String(s.chars().nth(index).unwrap().to_string()))
                } else {
                    Ok(JsValue::Undefined)
                }
            } else {
                // Other string methods not yet supported
                Ok(JsValue::Undefined)
            }
        }
        JsValue::Undefined => {
            Err(JErrorType::TypeError("Cannot read property of undefined".to_string()))
        }
        JsValue::Null => {
            Err(JErrorType::TypeError("Cannot read property of null".to_string()))
        }
        _ => {
            // Primitive types - return undefined for now
            Ok(JsValue::Undefined)
        }
    }
}

/// Convert a value to a property key string.
fn to_property_key(value: &JsValue) -> String {
    to_string(value)
}

/// Evaluate an assignment expression.
fn evaluate_assignment_expression(
    operator: &AssignmentOperator,
    left: &PatternOrExpression,
    right: &ExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    // Get the name to assign to
    let name = match left {
        PatternOrExpression::Pattern(pattern) => get_pattern_name(pattern)?,
        PatternOrExpression::Expression(expr) => get_expression_name(expr)?,
    };

    // Evaluate the right-hand side
    let rhs_value = evaluate_expression(right, ctx)?;

    // Compute the final value based on the operator
    let final_value = match operator {
        AssignmentOperator::Equals => rhs_value,
        AssignmentOperator::AddEquals => {
            let current = ctx.get_binding(&name)?;
            add_values(&current, &rhs_value)?
        }
        AssignmentOperator::SubtractEquals => {
            let current = ctx.get_binding(&name)?;
            subtract_values(&current, &rhs_value)?
        }
        AssignmentOperator::MultiplyEquals => {
            let current = ctx.get_binding(&name)?;
            multiply_values(&current, &rhs_value)?
        }
        AssignmentOperator::DivideEquals => {
            let current = ctx.get_binding(&name)?;
            divide_values(&current, &rhs_value)?
        }
        AssignmentOperator::ModuloEquals => {
            let current = ctx.get_binding(&name)?;
            modulo_values(&current, &rhs_value)?
        }
        AssignmentOperator::BitwiseLeftShiftEquals => {
            let current = ctx.get_binding(&name)?;
            left_shift(&current, &rhs_value)?
        }
        AssignmentOperator::BitwiseRightShiftEquals => {
            let current = ctx.get_binding(&name)?;
            right_shift(&current, &rhs_value)?
        }
        AssignmentOperator::BitwiseUnsignedRightShiftEquals => {
            let current = ctx.get_binding(&name)?;
            unsigned_right_shift(&current, &rhs_value)?
        }
        AssignmentOperator::BitwiseOrEquals => {
            let current = ctx.get_binding(&name)?;
            bitwise_or(&current, &rhs_value)?
        }
        AssignmentOperator::BitwiseAndEquals => {
            let current = ctx.get_binding(&name)?;
            bitwise_and(&current, &rhs_value)?
        }
        AssignmentOperator::BitwiseXorEquals => {
            let current = ctx.get_binding(&name)?;
            bitwise_xor(&current, &rhs_value)?
        }
    };

    // Set the binding and return the value
    ctx.set_binding(&name, final_value.clone())?;
    Ok(final_value)
}

/// Get the name from a pattern (for simple identifier patterns).
fn get_pattern_name(pattern: &PatternType) -> Result<String, JErrorType> {
    match pattern {
        PatternType::PatternWhichCanBeExpression(ExpressionPatternType::Identifier(id)) => {
            Ok(id.name.clone())
        }
        _ => Err(JErrorType::TypeError(
            "Complex patterns in assignment not yet supported".to_string(),
        )),
    }
}

/// Get the name from an expression (for simple identifier expressions).
fn get_expression_name(expr: &ExpressionType) -> Result<String, JErrorType> {
    match expr {
        ExpressionType::ExpressionWhichCanBePattern(ExpressionPatternType::Identifier(id)) => {
            Ok(id.name.clone())
        }
        _ => Err(JErrorType::TypeError(
            "Assignment to non-identifier expressions not yet supported".to_string(),
        )),
    }
}

/// Evaluate a unary expression.
fn evaluate_unary_expression(
    operator: &UnaryOperator,
    argument: &ExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    match operator {
        UnaryOperator::TypeOf => {
            let value = evaluate_expression(argument, ctx).unwrap_or(JsValue::Undefined);
            Ok(JsValue::String(get_typeof_string(&value)))
        }
        UnaryOperator::Void => {
            let _ = evaluate_expression(argument, ctx)?;
            Ok(JsValue::Undefined)
        }
        UnaryOperator::LogicalNot => {
            let value = evaluate_expression(argument, ctx)?;
            Ok(JsValue::Boolean(!to_boolean(&value)))
        }
        UnaryOperator::Minus => {
            let value = evaluate_expression(argument, ctx)?;
            negate_number(&value)
        }
        UnaryOperator::Plus => {
            let value = evaluate_expression(argument, ctx)?;
            to_number(&value)
        }
        UnaryOperator::BitwiseNot => {
            let value = evaluate_expression(argument, ctx)?;
            bitwise_not(&value)
        }
        UnaryOperator::Delete => {
            Err(JErrorType::TypeError("Delete operator not yet implemented".to_string()))
        }
    }
}

/// Evaluate a binary expression.
fn evaluate_binary_expression(
    operator: &BinaryOperator,
    left: &ExpressionType,
    right: &ExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    let left_val = evaluate_expression(left, ctx)?;
    let right_val = evaluate_expression(right, ctx)?;

    match operator {
        // Arithmetic
        BinaryOperator::Add => add_values(&left_val, &right_val),
        BinaryOperator::Subtract => subtract_values(&left_val, &right_val),
        BinaryOperator::Multiply => multiply_values(&left_val, &right_val),
        BinaryOperator::Divide => divide_values(&left_val, &right_val),
        BinaryOperator::Modulo => modulo_values(&left_val, &right_val),

        // Comparison
        BinaryOperator::LessThan => compare_values(&left_val, &right_val, |a, b| a < b),
        BinaryOperator::GreaterThan => compare_values(&left_val, &right_val, |a, b| a > b),
        BinaryOperator::LessThanEqual => compare_values(&left_val, &right_val, |a, b| a <= b),
        BinaryOperator::GreaterThanEqual => compare_values(&left_val, &right_val, |a, b| a >= b),

        // Equality
        BinaryOperator::StrictlyEqual => Ok(JsValue::Boolean(strict_equality(&left_val, &right_val))),
        BinaryOperator::StrictlyUnequal => Ok(JsValue::Boolean(!strict_equality(&left_val, &right_val))),
        BinaryOperator::LooselyEqual => Ok(JsValue::Boolean(loose_equality(&left_val, &right_val))),
        BinaryOperator::LooselyUnequal => Ok(JsValue::Boolean(!loose_equality(&left_val, &right_val))),

        // Bitwise
        BinaryOperator::BitwiseAnd => bitwise_and(&left_val, &right_val),
        BinaryOperator::BitwiseOr => bitwise_or(&left_val, &right_val),
        BinaryOperator::BitwiseXor => bitwise_xor(&left_val, &right_val),
        BinaryOperator::BitwiseLeftShift => left_shift(&left_val, &right_val),
        BinaryOperator::BitwiseRightShift => right_shift(&left_val, &right_val),
        BinaryOperator::BitwiseUnsignedRightShift => unsigned_right_shift(&left_val, &right_val),

        // Other
        BinaryOperator::InstanceOf => {
            Err(JErrorType::TypeError("instanceof not yet implemented".to_string()))
        }
        BinaryOperator::In => {
            Err(JErrorType::TypeError("in operator not yet implemented".to_string()))
        }
    }
}

/// Evaluate a logical expression with short-circuit evaluation.
fn evaluate_logical_expression(
    operator: &LogicalOperator,
    left: &ExpressionType,
    right: &ExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    let left_val = evaluate_expression(left, ctx)?;

    match operator {
        LogicalOperator::And => {
            if !to_boolean(&left_val) {
                Ok(left_val)
            } else {
                evaluate_expression(right, ctx)
            }
        }
        LogicalOperator::Or => {
            if to_boolean(&left_val) {
                Ok(left_val)
            } else {
                evaluate_expression(right, ctx)
            }
        }
    }
}

/// Evaluate a conditional (ternary) expression.
fn evaluate_conditional_expression(
    test: &ExpressionType,
    consequent: &ExpressionType,
    alternate: &ExpressionType,
    ctx: &mut EvalContext,
) -> ValueResult {
    let test_val = evaluate_expression(test, ctx)?;

    if to_boolean(&test_val) {
        evaluate_expression(consequent, ctx)
    } else {
        evaluate_expression(alternate, ctx)
    }
}

/// Evaluate a sequence expression.
fn evaluate_sequence_expression(
    expressions: &[Box<ExpressionType>],
    ctx: &mut EvalContext,
) -> ValueResult {
    let mut result = JsValue::Undefined;
    for expr in expressions {
        result = evaluate_expression(expr.as_ref(), ctx)?;
    }
    Ok(result)
}

/// Evaluate an update expression (++x, x++, --x, x--).
fn evaluate_update_expression(
    operator: &UpdateOperator,
    argument: &ExpressionType,
    prefix: bool,
    ctx: &mut EvalContext,
) -> ValueResult {
    // Get the variable name from the argument
    let name = get_expression_name(argument)?;

    // Get the current value
    let current_value = ctx.get_binding(&name)?;

    // Convert to number
    let old_number = to_number(&current_value)?;
    let old_f64 = match &old_number {
        JsValue::Number(n) => number_to_f64(n),
        _ => f64::NAN,
    };

    // Compute the new value based on operator
    let new_f64 = match operator {
        UpdateOperator::PlusPlus => old_f64 + 1.0,
        UpdateOperator::MinusMinus => old_f64 - 1.0,
    };

    // Create the new JsValue
    let new_value = if new_f64.fract() == 0.0 && new_f64.abs() < i64::MAX as f64 {
        JsValue::Number(JsNumberType::Integer(new_f64 as i64))
    } else {
        JsValue::Number(JsNumberType::Float(new_f64))
    };

    // Store the new value
    ctx.set_binding(&name, new_value.clone())?;

    // Return old value for postfix, new value for prefix
    if prefix {
        Ok(new_value)
    } else {
        Ok(old_number)
    }
}

// ============================================================================
// Type conversion helpers
// ============================================================================

/// Convert a value to boolean.
pub fn to_boolean(value: &JsValue) -> bool {
    match value {
        JsValue::Undefined => false,
        JsValue::Null => false,
        JsValue::Boolean(b) => *b,
        JsValue::Number(n) => match n {
            JsNumberType::Integer(0) => false,
            JsNumberType::Float(f) if *f == 0.0 || f.is_nan() => false,
            JsNumberType::NaN => false,
            _ => true,
        },
        JsValue::String(s) => !s.is_empty(),
        JsValue::Symbol(_) => true,
        JsValue::Object(_) => true,
    }
}

/// Get the typeof string for a value.
fn get_typeof_string(value: &JsValue) -> String {
    match value {
        JsValue::Undefined => "undefined".to_string(),
        JsValue::Null => "object".to_string(),
        JsValue::Boolean(_) => "boolean".to_string(),
        JsValue::Number(_) => "number".to_string(),
        JsValue::String(_) => "string".to_string(),
        JsValue::Symbol(_) => "symbol".to_string(),
        JsValue::Object(_) => "object".to_string(),
    }
}

/// Convert a value to a number.
fn to_number(value: &JsValue) -> ValueResult {
    Ok(match value {
        JsValue::Undefined => JsValue::Number(JsNumberType::NaN),
        JsValue::Null => JsValue::Number(JsNumberType::Integer(0)),
        JsValue::Boolean(true) => JsValue::Number(JsNumberType::Integer(1)),
        JsValue::Boolean(false) => JsValue::Number(JsNumberType::Integer(0)),
        JsValue::Number(n) => JsValue::Number(n.clone()),
        JsValue::String(s) => {
            if s.is_empty() {
                JsValue::Number(JsNumberType::Integer(0))
            } else if let Ok(i) = s.trim().parse::<i64>() {
                JsValue::Number(JsNumberType::Integer(i))
            } else if let Ok(f) = s.trim().parse::<f64>() {
                JsValue::Number(JsNumberType::Float(f))
            } else {
                JsValue::Number(JsNumberType::NaN)
            }
        }
        JsValue::Symbol(_) => {
            return Err(JErrorType::TypeError("Cannot convert Symbol to number".to_string()));
        }
        JsValue::Object(_) => JsValue::Number(JsNumberType::NaN),
    })
}

/// Negate a number value.
fn negate_number(value: &JsValue) -> ValueResult {
    let num_value = to_number(value)?;
    Ok(match num_value {
        JsValue::Number(JsNumberType::Integer(i)) => JsValue::Number(JsNumberType::Integer(-i)),
        JsValue::Number(JsNumberType::Float(f)) => JsValue::Number(JsNumberType::Float(-f)),
        JsValue::Number(JsNumberType::PositiveInfinity) => JsValue::Number(JsNumberType::NegativeInfinity),
        JsValue::Number(JsNumberType::NegativeInfinity) => JsValue::Number(JsNumberType::PositiveInfinity),
        JsValue::Number(JsNumberType::NaN) => JsValue::Number(JsNumberType::NaN),
        _ => JsValue::Number(JsNumberType::NaN),
    })
}

/// Bitwise NOT operation.
fn bitwise_not(value: &JsValue) -> ValueResult {
    let num = to_i32(value)?;
    Ok(JsValue::Number(JsNumberType::Integer(!num as i64)))
}

/// Convert to i32 for bitwise operations.
fn to_i32(value: &JsValue) -> Result<i32, JErrorType> {
    match to_number(value)? {
        JsValue::Number(JsNumberType::Integer(i)) => Ok(i as i32),
        JsValue::Number(JsNumberType::Float(f)) => Ok(f as i32),
        JsValue::Number(JsNumberType::NaN) => Ok(0),
        JsValue::Number(JsNumberType::PositiveInfinity) => Ok(0),
        JsValue::Number(JsNumberType::NegativeInfinity) => Ok(0),
        _ => Ok(0),
    }
}

/// Convert to u32 for unsigned bitwise operations.
fn to_u32(value: &JsValue) -> Result<u32, JErrorType> {
    Ok(to_i32(value)? as u32)
}

// ============================================================================
// Arithmetic operations
// ============================================================================

fn add_values(left: &JsValue, right: &JsValue) -> ValueResult {
    if matches!(left, JsValue::String(_)) || matches!(right, JsValue::String(_)) {
        let left_str = to_string(left);
        let right_str = to_string(right);
        return Ok(JsValue::String(format!("{}{}", left_str, right_str)));
    }

    let left_num = to_number(left)?;
    let right_num = to_number(right)?;
    apply_numeric_op(&left_num, &right_num, |a, b| a + b, |a, b| a + b)
}

fn subtract_values(left: &JsValue, right: &JsValue) -> ValueResult {
    let left_num = to_number(left)?;
    let right_num = to_number(right)?;
    apply_numeric_op(&left_num, &right_num, |a, b| a - b, |a, b| a - b)
}

fn multiply_values(left: &JsValue, right: &JsValue) -> ValueResult {
    let left_num = to_number(left)?;
    let right_num = to_number(right)?;
    apply_numeric_op(&left_num, &right_num, |a, b| a * b, |a, b| a * b)
}

fn divide_values(left: &JsValue, right: &JsValue) -> ValueResult {
    let left_num = to_number(left)?;
    let right_num = to_number(right)?;

    if let JsValue::Number(JsNumberType::Integer(0)) = right_num {
        return Ok(JsValue::Number(JsNumberType::PositiveInfinity));
    }
    if let JsValue::Number(JsNumberType::Float(f)) = right_num {
        if f == 0.0 {
            return Ok(JsValue::Number(JsNumberType::PositiveInfinity));
        }
    }

    apply_numeric_op(&left_num, &right_num, |a, b| a / b, |a, b| a / b)
}

fn modulo_values(left: &JsValue, right: &JsValue) -> ValueResult {
    let left_num = to_number(left)?;
    let right_num = to_number(right)?;
    apply_numeric_op(&left_num, &right_num, |a, b| a % b, |a, b| a % b)
}

fn apply_numeric_op<F, G>(left: &JsValue, right: &JsValue, int_op: F, float_op: G) -> ValueResult
where
    F: Fn(i64, i64) -> i64,
    G: Fn(f64, f64) -> f64,
{
    match (left, right) {
        (JsValue::Number(JsNumberType::NaN), _) | (_, JsValue::Number(JsNumberType::NaN)) => {
            Ok(JsValue::Number(JsNumberType::NaN))
        }
        (JsValue::Number(JsNumberType::Integer(a)), JsValue::Number(JsNumberType::Integer(b))) => {
            Ok(JsValue::Number(JsNumberType::Integer(int_op(*a, *b))))
        }
        (JsValue::Number(a), JsValue::Number(b)) => {
            let a_f64 = number_to_f64(a);
            let b_f64 = number_to_f64(b);
            Ok(JsValue::Number(JsNumberType::Float(float_op(a_f64, b_f64))))
        }
        _ => Ok(JsValue::Number(JsNumberType::NaN)),
    }
}

fn number_to_f64(n: &JsNumberType) -> f64 {
    match n {
        JsNumberType::Integer(i) => *i as f64,
        JsNumberType::Float(f) => *f,
        JsNumberType::NaN => f64::NAN,
        JsNumberType::PositiveInfinity => f64::INFINITY,
        JsNumberType::NegativeInfinity => f64::NEG_INFINITY,
    }
}

// ============================================================================
// Comparison operations
// ============================================================================

fn compare_values<F>(left: &JsValue, right: &JsValue, cmp: F) -> ValueResult
where
    F: Fn(f64, f64) -> bool,
{
    let left_num = to_number(left)?;
    let right_num = to_number(right)?;

    let result = match (&left_num, &right_num) {
        (JsValue::Number(JsNumberType::NaN), _) | (_, JsValue::Number(JsNumberType::NaN)) => false,
        (JsValue::Number(a), JsValue::Number(b)) => {
            cmp(number_to_f64(a), number_to_f64(b))
        }
        _ => false,
    };

    Ok(JsValue::Boolean(result))
}

pub fn strict_equality(left: &JsValue, right: &JsValue) -> bool {
    match (left, right) {
        (JsValue::Undefined, JsValue::Undefined) => true,
        (JsValue::Null, JsValue::Null) => true,
        (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
        (JsValue::String(a), JsValue::String(b)) => a == b,
        (JsValue::Number(JsNumberType::NaN), _) | (_, JsValue::Number(JsNumberType::NaN)) => false,
        (JsValue::Number(a), JsValue::Number(b)) => {
            number_to_f64(a) == number_to_f64(b)
        }
        (JsValue::Object(a), JsValue::Object(b)) => std::rc::Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn loose_equality(left: &JsValue, right: &JsValue) -> bool {
    if std::mem::discriminant(left) == std::mem::discriminant(right) {
        return strict_equality(left, right);
    }

    match (left, right) {
        (JsValue::Null, JsValue::Undefined) | (JsValue::Undefined, JsValue::Null) => true,
        (JsValue::Number(_), JsValue::String(_)) => {
            if let Ok(r) = to_number(right) {
                strict_equality(left, &r)
            } else {
                false
            }
        }
        (JsValue::String(_), JsValue::Number(_)) => {
            if let Ok(l) = to_number(left) {
                strict_equality(&l, right)
            } else {
                false
            }
        }
        (JsValue::Boolean(_), _) => {
            if let Ok(l) = to_number(left) {
                loose_equality(&l, right)
            } else {
                false
            }
        }
        (_, JsValue::Boolean(_)) => {
            if let Ok(r) = to_number(right) {
                loose_equality(left, &r)
            } else {
                false
            }
        }
        _ => false,
    }
}

// ============================================================================
// Bitwise operations
// ============================================================================

fn bitwise_and(left: &JsValue, right: &JsValue) -> ValueResult {
    let l = to_i32(left)?;
    let r = to_i32(right)?;
    Ok(JsValue::Number(JsNumberType::Integer((l & r) as i64)))
}

fn bitwise_or(left: &JsValue, right: &JsValue) -> ValueResult {
    let l = to_i32(left)?;
    let r = to_i32(right)?;
    Ok(JsValue::Number(JsNumberType::Integer((l | r) as i64)))
}

fn bitwise_xor(left: &JsValue, right: &JsValue) -> ValueResult {
    let l = to_i32(left)?;
    let r = to_i32(right)?;
    Ok(JsValue::Number(JsNumberType::Integer((l ^ r) as i64)))
}

fn left_shift(left: &JsValue, right: &JsValue) -> ValueResult {
    let l = to_i32(left)?;
    let r = to_u32(right)? & 0x1f;
    Ok(JsValue::Number(JsNumberType::Integer((l << r) as i64)))
}

fn right_shift(left: &JsValue, right: &JsValue) -> ValueResult {
    let l = to_i32(left)?;
    let r = to_u32(right)? & 0x1f;
    Ok(JsValue::Number(JsNumberType::Integer((l >> r) as i64)))
}

fn unsigned_right_shift(left: &JsValue, right: &JsValue) -> ValueResult {
    let l = to_u32(left)?;
    let r = to_u32(right)? & 0x1f;
    Ok(JsValue::Number(JsNumberType::Integer((l >> r) as i64)))
}

// ============================================================================
// String conversion
// ============================================================================

fn to_string(value: &JsValue) -> String {
    match value {
        JsValue::Undefined => "undefined".to_string(),
        JsValue::Null => "null".to_string(),
        JsValue::Boolean(true) => "true".to_string(),
        JsValue::Boolean(false) => "false".to_string(),
        JsValue::Number(n) => n.to_string(),
        JsValue::String(s) => s.clone(),
        JsValue::Symbol(_) => "[Symbol]".to_string(),
        JsValue::Object(_) => "[object Object]".to_string(),
    }
}
