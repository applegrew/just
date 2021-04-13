use crate::parser::ast::StatementType::VariableStatement;
use crate::parser::ast::{AdditiveOperator, Argument, ArgumentList, ArrayItem, AssignmentExpression, AssignmentOperator, BindingElement, BindingPattern, BindingType, CallExpressionType, CodeBlock, ComplexPropertyAccessExpressionData, DeclarationType, Expression, FormalParameters, FunctionCallExpressionType, FunctionDefinition, HoistableDeclarationType, Instruction, JsError, LhsExpressionType, LhsRhsAssignmentExpressionType, LiteralType, LogicAndMathExpression, LogicalEqualityOperator, MemberExpression, MultiplicativeOperator, NewExpressionType, NumericType, PostfixOperator, PrimaryExpressionType, RelationalOperator, ShiftOperator, SimpleBinding, SimplePropertyAccessExpressionData, StatementType, UnaryOperator, BindingProperty};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::time::Instant;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
pub struct JsParser;

const TAB_WIDTH: usize = 2;

fn pair_to_string(pair: Pair<Rule>, level: usize) -> Vec<String> {
    let mut tree = vec![];
    let span = pair.as_span();
    let rule_name = format!(
        "{:?} => ({},{}) #{:?}",
        pair.as_rule(),
        span.start(),
        span.end(),
        span.as_str()
    );
    let mut string_pads = String::with_capacity(level * TAB_WIDTH);
    for _ in 1..level * TAB_WIDTH + 1 {
        string_pads.push('_');
    }
    tree.push(format!("{}{}", string_pads, rule_name));
    for child_pair in pair.into_inner() {
        tree.append(pair_to_string(child_pair, level + 1).as_mut());
    }
    tree
}

impl JsParser {
    pub fn parse_to_token_tree(script: &str) -> Result<String, String> {
        let mut tree = vec![];
        let start = Instant::now();
        let result = Self::parse(Rule::script, script);
        let end = Instant::now();
        let total_time = end.saturating_duration_since(start);
        println!("Actual parse time is {}ms", total_time.as_millis());

        match result {
            Ok(pairs) => {
                for pair in pairs {
                    tree.push(pair_to_string(pair, 0).join("\n"));
                }
            }
            Err(rule) => {
                return Err(format!("Parse error due to {:?}", rule));
            }
        }
        Ok(tree.join("\n"))
    }

    pub fn parse_to_ast(script: &str) -> Result<CodeBlock, JsError> {
        let result = Self::parse(Rule::script, script);
        match result {
            Ok(pairs) => build_ast_from_script(pairs),
            Err(rule) => {
                return Err(JsError {
                    src: "parse_to_ast",
                    message: format!("Parse error due to {:?}", rule),
                });
            }
        }
    }
}

fn get_unexpected_error(src: &'static str, pair: &Pair<Rule>) -> JsError {
    let message = format!("Unexpected state reached in the parser at \"{:?}\". This indicates internal logic error in the parser.", pair.as_rule());
    JsError { src, message }
}

fn build_ast_from_script(pairs: Pairs<Rule>) -> Result<CodeBlock, JsError> {
    let mut instructions = vec![];
    for pair in pairs {
        match pair.as_rule() {
            Rule::declaration => {
                instructions.push(Instruction::Declaration(build_ast_from_declaration(pair)?))
            }
            Rule::EOI => { /* Do nothing */ }
            Rule::statement => {
                // Then this should be one of the statements
                instructions.push(Instruction::Statement(build_ast_from_statement(pair)?))
            }
            _ => return Err(get_unexpected_error("build_ast_from_script", &pair)),
        };
    }
    Ok(CodeBlock { instructions })
}

fn build_ast_from_declaration(pair: Pair<Rule>) -> Result<DeclarationType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::hoistable_declaration | Rule::hoistable_declaration__yield => {
            DeclarationType::HoistableDeclaration(build_ast_from_hoistable_declaration(inner_pair)?)
        }
        Rule::class_declaration | Rule::class_declaration__yield => {
            unimplemented!()
        }
        Rule::lexical_declaration__in | Rule::lexical_declaration__in_yield => {
            build_ast_from_lexical_declaration(inner_pair)?
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_declaration",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_lexical_declaration(pair: Pair<Rule>) -> Result<DeclarationType, JsError> {
    unimplemented!()
}

fn build_ast_from_hoistable_declaration(
    pair: Pair<Rule>,
) -> Result<HoistableDeclarationType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::generator_declaration | Rule::generator_declaration__yield => {
            HoistableDeclarationType::GeneratorDeclaration(build_ast_from_generator_declaration(
                inner_pair,
            )?)
        }
        Rule::function_declaration | Rule::function_declaration__yield => {
            HoistableDeclarationType::FunctionDeclaration(build_ast_from_function_declaration(
                inner_pair,
            )?)
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_hoistable_declaration",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_function_declaration(pair: Pair<Rule>) -> Result<FunctionDefinition, JsError> {
    let mut pair_iter = pair.into_inner();
    let f_name = pair_iter.next().unwrap().as_str().to_string();
    let formal_parameters = pair_iter.next().unwrap();
    let mut args = vec![];
    for param in formal_parameters.into_inner() {
        args.push(match param.as_rule() {
            Rule::function_rest_parameter | Rule::function_rest_parameter__yield => {
                let binding_rest_element = param.into_inner().next().unwrap();
                let binding_identifier = binding_rest_element.into_inner().next().unwrap();
                FunctionArgumentType::Rest(Data::IdentifierName(
                    binding_identifier.as_str().to_string(),
                ))
            }
            Rule::formal_parameter | Rule::formal_parameter__yield => {
                let binding_element = param.into_inner().next().unwrap();
                let mut binding_element_inner_iter = binding_element.into_inner();
                let binding_element_inner = binding_element_inner_iter.next().unwrap();
                if binding_element_inner.as_rule() == Rule::single_name_binding {
                    let mut single_name_binding_iter = binding_element_inner.into_inner();
                    let binding_identifier = Data::IdentifierName(
                        single_name_binding_iter
                            .next()
                            .unwrap()
                            .as_str()
                            .to_string(),
                    );
                    if let Some(initializer) = single_name_binding_iter.next() {
                        FunctionArgumentType::Regular {
                            identifier: binding_identifier,
                            default: Some(Box::new(build_ast_from_assignment_expression(
                                initializer.into_inner().next().unwrap(),
                            )?)),
                        }
                    } else {
                        FunctionArgumentType::Regular {
                            identifier: binding_identifier,
                            default: None,
                        }
                    }
                } else if binding_element_inner.as_rule() == Rule::binding_pattern {
                    //TODO
                    panic!("Not yet implemented!");
                } else {
                    return Err(get_unexpected_error(17, &binding_element_inner));
                }
            }
            _ => return Err(get_unexpected_error(16, &param)),
        });
    }
    let function_body = pair_iter.next().unwrap();
    let body = Box::new(build_ast_from_statement_list_pairs(
        function_body.into_inner(),
    )?);
    Ok(Node::Declaration(DeclarationType::FunctionDeclaration {
        name: f_name,
        arguments: args,
        body,
    }))
}

fn build_ast_from_formal_parameters(pair: Pair<Rule>) -> Result<FormalParameters, JsError> {}

fn build_ast_from_statement(pair: Pair<Rule>) -> Result<StatementType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::debugger_statement => unimplemented!(),
        Rule::continue_statement | Rule::continue_statement__yield => unimplemented!(),
        Rule::break_statement | Rule::break_statement__yield => unimplemented!(),
        Rule::throw_statement | Rule::throw_statement__yield => unimplemented!(),
        Rule::if_statement
        | Rule::if_statement__yield
        | Rule::if_statement__return
        | Rule::if_statement__yield_return => unimplemented!(),
        Rule::with_statement
        | Rule::with_statement__yield
        | Rule::with_statement__return
        | Rule::with_statement__yield_return => unimplemented!(),
        Rule::try_statement
        | Rule::try_statement__yield
        | Rule::try_statement__return
        | Rule::try_statement__yield_return => unimplemented!(),
        Rule::variable_statement | Rule::variable_statement__yield => {
            build_ast_from_variable_statement(inner_pair)?
        }
        Rule::breakable_statement
        | Rule::breakable_statement__yield
        | Rule::breakable_statement__return
        | Rule::breakable_statement__yield_return => unimplemented!(),
        Rule::block_statement
        | Rule::block_statement__yield
        | Rule::block_statement__return
        | Rule::block_statement__yield_return => unimplemented!(),
        Rule::expression_statement | Rule::expression_statement__yield => {
            StatementType::ExpressionStatement(build_ast_from_expression(
                inner_pair.into_inner().next().unwrap(),
            )?)
        }
        Rule::labelled_statement
        | Rule::labelled_statement__yield
        | Rule::labelled_statement__return
        | Rule::labelled_statement__yield_return => unimplemented!(),
        Rule::empty_statement => unimplemented!(),
        Rule::return_statement | Rule::return_statement__yield => unimplemented!(),
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_statement",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_variable_statement(pair: Pair<Rule>) -> Result<StatementType, JsError> {
    let mut declarations = vec![];
    for var_pair in pair.into_inner() {
        if var_pair.as_rule() == Rule::variable_declaration
            || var_pair.as_rule() == Rule::variable_declaration__in
        {
            declarations.push(
                build_ast_from_lexical_binding_or_variable_declaration_or_binding_element(
                    var_pair,
                )?,
            )
        } else if var_pair.as_rule() != Rule::smart_semicolon {
            // smart_semicolon at the end is expected but nothing except smart_semicolon.
            return Err(get_unexpected_error(
                "build_ast_from_variable_statement",
                &var_pair,
            ));
        }
    }
    Ok(VariableStatement(declarations))
}

fn build_ast_from_lexical_binding_or_variable_declaration_or_binding_element(
    pair: Pair<Rule>,
) -> Result<BindingElement, JsError> {
    let mut lexical_binding_inner_iter = pair.into_inner();
    let lexical_binding_inner = lexical_binding_inner_iter.next().unwrap();
    Ok(
        if lexical_binding_inner.as_rule() == Rule::binding_identifier
            || lexical_binding_inner.as_rule() == Rule::binding_identifier__yield
        {
            let id = lexical_binding_inner.as_str().to_string();
            if let Some(initializer) = lexical_binding_inner_iter.next() {
                BindingElement::DirectBindingElement(SimpleBinding {
                    identifier: id,
                    initializer: Some(build_ast_from_assignment_expression(
                        initializer.into_inner().next().unwrap(),
                    )?),
                })
            } else {
                BindingElement::DirectBindingElement(SimpleBinding {
                    identifier: id,
                    initializer: None,
                })
            }
        } else if lexical_binding_inner.as_rule() == Rule::binding_pattern
            || lexical_binding_inner.as_rule() == Rule::binding_pattern__yield
        {
            BindingElement::PatternBindingElement {
                binding_pattern: build_ast_from_binding_pattern(lexical_binding_inner)?,
                initializer: Some(build_ast_from_assignment_expression(
                    lexical_binding_inner_iter.next().unwrap(),
                )?),
            }
        } else if lexical_binding_inner.as_rule() == Rule::single_name_binding
            || lexical_binding_inner.as_rule() == Rule::single_name_binding__yield {
            BindingElement::DirectBindingElement(build_ast_from_single_name_binding(lexical_binding_inner)?)
        } else {
            return Err(get_unexpected_error(
                "build_ast_from_lexical_binding_or_variable_declaration",
                &lexical_binding_inner,
            ));
        },
    )
}

fn build_ast_from_single_name_binding(pair: Pair<Rule>) -> Result<SimpleBinding, JsError> {
    let mut inner_iter = pair.into_inner();
    let identifier = inner_iter.next().unwrap().as_str().to_string();
    Ok(if let Some(initializer) = inner_iter.next() {
        SimpleBinding{identifier, initializer:Some(build_ast_from_assignment_expression(initializer)?)}
    } else {
        SimpleBinding{identifier, initializer:None}
    })
}

fn build_ast_from_binding_pattern(pair: Pair<Rule>) -> Result<BindingPattern, JsError> {
    let mut binding_properties = vec![];
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule(){
        Rule::object_binding_pattern|Rule::object_binding_pattern__yield => {
            let mut binding_pattern_inner_iter = inner_pair.into_inner();
            for binding_property_pair in binding_pattern_inner_iter {
                binding_properties.push(build_ast_from_binding_property(binding_property_pair)?);
            }
            BindingPattern::ObjectBindingPattern(binding_properties)
        },
        Rule::array_binding_pattern|Rule::array_binding_pattern__yield => {

        },
        _=> return Err(get_unexpected_error("build_ast_from_binding_pattern", &inner_pair)),
    })
}

fn build_ast_from_binding_property(pair: Pair<Rule>) -> Result<BindingProperty, JsError> {
    let mut inner_iter = pair.into_inner();
    let inner_pair = inner_iter.next().unwrap();
    if inner_pair.as_rule()==Rule::single_name_binding || inner_pair.as_rule()==Rule::single_name_binding__yield {
        BindingProperty::SimpleBindingProperty(SimpleBinding{
            identifier:
        })
    }
}

fn build_ast_from_assignment_expression(pair: Pair<Rule>) -> Result<AssignmentExpression, JsError> {
    let mut inner_pair_iter = pair.into_inner();
    let inner_pair = inner_pair_iter.next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::arrow_function
        | Rule::arrow_function__in
        | Rule::arrow_function__yield
        | Rule::arrow_function__in_yield => build_ast_from_arrow_function(inner_pair)?,
        Rule::left_hand_side_expression | Rule::left_hand_side_expression__yield => {
            let lhs = build_ast_from_left_hand_side_expression(inner_pair)?;
            let mut next_pair = inner_pair_iter.next().unwrap();
            let operator = if next_pair.as_rule() == Rule::assignment_operator {
                let op_str = next_pair.as_str();
                next_pair = inner_pair_iter.next().unwrap();
                match op_str {
                    "*=" => AssignmentOperator::MultiplyEqual,
                    "/=" => AssignmentOperator::DivideEqual,
                    "%=" => AssignmentOperator::ModuloEqual,
                    "+=" => AssignmentOperator::AddEqual,
                    "-=" => AssignmentOperator::SubtractEqual,
                    "<<=" => AssignmentOperator::LeftShiftEqual,
                    ">>=" => AssignmentOperator::RightShiftEqual,
                    ">>>=" => AssignmentOperator::TripleRightShiftEqual,
                    "&=" => AssignmentOperator::BitwiseAndEqual,
                    "^=" => AssignmentOperator::BitwiseXorEqual,
                    "|=" => AssignmentOperator::BitwiseOrEqual,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_assignment_expression:1",
                            &next_pair,
                        ))
                    }
                }
            } else {
                AssignmentOperator::Equal
            };
            // next_pair is now assignment_expression
            let assignment_exp = Box::new(build_ast_from_assignment_expression(next_pair)?);
            AssignmentExpression::LhsRhsAssignmentExpression(LhsRhsAssignmentExpressionType {
                lhs_expression: lhs,
                assignment_operator: operator,
                rhs_expression: assignment_exp,
            })
        }
        Rule::yield_expression | Rule::yield_expression__in => {
            match inner_pair.into_inner().next() {
                Some(inner_pair) => {
                    let (assign_rule_pair, is_star) = if inner_pair.as_rule()
                        == Rule::star_assignment_expression__yield
                        || inner_pair.as_rule() == Rule::star_assignment_expression__in_yield
                    {
                        (inner_pair.into_inner().next().unwrap(), true)
                    } else {
                        (inner_pair, false)
                    };
                    let assignment_exp =
                        Box::new(build_ast_from_assignment_expression(assign_rule_pair)?);
                    AssignmentExpression::YieldExpression {
                        is_star,
                        assignment_expression: Some(assignment_exp),
                    }
                }
                None => AssignmentExpression::YieldExpression {
                    is_star: false,
                    assignment_expression: None,
                },
            }
        }
        Rule::conditional_expression
        | Rule::conditional_expression__in
        | Rule::conditional_expression__yield
        | Rule::conditional_expression__in_yield => AssignmentExpression::ConditionalExpression(
            build_ast_from_conditional_expression(inner_pair)?,
        ),
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_assignment_expression:2",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_arrow_function(pair: Pair<Rule>) -> Result<AssignmentExpression, JsError> {
    unimplemented!()
}

fn build_ast_from_left_hand_side_expression(
    pair: Pair<Rule>,
) -> Result<LhsExpressionType, JsError> {
    let inner_pair: Pair<Rule> = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::call_expression | Rule::call_expression__yield => {
            LhsExpressionType::CallExpression(build_ast_from_call_expression(inner_pair)?)
        }
        Rule::new_expression | Rule::new_expression__yield => {
            LhsExpressionType::NewExpression(build_ast_from_new_expression(inner_pair)?)
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_left_hand_side_expression",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_new_expression(pair: Pair<Rule>) -> Result<NewExpressionType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(
        if inner_pair.as_rule() == Rule::member_expression
            || inner_pair.as_rule() == Rule::member_expression__yield
        {
            NewExpressionType::Member(build_ast_from_member_expression(inner_pair)?)
        } else {
            NewExpressionType::New(Box::new(build_ast_from_new_expression(inner_pair)?))
        },
    )
}

fn build_ast_from_conditional_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut pair_iter = pair.into_inner();
    let logical_or_pair = pair_iter.next().unwrap();
    let or_node = build_ast_from_logical_or_expression(logical_or_pair)?;
    if let Some(inner_pair) = pair_iter.next() {
        let truthy = build_ast_from_assignment_expression(inner_pair)?;
        let falsy = build_ast_from_assignment_expression(pair_iter.next().unwrap())?;
        Ok(LogicAndMathExpression::ConditionOperatorExpression {
            condition: Box::new(or_node),
            if_true: Box::new(truthy),
            if_false: Box::new(falsy),
        })
    } else {
        Ok(or_node)
    }
}

fn build_ast_from_logical_or_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(build_ast_from_logical_and_expression(inner_pair)?);
    }
    if operands.len() == 1 {
        Ok(operands.pop().unwrap())
    } else {
        Ok(LogicAndMathExpression::LogicalOrExpression(Box::new(
            operands,
        )))
    }
}

fn build_ast_from_logical_and_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(build_ast_from_bitwise_or_expression(inner_pair)?);
    }
    if operands.len() == 1 {
        Ok(operands.pop().unwrap())
    } else {
        Ok(LogicAndMathExpression::LogicalAndExpression(Box::new(
            operands,
        )))
    }
}

fn build_ast_from_bitwise_or_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(build_ast_from_bitwise_xor_expression(inner_pair)?);
    }
    if operands.len() == 1 {
        Ok(operands.pop().unwrap())
    } else {
        Ok(LogicAndMathExpression::BitwiseOrExpression(Box::new(
            operands,
        )))
    }
}

fn build_ast_from_bitwise_xor_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(build_ast_from_bitwise_and_expression(inner_pair)?);
    }
    if operands.len() == 1 {
        Ok(operands.pop().unwrap())
    } else {
        Ok(LogicAndMathExpression::BitwiseXorExpression(Box::new(
            operands,
        )))
    }
}

fn build_ast_from_bitwise_and_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(build_ast_from_equality_expression(inner_pair)?);
    }
    if operands.len() == 1 {
        Ok(operands.pop().unwrap())
    } else {
        Ok(LogicAndMathExpression::BitwiseAndExpression(Box::new(
            operands,
        )))
    }
}

fn build_ast_from_equality_expression(pair: Pair<Rule>) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    let mut operators = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_relational_expression(pair_iter.next().unwrap())?;
    operands.push(first_one);
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            let inner_pair_1 = inner_pair.into_inner().next().unwrap();
            operators.push(match inner_pair_1.as_str() {
                "===" => LogicalEqualityOperator::StrictlyEqual,
                "!==" => LogicalEqualityOperator::StrictlyUnequal,
                "==" => LogicalEqualityOperator::LooselyEqual,
                "!=" => LogicalEqualityOperator::LooselyUnequal,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_equality_expression",
                        &inner_pair_1,
                    ))
                }
            });
            operands.push(build_ast_from_relational_expression(
                pair_iter.next().unwrap(),
            )?);
        } else {
            break;
        }
    }
    if operands.len() > 1 {
        Ok(LogicAndMathExpression::LogicalEqualityExpression {
            operands: Box::new(operands),
            operators,
        })
    } else {
        Ok(operands.pop().unwrap())
    }
}

fn build_ast_from_relational_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    let mut operators = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_shift_expression(pair_iter.next().unwrap())?;
    operands.push(first_one);
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            let inner_pair_1 = inner_pair.into_inner().next().unwrap();
            operators.push(match inner_pair_1.as_str() {
                "<=" => RelationalOperator::LessThanEqual,
                ">=" => RelationalOperator::GreaterThanEqual,
                "<" => RelationalOperator::LessThan,
                ">" => RelationalOperator::GreaterThan,
                "instanceof" => RelationalOperator::InstanceOf,
                "in" => RelationalOperator::In,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_relational_expression",
                        &inner_pair_1,
                    ))
                }
            });
            operands.push(build_ast_from_shift_expression(pair_iter.next().unwrap())?);
        } else {
            break;
        }
    }
    if operands.len() > 1 {
        Ok(LogicAndMathExpression::RelationalExpression {
            operands: Box::new(operands),
            operators,
        })
    } else {
        Ok(operands.pop().unwrap())
    }
}

fn build_ast_from_shift_expression(pair: Pair<Rule>) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    let mut operators = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_additive_expression(pair_iter.next().unwrap())?;
    operands.push(first_one);
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            let inner_pair_1 = inner_pair.into_inner().next().unwrap();
            operators.push(match inner_pair_1.as_str() {
                "<<" => ShiftOperator::LeftShift,
                ">>>" => ShiftOperator::UnsignedRightShift,
                ">>" => ShiftOperator::RightShift,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_shift_expression",
                        &inner_pair_1,
                    ))
                }
            });
            operands.push(build_ast_from_additive_expression(
                pair_iter.next().unwrap(),
            )?);
        } else {
            break;
        }
    }
    if operands.len() > 1 {
        Ok(LogicAndMathExpression::ShiftExpression {
            operands: Box::new(operands),
            operators,
        })
    } else {
        Ok(operands.pop().unwrap())
    }
}

fn build_ast_from_additive_expression(pair: Pair<Rule>) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    let mut operators = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_multiplicative_expression(pair_iter.next().unwrap())?;
    operands.push(first_one);
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            let inner_pair_1 = inner_pair.into_inner().next().unwrap();
            operators.push(match inner_pair_1.as_str() {
                "+" => AdditiveOperator::Add,
                "-" => AdditiveOperator::Subtract,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_additive_expression",
                        &inner_pair_1,
                    ))
                }
            });
            operands.push(build_ast_from_multiplicative_expression(
                pair_iter.next().unwrap(),
            )?);
        } else {
            break;
        }
    }
    if operands.len() > 1 {
        Ok(LogicAndMathExpression::AdditiveExpression {
            operands: Box::new(operands),
            operators,
        })
    } else {
        Ok(operands.pop().unwrap())
    }
}

fn build_ast_from_multiplicative_expression(
    pair: Pair<Rule>,
) -> Result<LogicAndMathExpression, JsError> {
    let mut operands = vec![];
    let mut operators = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_unary_expression(pair_iter.next().unwrap())?;
    operands.push(first_one);
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            let inner_pair_1 = inner_pair.into_inner().next().unwrap();
            operators.push(match inner_pair_1.as_str() {
                "*" => MultiplicativeOperator::Multiply,
                "/" => MultiplicativeOperator::Divide,
                "%" => MultiplicativeOperator::Modulo,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_multiplicative_expression",
                        &inner_pair_1,
                    ))
                }
            });
            operands.push(build_ast_from_unary_expression(pair_iter.next().unwrap())?);
        } else {
            break;
        }
    }
    if operands.len() > 1 {
        Ok(LogicAndMathExpression::MultiplicativeExpression {
            operands: Box::new(operands),
            operators,
        })
    } else {
        Ok(operands.pop().unwrap())
    }
}

fn build_ast_from_unary_expression(pair: Pair<Rule>) -> Result<LogicAndMathExpression, JsError> {
    let mut pair_iter = pair.into_inner();
    let first_pair = pair_iter.next().unwrap();

    Ok(
        if first_pair.as_rule() == Rule::postfix_expression
            || first_pair.as_rule() == Rule::postfix_expression__yield
        {
            build_ast_from_postfix_expression(first_pair)?
        } else {
            let first_inner_pair = first_pair.into_inner().next().unwrap();
            LogicAndMathExpression::UnaryExpression {
                operator: match first_inner_pair.as_str() {
                    "delete" => UnaryOperator::Delete,
                    "void" => UnaryOperator::Void,
                    "typeof" => UnaryOperator::Typeof,
                    "++" => UnaryOperator::PrefixIncrement,
                    "--" => UnaryOperator::PrefixDecrement,
                    "+" => UnaryOperator::Plus,
                    "-" => UnaryOperator::Minus,
                    "~" => UnaryOperator::BitwiseNot,
                    "!" => UnaryOperator::LogicalNot,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_unary_expression",
                            &first_inner_pair,
                        ))
                    }
                },
                operand: Box::new(build_ast_from_unary_expression(pair_iter.next().unwrap())?),
            }
        },
    )
}

fn build_ast_from_postfix_expression(pair: Pair<Rule>) -> Result<LogicAndMathExpression, JsError> {
    let mut pair_iter = pair.into_inner();
    let lhs = LogicAndMathExpression::LhsExpressionWrapper(
        build_ast_from_left_hand_side_expression(pair_iter.next().unwrap())?,
    );
    if let Some(op_pair) = pair_iter.next() {
        Ok(LogicAndMathExpression::PostfixExpression {
            operand: Box::new(lhs),
            operator: match op_pair.as_str() {
                "++" => PostfixOperator::PostfixIncrement,
                "--" => PostfixOperator::PostfixDecrement,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_postfix_expression",
                        &op_pair,
                    ))
                }
            },
        })
    } else {
        Ok(lhs)
    }
}

fn build_ast_from_call_expression(pair: Pair<Rule>) -> Result<CallExpressionType, JsError> {
    let mut pair_iter = pair.into_inner();
    let pair = pair_iter.next().unwrap();
    let mut obj = if pair.as_rule() == Rule::super_call || pair.as_rule() == Rule::super_call__yield
    {
        CallExpressionType::FunctionCallExpression(FunctionCallExpressionType::CallOnSuper {
            arguments: build_ast_from_arguments(pair.into_inner().next().unwrap())?,
        })
    } else {
        let arguments_pair = pair_iter.next().unwrap();
        CallExpressionType::FunctionCallExpression(
            FunctionCallExpressionType::CallOnMemberExpression {
                function_obj: Box::new(build_ast_from_member_expression(pair)?),
                arguments: build_ast_from_arguments(arguments_pair)?,
            },
        )
    };
    for pair in pair_iter {
        obj = match pair.as_rule() {
            Rule::expression__in_yield | Rule::expression__in => {
                CallExpressionType::ComplexPropertyAccessExpression(
                    ComplexPropertyAccessExpressionData {
                        object: Box::new(obj),
                        property_expression: build_ast_from_expression(pair)?,
                    },
                )
            }
            Rule::identifier_name => CallExpressionType::SimplePropertyAccessExpression(
                SimplePropertyAccessExpressionData {
                    object: Box::new(obj),
                    property_name: get_identifier_name(pair),
                },
            ),
            Rule::template_literal | Rule::template_literal__yield => unimplemented!(),
            Rule::arguments | Rule::arguments__yield => CallExpressionType::FunctionCallExpression(
                FunctionCallExpressionType::CallOnCallExpressionType {
                    function_obj: Box::new(obj),
                    arguments: build_ast_from_arguments(pair)?,
                },
            ),
            _ => {
                return Err(get_unexpected_error(
                    "build_ast_from_call_expression",
                    &pair,
                ))
            }
        };
    }
    Ok(obj)
}

fn get_identifier_name(pair: Pair<Rule>) -> String {
    pair.as_str().trim().to_string()
}

fn build_ast_from_expression(pair: Pair<Rule>) -> Result<Expression, JsError> {
    let mut node_children = vec![];
    for inner_pair in pair.into_inner() {
        node_children.push(build_ast_from_assignment_expression(inner_pair)?);
    }
    Ok(Expression {
        assignment_expressions: node_children,
    })
}

fn build_ast_from_arguments(pair: Pair<Rule>) -> Result<ArgumentList, JsError> {
    let mut arguments = vec![];
    if let Some(argument_list_pair) = pair.into_inner().next() {
        for inner_pair in argument_list_pair.into_inner() {
            arguments.push(
                if inner_pair.as_rule() == Rule::rest_assignment_expression__in
                    || inner_pair.as_rule() == Rule::rest_assignment_expression__in_yield
                {
                    Argument::Spread(build_ast_from_assignment_expression(
                        inner_pair.into_inner().next().unwrap(),
                    )?)
                } else {
                    Argument::Regular(build_ast_from_assignment_expression(inner_pair)?)
                },
            );
        }
    }
    Ok(ArgumentList { arguments })
}

fn build_ast_from_member_expression(pair: Pair<Rule>) -> Result<MemberExpression, JsError> {
    let mut pair_iter = pair.into_inner();
    let pair_1 = pair_iter.next().unwrap();
    Ok(
        if pair_1.as_rule() == Rule::new_member_expression
            || pair_1.as_rule() == Rule::new_member_expression__yield
        {
            let member_expression_pair = pair_1.into_inner().next().unwrap();
            let arguments_pair = pair_iter.next().unwrap();
            MemberExpression::NewObjectFromFunction {
                function_obj: Box::new(build_ast_from_member_expression(member_expression_pair)?),
                arguments: build_ast_from_arguments(arguments_pair)?,
            }
        } else {
            let mut obj = match pair_1.as_rule() {
                Rule::super_property | Rule::super_property__yield => {
                    let super_pair = pair_1.into_inner().next().unwrap();
                    if super_pair.as_rule() == Rule::identifier_name {
                        MemberExpression::SuperSimplePropertyAccessExpression {
                            property_name: get_identifier_name(super_pair),
                        }
                    } else {
                        MemberExpression::SuperComplexPropertyAccessExpression {
                            property_expression: build_ast_from_expression(super_pair)?,
                        }
                    }
                }
                Rule::meta_property => MemberExpression::NewDotTargetExpression,
                Rule::primary_expression | Rule::primary_expression__yield => {
                    MemberExpression::PrimaryExpression(build_ast_from_primary_expression(pair_1)?)
                }
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_member_expression:1",
                        &pair_1,
                    ))
                }
            };
            for pair in pair_iter {
                obj = match pair.as_rule() {
                    Rule::expression__in_yield | Rule::expression__in => {
                        MemberExpression::MemberComplexPropertyAccessExpression(
                            ComplexPropertyAccessExpressionData {
                                object: Box::new(obj),
                                property_expression: build_ast_from_expression(pair)?,
                            },
                        )
                    }
                    Rule::identifier_name => {
                        MemberExpression::MemberSimplePropertyAccessExpression(
                            SimplePropertyAccessExpressionData {
                                object: Box::new(obj),
                                property_name: get_identifier_name(pair),
                            },
                        )
                    }
                    Rule::template_literal | Rule::template_literal__yield => unimplemented!(),
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_member_expression:2",
                            &pair,
                        ))
                    }
                };
            }
            obj
        },
    )
}

fn build_ast_from_primary_expression(pair: Pair<Rule>) -> Result<PrimaryExpressionType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::identifier_reference | Rule::identifier_reference__yield => {
            build_ast_from_identifier_reference(inner_pair)?
        }
        Rule::literal => PrimaryExpressionType::Literal(build_ast_from_literal(inner_pair)?),
        Rule::this_exp => PrimaryExpressionType::ThisKeyword,
        Rule::array_literal | Rule::array_literal__yield => {
            build_ast_from_array_literal(inner_pair)?
        }
        Rule::object_literal | Rule::object_literal__yield => unimplemented!(),
        Rule::generator_expression => unimplemented!(),
        Rule::function_expression => unimplemented!(),
        Rule::class_expression | Rule::class_expression__yield => unimplemented!(),
        Rule::regular_expression_literal => unimplemented!(),
        Rule::template_literal | Rule::template_literal__yield => unimplemented!(),
        Rule::cover_parenthesized_expression_and_arrow_parameter_list
        | Rule::cover_parenthesized_expression_and_arrow_parameter_list__yield => {
            panic!("Use-case not clear. Not implemented right now!");
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_primary_expression",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_literal(pair: Pair<Rule>) -> Result<LiteralType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::null_literal => LiteralType::NullLiteral,
        Rule::numeric_literal => {
            LiteralType::NumericLiteral(build_ast_from_numeric_literal(inner_pair)?)
        }
        Rule::string_literal => build_ast_from_string_literal(inner_pair)?,
        Rule::boolean_literal => {
            let bool = inner_pair.as_str();
            LiteralType::BoolLiteral(bool == "true")
        }
        _ => return Err(get_unexpected_error("build_ast_from_literal", &inner_pair)),
    })
}

fn build_ast_from_string_literal(pair: Pair<Rule>) -> Result<LiteralType, JsError> {
    let s = pair.as_str();
    Ok(LiteralType::StringLiteral(String::from(&s[1..s.len() - 1])))
}

fn build_ast_from_numeric_literal(pair: Pair<Rule>) -> Result<NumericType, JsError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::binary_integer_literal => NumericType::IntegerLiteral(
            isize::from_str_radix(&inner_pair.as_str()[2..], 2).unwrap() as i32,
        ),
        Rule::octal_integer_literal => NumericType::IntegerLiteral(
            isize::from_str_radix(&inner_pair.as_str()[2..], 8).unwrap() as i32,
        ),
        Rule::hex_integer_literal => NumericType::IntegerLiteral(
            isize::from_str_radix(&inner_pair.as_str()[2..], 16).unwrap() as i32,
        ),
        Rule::decimal_literal => {
            let mut num: f64 = 0.0;
            let mut is_float = false;
            for decimal_pair in inner_pair.into_inner() {
                num = match decimal_pair.as_rule() {
                    Rule::decimal_integer_literal => {
                        isize::from_str_radix(decimal_pair.as_str(), 10).unwrap() as f64
                    }
                    Rule::decimal_digits => {
                        is_float = true;
                        let d = decimal_pair.as_str();
                        num + isize::from_str_radix(d, 10).unwrap() as f64
                            / 10_f64.powf(d.len() as f64)
                    }
                    Rule::exponent_part => {
                        num * 10_f64.powf(
                            isize::from_str_radix(&decimal_pair.as_str()[1..], 10).unwrap() as f64,
                        )
                    }
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_numeric_literal:1",
                            &decimal_pair,
                        ))
                    }
                }
            }
            if !is_float {
                NumericType::IntegerLiteral(num as i32)
            } else {
                NumericType::FloatLiteral(num)
            }
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_numeric_literal:2",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_array_literal(pair: Pair<Rule>) -> Result<PrimaryExpressionType, JsError> {
    let mut arguments = vec![];
    let mut index: u16 = 0;
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::elision => {
                index += inner_pair.as_str().matches(',').count() as u16;
            }
            Rule::assignment_expression__in | Rule::assignment_expression__in_yield => {
                arguments.push(ArrayItem {
                    index,
                    item: Argument::Regular(build_ast_from_assignment_expression(inner_pair)?),
                });
                index += 1;
            }
            Rule::spread_element | Rule::spread_element__yield => {
                arguments.push(ArrayItem {
                    index,
                    item: Argument::Spread(build_ast_from_assignment_expression(
                        inner_pair.into_inner().next().unwrap(),
                    )?),
                });
                index += 1;
            }
            _ => {
                return Err(get_unexpected_error(
                    "build_ast_from_array_literal",
                    &inner_pair,
                ))
            }
        }
    }
    Ok(PrimaryExpressionType::ArrayLiteral {
        items: arguments,
        length: index,
    })
}

fn build_ast_from_identifier_reference(pair: Pair<Rule>) -> Result<PrimaryExpressionType, JsError> {
    let id = pair.as_str();
    Ok(if id == "yield" {
        PrimaryExpressionType::YieldKeyword
    } else {
        PrimaryExpressionType::IdentifierReference(id.to_string())
    })
}
