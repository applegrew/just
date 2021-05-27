use crate::parser::ast::{
    AssignmentOperator, BinaryOperator, BlockStatementData, DeclarationType,
    ExpressionOrSpreadElement, ExpressionOrSuper, ExpressionPatternType, ExpressionType,
    ExtendedNumberLiteralType, ForIteratorData, FunctionBodyData, FunctionData, HasMeta,
    IdentifierData, JsError, JsErrorType, LiteralData, LiteralType, LogicalOperator,
    MemberExpressionType, Meta, NumberLiteralType, PatternOrExpression, PatternType, ProgramData,
    StatementType, UnaryOperator, UpdateOperator, VariableDeclarationData, VariableDeclarationKind,
    VariableDeclarationOrExpression, VariableDeclaratorData,
};
use crate::parser::util::TAB_WIDTH;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::rc::Rc;
use std::time::Instant;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
pub struct JsParser;

type JsRuleError = JsError<Rule>;

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
            Err(err) => {
                return Err(format!("Parse error due to {:?}", err));
            }
        }
        Ok(tree.join("\n"))
    }

    pub fn parse_to_ast(script: Rc<String>) -> Result<ProgramData, JsRuleError> {
        let result = Self::parse(Rule::script, &script);
        match result {
            Ok(pairs) => build_ast_from_script(pairs, &script),
            Err(err) => {
                return Err(JsRuleError {
                    kind: JsErrorType::ParserValidation(err.clone()),
                    message: format!("Parse error due to \n{}", err),
                });
            }
        }
    }

    pub fn parse_to_ast_from_str(script: &str) -> Result<ProgramData, JsRuleError> {
        Self::parse_to_ast(Rc::new(script.to_string()))
    }

    pub fn parse_to_ast_formatted_string(script: &str) -> Result<String, JsRuleError> {
        let result = Self::parse_to_ast_from_str(script)?;
        Ok(result.to_formatted_string(script))
    }

    pub fn parse_numeric_string(
        s: &String,
        is_error_on_empty: bool,
    ) -> Result<ExtendedNumberLiteralType, JsRuleError> {
        let result = Self::parse(Rule::string_numeric_literal, s);
        Ok(match result {
            Ok(mut pairs) => {
                if let Some(pair) = pairs.next() {
                    match pair.as_rule() {
                        Rule::str_numeric_literal => build_ast_from_str_numeric_literal(pair)?,
                        _ => return Err(get_unexpected_error("parse_numeric_string", &pair)),
                    }
                } else {
                    if is_error_on_empty {
                        return Err(JsRuleError {
                            kind: JsErrorType::ParserGeneralError,
                            message: "Got empty string".to_string(),
                        });
                    } else {
                        ExtendedNumberLiteralType::Std(NumberLiteralType::IntegerLiteral(0))
                    }
                }
            }
            Err(err) => {
                return Err(JsRuleError {
                    kind: JsErrorType::ParserValidation(err.clone()),
                    message: format!("Parse error due to \n{}", err),
                });
            }
        })
    }
}

fn get_unexpected_error(src: &'static str, pair: &Pair<Rule>) -> JsRuleError {
    let message = format!("Unexpected state reached in the parser at \"{:?}\". This indicates internal logic error in the parser.", pair.as_rule());
    JsRuleError {
        message,
        kind: JsErrorType::Unexpected(src),
    }
}

fn get_validation_error(error: String, pair: &Pair<Rule>, script: &Rc<String>) -> JsRuleError {
    let message = format!("Parsing error encountered: {}", error);
    JsRuleError {
        message,
        kind: JsErrorType::AstBuilderValidation(get_meta(pair, script)),
    }
}

fn get_meta(pair: &Pair<Rule>, script: &Rc<String>) -> Meta {
    Meta {
        start_index: pair.as_span().start(),
        end_index: pair.as_span().end(),
        script: script.clone(),
    }
}

fn build_ast_from_script(
    pairs: Pairs<Rule>,
    script: &Rc<String>,
) -> Result<ProgramData, JsRuleError> {
    let mut instructions = vec![];
    let mut end: usize = 0;
    for pair in pairs {
        let meta = get_meta(&pair, script);
        if meta.end_index > end {
            end = meta.end_index;
        }
        match pair.as_rule() {
            Rule::declaration => instructions.push(StatementType::DeclarationStatement(
                build_ast_from_declaration(pair, script)?,
            )),
            Rule::EOI => { /* Do nothing */ }
            Rule::statement => {
                // Then this should be one of the statements
                instructions.push(build_ast_from_statement(pair, script)?)
            }
            _ => return Err(get_unexpected_error("build_ast_from_script", &pair)),
        };
    }
    Ok(ProgramData {
        meta: Meta {
            start_index: 0,
            end_index: end,
            script: script.clone(),
        },
        body: instructions,
    })
}

fn build_ast_from_declaration(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<DeclarationType, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::hoistable_declaration | Rule::hoistable_declaration__yield => {
            build_ast_from_hoistable_declaration(inner_pair, script)?
        }
        Rule::class_declaration | Rule::class_declaration__yield => {
            unimplemented!()
        }
        Rule::lexical_declaration__in | Rule::lexical_declaration__in_yield => {
            DeclarationType::VariableDeclaration(build_ast_from_lexical_declaration(
                inner_pair, script,
            )?)
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_declaration",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_lexical_declaration(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<VariableDeclarationData, JsRuleError> {
    unimplemented!()
}

fn build_ast_from_hoistable_declaration(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<DeclarationType, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::generator_declaration | Rule::generator_declaration__yield => {
            DeclarationType::FunctionDeclaration(build_ast_from_generator_declaration(
                inner_pair, script,
            )?)
        }
        Rule::function_declaration | Rule::function_declaration__yield => {
            DeclarationType::FunctionDeclaration(
                build_ast_from_function_declaration_or_function_expression(inner_pair, script)?,
            )
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_hoistable_declaration",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_generator_declaration(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<FunctionData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut pair_iter = pair.into_inner();
    let f_name = get_identifier_data(pair_iter.next().unwrap(), script);
    let formal_parameters = pair_iter.next().unwrap();
    let args = build_ast_from_formal_parameters(formal_parameters, script)?;
    // We first have generator_body then function_body__yield in it
    let function_body_pair = pair_iter.next().unwrap().into_inner().next().unwrap();
    let body = build_ast_from_function_body(function_body_pair, script)?;
    Ok(FunctionData {
        meta,
        id: Some(f_name),
        body,
        params: args,
        generator: true,
    })
}

fn build_ast_from_function_declaration_or_function_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<FunctionData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut pair_iter = pair.into_inner();
    let first_pair = pair_iter.next().unwrap();
    let (f_name, formal_parameters) = if first_pair.as_rule() == Rule::binding_identifier {
        (
            Some(get_identifier_data(first_pair, script)),
            pair_iter.next().unwrap(),
        )
    } else {
        (None, first_pair)
    };
    let args = build_ast_from_formal_parameters(formal_parameters, script)?;
    let function_body_pair = pair_iter.next().unwrap();
    let body = build_ast_from_function_body(function_body_pair, script)?;
    Ok(FunctionData {
        meta,
        id: f_name,
        body,
        params: args,
        generator: false,
    })
}

fn build_ast_from_formal_parameters(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Vec<Box<PatternType>>, JsRuleError> {
    let mut args: Vec<Box<PatternType>> = vec![];
    for param in pair.into_inner() {
        let meta = get_meta(&param, script);
        args.push(match param.as_rule() {
            Rule::function_rest_parameter | Rule::function_rest_parameter__yield => {
                let binding_rest_element = param.into_inner().next().unwrap();
                let binding_identifier = binding_rest_element.into_inner().next().unwrap();
                Box::new(PatternType::RestElement {
                    meta,
                    argument: Box::new(
                        ExpressionPatternType::Identifier(get_identifier_data(
                            binding_identifier,
                            script,
                        ))
                        .convert_to_pattern(),
                    ),
                })
            }
            Rule::formal_parameter | Rule::formal_parameter__yield => {
                build_ast_from_binding_element(param, script)?
            }
            _ => {
                return Err(get_unexpected_error(
                    "build_ast_from_formal_parameters",
                    &param,
                ))
            }
        });
    }
    Ok(args)
}

fn build_ast_from_binding_element(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<PatternType>, JsRuleError> {
    let binding_element = pair.into_inner().next().unwrap();
    let mut binding_element_inner_iter = binding_element.into_inner();
    let binding_element_inner = binding_element_inner_iter.next().unwrap();
    Ok(
        if binding_element_inner.as_rule() == Rule::single_name_binding {
            let meta = get_meta(&binding_element_inner, script);
            let mut single_name_binding_iter = binding_element_inner.into_inner();
            let binding_identifier = ExpressionPatternType::Identifier(get_identifier_data(
                single_name_binding_iter.next().unwrap(),
                script,
            ));
            if let Some(initializer) = single_name_binding_iter.next() {
                Box::new(PatternType::AssignmentPattern {
                    meta,
                    left: Box::new(binding_identifier.convert_to_pattern()),
                    right: build_ast_from_assignment_expression(
                        initializer.into_inner().next().unwrap(),
                        script,
                    )?,
                })
            } else {
                Box::new(binding_identifier.convert_to_pattern())
            }
        } else if binding_element_inner.as_rule() == Rule::binding_pattern {
            unimplemented!();
        } else {
            return Err(get_unexpected_error(
                "build_ast_from_binding_element",
                &binding_element_inner,
            ));
        },
    )
}

fn build_ast_from_function_body(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<FunctionBodyData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut statements = vec![];
    for pair in pair.into_inner() {
        statements.push(match pair.as_rule() {
            Rule::declaration | Rule::declaration__yield => {
                StatementType::DeclarationStatement(build_ast_from_declaration(pair, script)?)
            }
            Rule::statement
            | Rule::statement__yield
            | Rule::statement__return
            | Rule::statement__yield_return => build_ast_from_statement(pair, script)?,
            _ => return Err(get_unexpected_error("build_ast_from_function_body", &pair)),
        });
    }
    Ok(FunctionBodyData {
        meta,
        body: Box::new(statements),
    })
}

fn build_ast_from_statement(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    let meta = get_meta(&inner_pair, script);
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
            build_ast_from_variable_statement(inner_pair, script)?
        }
        Rule::breakable_statement
        | Rule::breakable_statement__yield
        | Rule::breakable_statement__return
        | Rule::breakable_statement__yield_return => {
            build_ast_from_breakable_statement(inner_pair, script)?
        }
        Rule::block_statement
        | Rule::block_statement__yield
        | Rule::block_statement__return
        | Rule::block_statement__yield_return => StatementType::BlockStatement(
            build_ast_from_block(inner_pair.into_inner().next().unwrap(), script)?,
        ),
        Rule::expression_statement | Rule::expression_statement__yield => {
            StatementType::ExpressionStatement {
                meta,
                expression: build_ast_from_expression(
                    inner_pair.into_inner().next().unwrap(),
                    script,
                )?,
            }
        }
        Rule::labelled_statement
        | Rule::labelled_statement__yield
        | Rule::labelled_statement__return
        | Rule::labelled_statement__yield_return => unimplemented!(),
        Rule::empty_statement => unimplemented!(),
        Rule::return_statement | Rule::return_statement__yield => {
            build_ast_from_return_statement(inner_pair, script)?
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_statement",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_block(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<BlockStatementData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut declarations = vec![];
    for inner_pair in pair.into_inner() {
        declarations.push(match inner_pair.as_rule() {
            Rule::declaration | Rule::declaration__yield => {
                StatementType::DeclarationStatement(build_ast_from_declaration(inner_pair, script)?)
            }
            Rule::statement
            | Rule::statement__yield
            | Rule::statement__return
            | Rule::statement__yield_return => build_ast_from_statement(inner_pair, script)?,
            _ => return Err(get_unexpected_error("build_ast_from_block", &inner_pair)),
        });
    }
    Ok(BlockStatementData {
        meta,
        body: Box::new(declarations),
    })
}

fn build_ast_from_breakable_statement(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(
        if inner_pair.as_rule() == Rule::iteration_statement
            || inner_pair.as_rule() == Rule::iteration_statement__yield
            || inner_pair.as_rule() == Rule::iteration_statement__return
            || inner_pair.as_rule() == Rule::iteration_statement__yield_return
        {
            build_ast_from_iteration_statement(inner_pair, script)?
        } else {
            build_ast_from_switch_statement(inner_pair, script)?
        },
    )
}

fn build_ast_from_iteration_statement(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let tag = pair.as_str().splitn(2, ' ').next().unwrap();
    Ok(match tag {
        "do" => build_ast_for_breakable_statement_do(pair, script)?,
        "while" => build_ast_for_breakable_statement_while(pair, script)?,
        "for" => build_ast_for_breakable_statement_for(pair, script)?,
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_iteration_statement",
                &pair,
            ))
        }
    })
}

fn build_ast_for_breakable_statement_do(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_iter = pair.into_inner();
    let statement_pair = inner_iter.next().unwrap();
    let test_expression_pair = inner_iter.next().unwrap();
    Ok(StatementType::DoWhileStatement {
        meta,
        test: build_ast_from_expression(test_expression_pair, script)?,
        body: Box::new(build_ast_from_statement(statement_pair, script)?),
    })
}

fn build_ast_for_breakable_statement_while(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_iter = pair.into_inner();
    let test_expression_pair = inner_iter.next().unwrap();
    let statement_pair = inner_iter.next().unwrap();
    Ok(StatementType::WhileStatement {
        meta,
        test: build_ast_from_expression(test_expression_pair, script)?,
        body: Box::new(build_ast_from_statement(statement_pair, script)?),
    })
}

fn build_ast_for_breakable_statement_for(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_iter = pair.into_inner();
    let first_pair = inner_iter.next().unwrap();
    Ok(match first_pair.as_rule() {
        Rule::left_hand_side_expression
        | Rule::left_hand_side_expression__yield
        | Rule::for_binding
        | Rule::for_binding__yield
        | Rule::for_declaration
        | Rule::for_declaration__yield => {
            let in_of_left = match first_pair.as_rule() {
                Rule::left_hand_side_expression | Rule::left_hand_side_expression__yield => {
                    VariableDeclarationOrExpression::Expression(
                        build_ast_from_left_hand_side_expression(first_pair, script)?,
                    )
                }
                Rule::for_binding | Rule::for_binding__yield => {
                    let meta = get_meta(&first_pair, script);
                    let meta2 = meta.clone();
                    VariableDeclarationOrExpression::VariableDeclaration(VariableDeclarationData {
                        meta,
                        declarations: vec![VariableDeclaratorData {
                            meta: meta2,
                            id: build_ast_from_for_binding(first_pair, script)?,
                            init: None,
                        }],
                        kind: VariableDeclarationKind::Var,
                    })
                }
                Rule::for_declaration | Rule::for_declaration__yield => {
                    build_ast_from_for_declaration(first_pair, script)?
                }
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_for_breakable_statement_for:1",
                        &first_pair,
                    ))
                }
            };
            let second_pair = inner_iter.next().unwrap();
            let (in_of_right, is_for_of) = match second_pair.as_rule() {
                Rule::assignment_expression__in | Rule::assignment_expression__in_yield => (
                    build_ast_from_assignment_expression(second_pair, script)?,
                    true,
                ),
                _ => (build_ast_from_expression(second_pair, script)?, false),
            };
            let statement = build_ast_from_statement(inner_iter.next().unwrap(), script)?;
            let node = ForIteratorData {
                meta,
                left: in_of_left,
                right: in_of_right,
                body: Box::new(statement),
            };
            if is_for_of {
                StatementType::ForOfStatement(node)
            } else {
                StatementType::ForInStatement(node)
            }
        }
        _ => {
            let init = match first_pair.as_rule() {
                Rule::lexical_declaration | Rule::lexical_declaration__yield => {
                    //Lexical Declaration rule ends with smart semicolon which is too flexible. We need to ensure it is semi-colon and nothing else.
                    let last_char = first_pair.as_str().trim_end().chars().last().unwrap();
                    if last_char != ';' {
                        return Err(get_validation_error(
                            format!(
                                "Was expecting semi-colon at the end, but got '{}'.",
                                last_char
                            ),
                            &first_pair,
                            script,
                        ));
                    } else {
                        Some(VariableDeclarationOrExpression::VariableDeclaration(
                            build_ast_from_lexical_declaration(first_pair, script)?,
                        ))
                    }
                }
                Rule::variable_declaration_list | Rule::variable_declaration_list__yield => {
                    let meta = get_meta(&first_pair, script);
                    let declarations =
                        build_ast_from_variable_declaration_list(first_pair, script)?;
                    if declarations.is_empty() {
                        None
                    } else {
                        Some(VariableDeclarationOrExpression::VariableDeclaration(
                            VariableDeclarationData {
                                meta,
                                declarations,
                                kind: VariableDeclarationKind::Var,
                            },
                        ))
                    }
                }
                Rule::init_expression | Rule::init_expression__yield => {
                    if let Some(inner_pair) = first_pair.into_inner().next() {
                        Some(VariableDeclarationOrExpression::Expression(
                            build_ast_from_expression(inner_pair, script)?,
                        ))
                    } else {
                        None
                    }
                }
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_for_breakable_statement_for:2",
                        &first_pair,
                    ))
                }
            };
            let test_pair = inner_iter.next().unwrap();
            let test = if let Some(test_expression_pair) = test_pair.into_inner().next() {
                Some(build_ast_from_expression(test_expression_pair, script)?)
            } else {
                None
            };
            let update_pair = inner_iter.next().unwrap();
            let update = if let Some(update_expression_pair) = update_pair.into_inner().next() {
                Some(build_ast_from_expression(update_expression_pair, script)?)
            } else {
                None
            };
            StatementType::ForStatement {
                meta,
                init,
                test,
                update,
                body: Box::new(build_ast_from_statement(
                    inner_iter.next().unwrap(),
                    script,
                )?),
            }
        }
    })
}

fn build_ast_from_binding_pattern(pair: Pair<Rule>) -> Result<Box<PatternType>, JsRuleError> {
    unimplemented!()
}

fn build_ast_from_for_binding(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<PatternType>, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::binding_identifier | Rule::binding_identifier__yield => {
            let id: Box<PatternType> = Box::new(
                ExpressionPatternType::Identifier(get_identifier_data(inner_pair, script))
                    .convert_to_pattern(),
            );
            id
        }
        Rule::binding_pattern | Rule::binding_pattern__yield => {
            build_ast_from_binding_pattern(inner_pair)?
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_for_binding",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_for_declaration(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<VariableDeclarationOrExpression, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_iter = pair.into_inner();
    let let_or_const_pair = inner_iter.next().unwrap();
    let for_binding_pair = inner_iter.next().unwrap();
    let meta2 = meta.clone();
    Ok(VariableDeclarationOrExpression::VariableDeclaration(
        VariableDeclarationData {
            meta,
            declarations: vec![VariableDeclaratorData {
                meta: meta2,
                id: build_ast_from_for_binding(for_binding_pair, script)?,
                init: None,
            }],
            kind: get_let_or_const(let_or_const_pair)?,
        },
    ))
}

fn get_let_or_const(let_or_const_pair: Pair<Rule>) -> Result<VariableDeclarationKind, JsRuleError> {
    Ok(match let_or_const_pair.as_str() {
        "let" => VariableDeclarationKind::Let,
        "const" => VariableDeclarationKind::Const,
        _ => return Err(get_unexpected_error("get_let_or_const", &let_or_const_pair)),
    })
}

fn build_ast_from_switch_statement(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    unimplemented!()
}

fn build_ast_from_return_statement(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let meta = get_meta(&pair, script);
    let inner_pair = pair.into_inner().next().unwrap();
    let argument: Option<Box<ExpressionType>> = if inner_pair.as_rule() == Rule::expression__in {
        Some(build_ast_from_expression(inner_pair, script)?)
    } else if inner_pair.as_rule() == Rule::smart_semicolon {
        None
    } else {
        return Err(get_unexpected_error(
            "build_ast_from_return_statement",
            &inner_pair,
        ));
    };
    Ok(StatementType::ReturnStatement { meta, argument })
}

fn build_ast_from_variable_statement(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<StatementType, JsRuleError> {
    let meta = get_meta(&pair, script);

    Ok(StatementType::DeclarationStatement(
        DeclarationType::VariableDeclaration(VariableDeclarationData {
            meta,
            declarations: build_ast_from_variable_declaration_list(
                pair.into_inner().next().unwrap(),
                script,
            )?,
            kind: VariableDeclarationKind::Var,
        }),
    ))
}

fn build_ast_from_variable_declaration_list(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Vec<VariableDeclaratorData>, JsRuleError> {
    let mut declarations = vec![];
    for var_pair in pair.into_inner() {
        if var_pair.as_rule() == Rule::variable_declaration
            || var_pair.as_rule() == Rule::variable_declaration__in
        {
            declarations.push(
                build_ast_from_lexical_binding_or_variable_declaration_or_binding_element(
                    var_pair, script,
                )?,
            )
        } else {
            return Err(get_unexpected_error(
                "build_ast_from_variable_declaration_list",
                &var_pair,
            ));
        }
    }
    Ok(declarations)
}

fn build_ast_from_lexical_binding_or_variable_declaration_or_binding_element(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<VariableDeclaratorData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_iter = pair.into_inner();
    let inner_pair = inner_iter.next().unwrap();
    Ok(
        if inner_pair.as_rule() == Rule::binding_identifier
            || inner_pair.as_rule() == Rule::binding_identifier__yield
        {
            let id = Box::new(
                ExpressionPatternType::Identifier(get_identifier_data(inner_pair, script))
                    .convert_to_pattern(),
            );
            if let Some(initializer) = inner_iter.next() {
                VariableDeclaratorData {
                    meta,
                    id,
                    init: Some(build_ast_from_assignment_expression(
                        initializer.into_inner().next().unwrap(),
                        script,
                    )?),
                }
            } else {
                VariableDeclaratorData {
                    meta,
                    id,
                    init: None,
                }
            }
        } else if inner_pair.as_rule() == Rule::binding_pattern
            || inner_pair.as_rule() == Rule::binding_pattern__yield
        {
            VariableDeclaratorData {
                meta,
                id: build_ast_from_binding_pattern(inner_pair)?,
                init: Some(build_ast_from_assignment_expression(
                    inner_iter.next().unwrap(),
                    script,
                )?),
            }
        } else if inner_pair.as_rule() == Rule::single_name_binding
            || inner_pair.as_rule() == Rule::single_name_binding__yield
        {
            build_ast_from_single_name_binding(inner_pair, script)?
        } else {
            return Err(get_unexpected_error(
                "build_ast_from_lexical_binding_or_variable_declaration",
                &inner_pair,
            ));
        },
    )
}

fn build_ast_from_single_name_binding(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<VariableDeclaratorData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_iter = pair.into_inner();
    let id = Box::new(
        ExpressionPatternType::Identifier(get_identifier_data(inner_iter.next().unwrap(), script))
            .convert_to_pattern(),
    );
    Ok(if let Some(initializer) = inner_iter.next() {
        VariableDeclaratorData {
            meta,
            id,
            init: Some(build_ast_from_assignment_expression(initializer, script)?),
        }
    } else {
        VariableDeclaratorData {
            meta,
            id,
            init: None,
        }
    })
}

// fn build_ast_from_binding_pattern(pair: Pair<Rule>) -> Result<PatternType, JsRuleError> {
//     let meta = get_meta(&pair);
//     let mut binding_properties = vec![];
//     let inner_pair = pair.into_inner().next().unwrap();
//     Ok(match inner_pair.as_rule() {
//         Rule::object_binding_pattern | Rule::object_binding_pattern__yield => {
//             let mut binding_pattern_inner_iter = inner_pair.into_inner();
//             for binding_property_pair in binding_pattern_inner_iter {
//                 binding_properties.push(build_ast_from_binding_property(binding_property_pair)?);
//             }
//             PatternType::ObjectPattern {
//                 meta,
//                 properties: binding_properties,
//             }
//         }
//         Rule::array_binding_pattern | Rule::array_binding_pattern__yield => {}
//         _ => {
//             return Err(get_unexpected_error(
//                 "build_ast_from_binding_pattern",
//                 &inner_pair,
//             ))
//         }
//     })
// }
//
// fn build_ast_from_binding_property(pair: Pair<Rule>) -> Result<AssignmentPropertyData, JsRuleError> {
//     let mut inner_iter = pair.into_inner();
//     let inner_pair = inner_iter.next().unwrap();
//     if inner_pair.as_rule() == Rule::single_name_binding
//         || inner_pair.as_rule() == Rule::single_name_binding__yield
//     {
//         build_ast_from_single_name_binding()
//     }
// }

fn build_ast_from_assignment_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut inner_pair_iter = pair.into_inner();
    let inner_pair = inner_pair_iter.next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::arrow_function
        | Rule::arrow_function__in
        | Rule::arrow_function__yield
        | Rule::arrow_function__in_yield => build_ast_from_arrow_function(inner_pair)?,
        Rule::left_hand_side_expression | Rule::left_hand_side_expression__yield => {
            let lhs = build_ast_from_left_hand_side_expression(inner_pair, script)?;
            let mut next_pair = inner_pair_iter.next().unwrap();
            let operator = if next_pair.as_rule() == Rule::assignment_operator {
                let op_str = next_pair.as_str();
                next_pair = inner_pair_iter.next().unwrap();
                match op_str {
                    "*=" => AssignmentOperator::MultiplyEquals,
                    "/=" => AssignmentOperator::DivideEquals,
                    "%=" => AssignmentOperator::ModuloEquals,
                    "+=" => AssignmentOperator::AddEquals,
                    "-=" => AssignmentOperator::SubtractEquals,
                    "<<=" => AssignmentOperator::BitwiseLeftShiftEquals,
                    ">>=" => AssignmentOperator::BitwiseRightShiftEquals,
                    ">>>=" => AssignmentOperator::BitwiseUnsignedRightShiftEquals,
                    "&=" => AssignmentOperator::BitwiseAndEquals,
                    "^=" => AssignmentOperator::BitwiseXorEquals,
                    "|=" => AssignmentOperator::BitwiseOrEquals,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_assignment_expression:1",
                            &next_pair,
                        ))
                    }
                }
            } else {
                AssignmentOperator::Equals
            };
            // next_pair is now assignment_expression
            let assignment_exp = build_ast_from_assignment_expression(next_pair, script)?;
            Box::new(ExpressionType::AssignmentExpression {
                meta,
                left: PatternOrExpression::Expression(lhs),
                operator,
                right: assignment_exp,
            })
        }
        Rule::yield_expression | Rule::yield_expression__in => {
            match inner_pair.into_inner().next() {
                Some(inner_pair) => {
                    let (assign_rule_pair, delegate) = if inner_pair.as_rule()
                        == Rule::star_assignment_expression__yield
                        || inner_pair.as_rule() == Rule::star_assignment_expression__in_yield
                    {
                        (inner_pair.into_inner().next().unwrap(), true)
                    } else {
                        (inner_pair, false)
                    };
                    let assignment_exp =
                        build_ast_from_assignment_expression(assign_rule_pair, script)?;
                    Box::new(ExpressionType::YieldExpression {
                        meta,
                        delegate,
                        argument: Some(assignment_exp),
                    })
                }
                None => Box::new(ExpressionType::YieldExpression {
                    meta,
                    delegate: false,
                    argument: None,
                }),
            }
        }
        Rule::conditional_expression
        | Rule::conditional_expression__in
        | Rule::conditional_expression__yield
        | Rule::conditional_expression__in_yield => {
            build_ast_from_conditional_expression(inner_pair, script)?
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_assignment_expression:2",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_arrow_function(pair: Pair<Rule>) -> Result<Box<ExpressionType>, JsRuleError> {
    unimplemented!()
}

fn build_ast_from_left_hand_side_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let inner_pair: Pair<Rule> = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::call_expression | Rule::call_expression__yield => {
            build_ast_from_call_expression(inner_pair, script)?
        }
        Rule::new_expression | Rule::new_expression__yield => {
            build_ast_from_new_expression(inner_pair, script)?
        }
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_left_hand_side_expression",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_new_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(
        if inner_pair.as_rule() == Rule::member_expression
            || inner_pair.as_rule() == Rule::member_expression__yield
        {
            build_ast_from_member_expression(inner_pair, script)?
        } else {
            Box::new(ExpressionType::NewExpression {
                meta: get_meta(&inner_pair, script),
                callee: build_ast_from_new_expression(inner_pair, script)?,
                arguments: vec![],
            })
        },
    )
}

fn build_ast_from_conditional_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut pair_iter = pair.into_inner();
    let logical_or_pair = pair_iter.next().unwrap();
    let or_node = build_ast_from_logical_or_expression(logical_or_pair, script)?;
    if let Some(inner_pair) = pair_iter.next() {
        let truthy = build_ast_from_assignment_expression(inner_pair, script)?;
        let falsy = build_ast_from_assignment_expression(pair_iter.next().unwrap(), script)?;
        Ok(Box::new(ExpressionType::ConditionalExpression {
            meta,
            test: or_node,
            consequent: truthy,
            alternate: falsy,
        }))
    } else {
        Ok(or_node)
    }
}

fn get_ast_for_logical_expression(
    left: Option<Box<ExpressionType>>,
    right: Box<ExpressionType>,
    operator: LogicalOperator,
    script: &Rc<String>,
) -> Box<ExpressionType> {
    if let Some(actual_left) = left {
        Box::new(ExpressionType::LogicalExpression {
            meta: Meta {
                start_index: actual_left.get_meta().start_index,
                end_index: right.get_meta().end_index,
                script: script.clone(),
            },
            operator,
            left: actual_left,
            right,
        })
    } else {
        right
    }
}

fn get_ast_for_binary_expression(
    left: Option<Box<ExpressionType>>,
    right: Box<ExpressionType>,
    operator: Option<BinaryOperator>,
    script: &Rc<String>,
) -> Box<ExpressionType> {
    if let Some(actual_left) = left {
        Box::new(ExpressionType::BinaryExpression {
            meta: Meta {
                start_index: actual_left.get_meta().start_index,
                end_index: right.get_meta().end_index,
                script: script.clone(),
            },
            operator: operator.unwrap(),
            left: actual_left,
            right,
        })
    } else {
        right
    }
}

fn build_ast_from_logical_or_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    for inner_pair in pair.into_inner() {
        let right = build_ast_from_logical_and_expression(inner_pair, script)?;
        left = Some(get_ast_for_logical_expression(
            left,
            right,
            LogicalOperator::Or,
            script,
        ));
    }
    Ok(left.unwrap())
}

fn build_ast_from_logical_and_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    for inner_pair in pair.into_inner() {
        let right = build_ast_from_bitwise_or_expression(inner_pair, script)?;
        left = Some(get_ast_for_logical_expression(
            left,
            right,
            LogicalOperator::And,
            script,
        ));
    }
    Ok(left.unwrap())
}

fn build_ast_from_bitwise_or_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    for inner_pair in pair.into_inner() {
        let right = build_ast_from_bitwise_xor_expression(inner_pair, script)?;
        left = Some(get_ast_for_binary_expression(
            left,
            right,
            Some(BinaryOperator::BitwiseOr),
            script,
        ))
    }
    Ok(left.unwrap())
}

fn build_ast_from_bitwise_xor_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    for inner_pair in pair.into_inner() {
        let right = build_ast_from_bitwise_and_expression(inner_pair, script)?;
        left = Some(get_ast_for_binary_expression(
            left,
            right,
            Some(BinaryOperator::BitwiseXor),
            script,
        ))
    }
    Ok(left.unwrap())
}

fn build_ast_from_bitwise_and_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    for inner_pair in pair.into_inner() {
        let right = build_ast_from_equality_expression(inner_pair, script)?;
        left = Some(get_ast_for_binary_expression(
            left,
            right,
            Some(BinaryOperator::BitwiseAnd),
            script,
        ))
    }
    Ok(left.unwrap())
}

fn build_ast_from_equality_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    if cfg!(debug_assertions) {
        if pair.as_rule() != Rule::equality_expression
            && pair.as_rule() != Rule::equality_expression__in
            && pair.as_rule() != Rule::equality_expression__yield
            && pair.as_rule() != Rule::equality_expression__in_yield
        {
            return Err(get_unexpected_error(
                "build_ast_from_equality_expression:0",
                &pair,
            ));
        }
    }
    let mut left = None;
    let mut pair_iter = pair.into_inner();
    loop {
        if let Some(mut inner_pair) = pair_iter.next() {
            let mut operator = None;
            if inner_pair.as_rule() == Rule::equality_operator {
                operator = Some(match inner_pair.as_str() {
                    "===" => BinaryOperator::StrictlyEqual,
                    "!==" => BinaryOperator::StrictlyUnequal,
                    "==" => BinaryOperator::LooselyEqual,
                    "!=" => BinaryOperator::LooselyUnequal,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_equality_expression",
                            &inner_pair,
                        ))
                    }
                });
                inner_pair = pair_iter.next().unwrap();
            }
            let right = build_ast_from_relational_expression(inner_pair, script)?;
            left = Some(get_ast_for_binary_expression(left, right, operator, script));
        } else {
            break;
        }
    }
    Ok(left.unwrap())
}

fn build_ast_from_relational_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    if cfg!(debug_assertions) {
        if pair.as_rule() != Rule::relational_expression
            && pair.as_rule() != Rule::relational_expression__in
            && pair.as_rule() != Rule::relational_expression__yield
            && pair.as_rule() != Rule::relational_expression__in_yield
        {
            return Err(get_unexpected_error(
                "build_ast_from_relational_expression:0",
                &pair,
            ));
        }
    }
    let mut left = None;
    let mut pair_iter = pair.into_inner();
    loop {
        if let Some(mut inner_pair) = pair_iter.next() {
            let mut operator = None;
            if inner_pair.as_rule() == Rule::relational_operator
                || inner_pair.as_rule() == Rule::relational_operator__in
            {
                operator = Some(match inner_pair.as_str() {
                    "<=" => BinaryOperator::LessThanEqual,
                    ">=" => BinaryOperator::GreaterThanEqual,
                    "<" => BinaryOperator::LessThan,
                    ">" => BinaryOperator::GreaterThan,
                    "instanceof" => BinaryOperator::InstanceOf,
                    "in" => BinaryOperator::In,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_relational_expression",
                            &inner_pair,
                        ))
                    }
                });
                inner_pair = pair_iter.next().unwrap();
            }
            let right = build_ast_from_shift_expression(inner_pair, script)?;
            left = Some(get_ast_for_binary_expression(left, right, operator, script));
        } else {
            break;
        }
    }
    Ok(left.unwrap())
}

fn build_ast_from_shift_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    if cfg!(debug_assertions) {
        if pair.as_rule() != Rule::shift_expression
            && pair.as_rule() != Rule::shift_expression__yield
        {
            return Err(get_unexpected_error(
                "build_ast_from_shift_expression:0",
                &pair,
            ));
        }
    }
    let mut left = None;
    let mut pair_iter = pair.into_inner();
    loop {
        if let Some(mut inner_pair) = pair_iter.next() {
            let mut operator = None;
            if inner_pair.as_rule() == Rule::shift_operator {
                operator = Some(match inner_pair.as_str() {
                    "<<" => BinaryOperator::BitwiseLeftShift,
                    ">>>" => BinaryOperator::BitwiseUnsignedRightShift,
                    ">>" => BinaryOperator::BitwiseRightShift,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_shift_expression",
                            &inner_pair,
                        ))
                    }
                });
                inner_pair = pair_iter.next().unwrap();
            }
            let right = build_ast_from_additive_expression(inner_pair, script)?;
            left = Some(get_ast_for_binary_expression(left, right, operator, script));
        } else {
            break;
        }
    }
    Ok(left.unwrap())
}

fn build_ast_from_additive_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    let mut pair_iter = pair.into_inner();
    loop {
        if let Some(mut inner_pair) = pair_iter.next() {
            let mut operator = None;
            if inner_pair.as_rule() == Rule::additive_operator {
                operator = Some(match inner_pair.as_str() {
                    "+" => BinaryOperator::Add,
                    "-" => BinaryOperator::Subtract,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_additive_expression",
                            &inner_pair,
                        ))
                    }
                });
                inner_pair = pair_iter.next().unwrap();
            }
            let right = build_ast_from_multiplicative_expression(inner_pair, script)?;
            left = Some(get_ast_for_binary_expression(left, right, operator, script));
        } else {
            break;
        }
    }
    Ok(left.unwrap())
}

fn build_ast_from_multiplicative_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut left = None;
    let mut pair_iter = pair.into_inner();
    loop {
        if let Some(mut inner_pair) = pair_iter.next() {
            let mut operator = None;
            if inner_pair.as_rule() == Rule::multiplicative_operator {
                operator = Some(match inner_pair.as_str() {
                    "*" => BinaryOperator::Multiply,
                    "/" => BinaryOperator::Divide,
                    "%" => BinaryOperator::Modulo,
                    _ => {
                        return Err(get_unexpected_error(
                            "build_ast_from_multiplicative_expression",
                            &inner_pair,
                        ))
                    }
                });
                inner_pair = pair_iter.next().unwrap();
            }
            let right = build_ast_from_unary_expression(inner_pair, script)?;
            left = Some(get_ast_for_binary_expression(left, right, operator, script));
        } else {
            break;
        }
    }
    Ok(left.unwrap())
}

fn build_ast_from_unary_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut pair_iter = pair.into_inner();
    let first_pair = pair_iter.next().unwrap();

    Ok(
        if first_pair.as_rule() == Rule::postfix_expression
            || first_pair.as_rule() == Rule::postfix_expression__yield
        {
            build_ast_from_postfix_expression(first_pair, script)?
        } else {
            let first_inner_pair = first_pair.into_inner().next().unwrap();
            Box::new(match first_inner_pair.as_str() {
                "++" | "--" => ExpressionType::UpdateExpression {
                    meta,
                    operator: match first_inner_pair.as_str() {
                        "++" => UpdateOperator::PlusPlus,
                        "--" => UpdateOperator::MinusMinus,
                        _ => {
                            return Err(get_unexpected_error(
                                "build_ast_from_unary_expression:1",
                                &first_inner_pair,
                            ))
                        }
                    },
                    argument: build_ast_from_unary_expression(pair_iter.next().unwrap(), script)?,
                    prefix: true,
                },
                _ => ExpressionType::UnaryExpression {
                    meta,
                    operator: match first_inner_pair.as_str() {
                        "delete" => UnaryOperator::Delete,
                        "void" => UnaryOperator::Void,
                        "typeof" => UnaryOperator::TypeOf,
                        "+" => UnaryOperator::Plus,
                        "-" => UnaryOperator::Minus,
                        "~" => UnaryOperator::BitwiseNot,
                        "!" => UnaryOperator::LogicalNot,
                        _ => {
                            return Err(get_unexpected_error(
                                "build_ast_from_unary_expression:2",
                                &first_inner_pair,
                            ))
                        }
                    },
                    argument: build_ast_from_unary_expression(pair_iter.next().unwrap(), script)?,
                },
            })
        },
    )
}

fn build_ast_from_postfix_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut pair_iter = pair.into_inner();
    let lhs = build_ast_from_left_hand_side_expression(pair_iter.next().unwrap(), script)?;
    Ok(if let Some(op_pair) = pair_iter.next() {
        Box::new(ExpressionType::UpdateExpression {
            meta,
            operator: match op_pair.as_str() {
                "++" => UpdateOperator::PlusPlus,
                "--" => UpdateOperator::MinusMinus,
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_postfix_expression",
                        &op_pair,
                    ))
                }
            },
            argument: lhs,
            prefix: false,
        })
    } else {
        lhs
    })
}

fn build_ast_from_call_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let mut pair_iter = pair.into_inner();
    let pair = pair_iter.next().unwrap();
    let meta = get_meta(&pair, script);
    let mut obj: Box<ExpressionType> = Box::new(
        if pair.as_rule() == Rule::super_call || pair.as_rule() == Rule::super_call__yield {
            ExpressionType::CallExpression {
                meta,
                callee: ExpressionOrSuper::Super,
                arguments: build_ast_from_arguments(pair.into_inner().next().unwrap(), script)?,
            }
        } else {
            let arguments_pair = pair_iter.next().unwrap();
            ExpressionType::CallExpression {
                meta,
                callee: ExpressionOrSuper::Expression(build_ast_from_member_expression(
                    pair, script,
                )?),
                arguments: build_ast_from_arguments(arguments_pair, script)?,
            }
        },
    );
    for pair in pair_iter {
        let second_meta = get_meta(&pair, script);
        let meta = Meta {
            start_index: obj.get_meta().start_index,
            end_index: second_meta.end_index,
            script: script.clone(),
        };
        obj = match pair.as_rule() {
            Rule::expression__in_yield | Rule::expression__in => Box::new(
                ExpressionPatternType::MemberExpression(
                    MemberExpressionType::ComputedMemberExpression {
                        meta,
                        object: ExpressionOrSuper::Expression(obj),
                        property: build_ast_from_expression(pair, script)?,
                    },
                )
                .convert_to_expression(),
            ),
            Rule::identifier_name => Box::new(
                ExpressionPatternType::MemberExpression(
                    MemberExpressionType::SimpleMemberExpression {
                        meta,
                        object: ExpressionOrSuper::Expression(obj),
                        property: get_identifier_data(pair, script),
                    },
                )
                .convert_to_expression(),
            ),
            //Tagged template literal
            Rule::template_literal | Rule::template_literal__yield => unimplemented!(),
            Rule::arguments | Rule::arguments__yield => Box::new(ExpressionType::CallExpression {
                meta,
                callee: ExpressionOrSuper::Expression(obj),
                arguments: build_ast_from_arguments(pair, script)?,
            }),
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

fn get_identifier_data(pair: Pair<Rule>, script: &Rc<String>) -> IdentifierData {
    IdentifierData {
        meta: get_meta(&pair, script),
        name: pair.as_str().trim().to_string(),
    }
}

fn build_ast_from_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut node_children: Vec<Box<ExpressionType>> = vec![];
    for inner_pair in pair.into_inner() {
        node_children.push(build_ast_from_assignment_expression(inner_pair, script)?);
    }
    Ok(Box::new(ExpressionType::SequenceExpression {
        meta,
        expressions: node_children,
    }))
}

fn build_ast_from_arguments(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Vec<ExpressionOrSpreadElement>, JsRuleError> {
    let mut arguments = vec![];
    if let Some(argument_list_pair) = pair.into_inner().next() {
        for inner_pair in argument_list_pair.into_inner() {
            arguments.push(
                if inner_pair.as_rule() == Rule::rest_assignment_expression__in
                    || inner_pair.as_rule() == Rule::rest_assignment_expression__in_yield
                {
                    ExpressionOrSpreadElement::SpreadElement(build_ast_from_assignment_expression(
                        inner_pair.into_inner().next().unwrap(),
                        script,
                    )?)
                } else {
                    ExpressionOrSpreadElement::Expression(build_ast_from_assignment_expression(
                        inner_pair, script,
                    )?)
                },
            );
        }
    }
    Ok(arguments)
}

fn build_ast_from_member_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut pair_iter = pair.into_inner();
    let pair_1 = pair_iter.next().unwrap();
    Ok(
        if pair_1.as_rule() == Rule::new_member_expression
            || pair_1.as_rule() == Rule::new_member_expression__yield
        {
            let member_expression_pair = pair_1.into_inner().next().unwrap();
            let arguments_pair = pair_iter.next().unwrap();
            Box::new(ExpressionType::NewExpression {
                meta,
                callee: build_ast_from_member_expression(member_expression_pair, script)?,
                arguments: build_ast_from_arguments(arguments_pair, script)?,
            })
        } else {
            let mut obj: Box<ExpressionType> = match pair_1.as_rule() {
                Rule::super_property | Rule::super_property__yield => {
                    let super_pair = pair_1.into_inner().next().unwrap();
                    if super_pair.as_rule() == Rule::identifier_name {
                        Box::new(
                            ExpressionPatternType::MemberExpression(
                                MemberExpressionType::SimpleMemberExpression {
                                    meta,
                                    object: ExpressionOrSuper::Super,
                                    property: get_identifier_data(super_pair, script),
                                },
                            )
                            .convert_to_expression(),
                        )
                    } else {
                        Box::new(
                            ExpressionPatternType::MemberExpression(
                                MemberExpressionType::ComputedMemberExpression {
                                    meta,
                                    object: ExpressionOrSuper::Super,
                                    property: build_ast_from_expression(super_pair, script)?,
                                },
                            )
                            .convert_to_expression(),
                        )
                    }
                }
                Rule::meta_property => {
                    let start = meta.start_index;
                    let end = meta.end_index;
                    Box::new(ExpressionType::MetaProperty {
                        meta,
                        meta_object: IdentifierData {
                            meta: Meta {
                                start_index: start,
                                end_index: start + 3,
                                script: script.clone(),
                            },
                            name: "new".to_string(),
                        },
                        property: IdentifierData {
                            meta: Meta {
                                start_index: start + 4,
                                end_index: end,
                                script: script.clone(),
                            },
                            name: "target".to_string(),
                        },
                    })
                }
                Rule::primary_expression | Rule::primary_expression__yield => {
                    build_ast_from_primary_expression(pair_1, script)?
                }
                _ => {
                    return Err(get_unexpected_error(
                        "build_ast_from_member_expression:1",
                        &pair_1,
                    ))
                }
            };
            for pair in pair_iter {
                let second_meta = get_meta(&pair, script);
                let meta = Meta {
                    start_index: obj.get_meta().start_index,
                    end_index: second_meta.end_index,
                    script: script.clone(),
                };
                obj = match pair.as_rule() {
                    Rule::expression__in_yield | Rule::expression__in => Box::new(
                        ExpressionPatternType::MemberExpression(
                            MemberExpressionType::ComputedMemberExpression {
                                meta,
                                object: ExpressionOrSuper::Expression(obj),
                                property: build_ast_from_expression(pair, script)?,
                            },
                        )
                        .convert_to_expression(),
                    ),
                    Rule::identifier_name => Box::new(
                        ExpressionPatternType::MemberExpression(
                            MemberExpressionType::SimpleMemberExpression {
                                meta,
                                object: ExpressionOrSuper::Expression(obj),
                                property: get_identifier_data(pair, script),
                            },
                        )
                        .convert_to_expression(),
                    ),
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

fn build_ast_from_primary_expression(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    let meta = get_meta(&inner_pair, script);
    Ok(match inner_pair.as_rule() {
        Rule::identifier_reference | Rule::identifier_reference__yield => {
            build_ast_from_identifier_reference(inner_pair, script)?
        }
        Rule::literal => Box::new(ExpressionType::Literal(build_ast_from_literal(
            inner_pair, script,
        )?)),
        Rule::this_exp => Box::new(ExpressionType::ThisExpression { meta }),
        Rule::array_literal | Rule::array_literal__yield => {
            Box::new(build_ast_from_array_literal(inner_pair, script)?)
        }
        Rule::object_literal | Rule::object_literal__yield => unimplemented!(),
        Rule::generator_expression => unimplemented!(),
        Rule::function_expression => Box::new(ExpressionType::FunctionExpression(
            build_ast_from_function_declaration_or_function_expression(inner_pair, script)?,
        )),
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

fn build_ast_from_literal(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<LiteralData, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    let meta = get_meta(&inner_pair, script);
    Ok(match inner_pair.as_rule() {
        Rule::null_literal => LiteralData {
            meta,
            value: LiteralType::NullLiteral,
        },
        Rule::numeric_literal => LiteralData {
            meta,
            value: LiteralType::NumberLiteral(build_ast_from_numeric_literal(inner_pair)?),
        },
        Rule::string_literal => build_ast_from_string_literal(inner_pair, script)?,
        Rule::boolean_literal => {
            let bool = inner_pair.as_str();
            LiteralData {
                meta,
                value: LiteralType::BooleanLiteral(bool == "true"),
            }
        }
        _ => return Err(get_unexpected_error("build_ast_from_literal", &inner_pair)),
    })
}

fn build_ast_from_string_literal(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<LiteralData, JsRuleError> {
    let meta = get_meta(&pair, script);
    let s = pair.as_str();
    Ok(LiteralData {
        meta,
        value: LiteralType::StringLiteral(String::from(&s[1..s.len() - 1])),
    })
}

fn build_ast_from_str_numeric_literal(
    pair: Pair<Rule>,
) -> Result<ExtendedNumberLiteralType, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::binary_integer_literal => {
            ExtendedNumberLiteralType::Std(get_ast_for_binary_integer_literal(inner_pair))
        }
        Rule::octal_integer_literal => {
            ExtendedNumberLiteralType::Std(get_ast_for_octal_integer_literal(inner_pair))
        }
        Rule::hex_integer_literal => {
            ExtendedNumberLiteralType::Std(get_ast_for_hex_integer_literal(inner_pair))
        }
        Rule::str_decimal_literal => build_ast_str_decimal_literal(inner_pair)?,
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_str_numeric_literal",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_str_decimal_literal(
    pair: Pair<Rule>,
) -> Result<ExtendedNumberLiteralType, JsRuleError> {
    let mut pair_iter = pair.into_inner();
    let mut inner_pair = pair_iter.next().unwrap();
    let is_negative = if inner_pair.as_rule() == Rule::str_negative_op {
        inner_pair = pair_iter.next().unwrap();
        true
    } else {
        false
    };
    build_ast_str_unsigned_decimal_literal(inner_pair, is_negative)
}

fn build_ast_str_unsigned_decimal_literal(
    pair: Pair<Rule>,
    is_negative: bool,
) -> Result<ExtendedNumberLiteralType, JsRuleError> {
    let mut num: f64 = 0.0;
    let mut is_float = false;
    for decimal_pair in pair.into_inner() {
        num = match decimal_pair.as_rule() {
            Rule::str_decimal_literal_infinity => {
                return Ok(if is_negative {
                    ExtendedNumberLiteralType::NegativeInfinity
                } else {
                    ExtendedNumberLiteralType::Infinity
                });
            }
            Rule::decimal_digits_integer_part => parse_decimal_integer_literal(decimal_pair),
            Rule::decimal_digits => {
                is_float = true;
                num + parse_decimal_digits(decimal_pair)
            }
            Rule::exponent_part => num * parse_exponent_part(decimal_pair),
            _ => {
                return Err(get_unexpected_error(
                    "build_ast_str_unsigned_decimal_literal",
                    &decimal_pair,
                ))
            }
        }
    }
    if is_negative {
        num *= -1.0;
    }
    Ok(if !is_float {
        ExtendedNumberLiteralType::Std(NumberLiteralType::IntegerLiteral(num as i64))
    } else {
        ExtendedNumberLiteralType::Std(NumberLiteralType::FloatLiteral(num))
    })
}

fn get_ast_for_binary_integer_literal(pair: Pair<Rule>) -> NumberLiteralType {
    NumberLiteralType::IntegerLiteral(isize::from_str_radix(&pair.as_str()[2..], 2).unwrap() as i64)
}

fn get_ast_for_octal_integer_literal(pair: Pair<Rule>) -> NumberLiteralType {
    NumberLiteralType::IntegerLiteral(isize::from_str_radix(&pair.as_str()[2..], 8).unwrap() as i64)
}

fn get_ast_for_hex_integer_literal(pair: Pair<Rule>) -> NumberLiteralType {
    NumberLiteralType::IntegerLiteral(isize::from_str_radix(&pair.as_str()[2..], 16).unwrap() as i64)
}

fn parse_decimal_integer_literal(decimal_pair: Pair<Rule>) -> f64 {
    isize::from_str_radix(decimal_pair.as_str(), 10).unwrap() as f64
}

fn parse_decimal_digits(decimal_pair: Pair<Rule>) -> f64 {
    let d = decimal_pair.as_str();
    isize::from_str_radix(d, 10).unwrap() as f64 / 10_f64.powf(d.len() as f64)
}

fn parse_exponent_part(decimal_pair: Pair<Rule>) -> f64 {
    10_f64.powf(isize::from_str_radix(&decimal_pair.as_str()[1..], 10).unwrap() as f64)
}

fn build_ast_decimal_literal(pair: Pair<Rule>) -> Result<NumberLiteralType, JsRuleError> {
    let mut num: f64 = 0.0;
    let mut is_float = false;
    for decimal_pair in pair.into_inner() {
        num = match decimal_pair.as_rule() {
            Rule::decimal_integer_literal => parse_decimal_integer_literal(decimal_pair),
            Rule::decimal_digits => {
                is_float = true;
                num + parse_decimal_digits(decimal_pair)
            }
            Rule::exponent_part => num * parse_exponent_part(decimal_pair),
            _ => {
                return Err(get_unexpected_error(
                    "build_ast_decimal_literal",
                    &decimal_pair,
                ))
            }
        }
    }
    Ok(if !is_float {
        NumberLiteralType::IntegerLiteral(num as i64)
    } else {
        NumberLiteralType::FloatLiteral(num)
    })
}

fn build_ast_from_numeric_literal(pair: Pair<Rule>) -> Result<NumberLiteralType, JsRuleError> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::binary_integer_literal => get_ast_for_binary_integer_literal(inner_pair),
        Rule::octal_integer_literal => get_ast_for_octal_integer_literal(inner_pair),
        Rule::hex_integer_literal => get_ast_for_hex_integer_literal(inner_pair),
        Rule::decimal_literal => build_ast_decimal_literal(inner_pair)?,
        _ => {
            return Err(get_unexpected_error(
                "build_ast_from_numeric_literal",
                &inner_pair,
            ))
        }
    })
}

fn build_ast_from_array_literal(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<ExpressionType, JsRuleError> {
    let meta = get_meta(&pair, script);
    let mut arguments = vec![];
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::elision => {
                for _ in 1..(inner_pair.as_str().matches(',').count()) {
                    arguments.push(None);
                }
            }
            Rule::assignment_expression__in | Rule::assignment_expression__in_yield => {
                arguments.push(Some(ExpressionOrSpreadElement::Expression(
                    build_ast_from_assignment_expression(inner_pair, script)?,
                )));
            }
            Rule::spread_element | Rule::spread_element__yield => {
                arguments.push(Some(ExpressionOrSpreadElement::SpreadElement(
                    build_ast_from_assignment_expression(
                        inner_pair.into_inner().next().unwrap(),
                        script,
                    )?,
                )));
            }
            _ => {
                return Err(get_unexpected_error(
                    "build_ast_from_array_literal",
                    &inner_pair,
                ))
            }
        }
    }
    Ok(ExpressionType::ArrayExpression {
        meta,
        elements: arguments,
    })
}

fn build_ast_from_identifier_reference(
    pair: Pair<Rule>,
    script: &Rc<String>,
) -> Result<Box<ExpressionType>, JsRuleError> {
    let id = pair.as_str();
    Ok(if id == "yield" {
        Box::new(ExpressionType::YieldExpression {
            meta: get_meta(&pair, script),
            argument: None,
            delegate: false,
        })
    } else {
        Box::new(
            ExpressionPatternType::Identifier(get_identifier_data(pair, script))
                .convert_to_expression(),
        )
    })
}
