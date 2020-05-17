use std::rc::Rc;
use std::time::Instant;

use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use super::ast::*;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
pub struct JsParser;

const TAB_WIDTH: usize = 2;

pub fn parse_to_token_tree(script: &str) -> Result<String, String> {
    let mut tree = vec![];
    let start = Instant::now();
    let result = JsParser::parse(Rule::script, script);
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
        string_pads.push(' ');
    }
    tree.push(format!("{}{}", string_pads, rule_name));
    for child_pair in pair.into_inner() {
        tree.append(pair_to_string(child_pair, level + 1).as_mut());
    }
    tree
}

pub fn parse_to_pairs(script: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    JsParser::parse(Rule::script, script)
}

pub fn parse_to_ast(script: &str) -> Result<Node, Error<Rule>> {
    let pairs = JsParser::parse(Rule::script, script)?;
    build_ast_from_statement_list_pairs(pairs)
}

fn build_ast_from_statement_list_pairs(pairs: Pairs<Rule>) -> Result<Node, Error<Rule>> {
    let mut s = vec![];
    let mut all_hoisted_declarations = vec![];
    for pair in pairs {
        match pair.as_rule() {
            Rule::declaration | Rule::declaration__yield => {
                let err = Err(get_unexpected_error(24, &pair));
                let n = Rc::new(build_ast_from_declaration(pair)?);
                match &*n {
                    Node::Declaration(declaration_type) => {
                        match declaration_type {
                            DeclarationType::FunctionDeclaration { .. }
                            | DeclarationType::GeneratorDeclaration { .. }
                            | DeclarationType::VarDeclaration(_) => {
                                all_hoisted_declarations.push(Rc::downgrade(&n));
                            }
                            _ => { /* Do nothing as others are not hoistable */ }
                        }
                    }
                    _ => return err,
                };
                s.push(n);
            }
            Rule::statement
            | Rule::statement__yield
            | Rule::statement__return
            | Rule::statement__yield_return => {
                let n = Rc::new(build_ast_from_statement(pair)?);
                match &*n {
                    Node::Declaration(declaration_type) => {
                        match declaration_type {
                            DeclarationType::VarDeclaration(_) => {
                                all_hoisted_declarations.push(Rc::downgrade(&n));
                            }
                            _ => { /* Do nothing as others are not hoistable */ }
                        }
                    }
                    _ => { /* Do nothing*/ }
                };
                s.push(n);
            }
            Rule::EOI => { /* Do nothing */ }
            _ => return Err(get_unexpected_error(18, &pair)),
        };
    }
    Ok(Node::Statements {
        all_hoisted_declarations,
        statements: s,
    })
}

fn build_ast_from_statement(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::debugger_statement => panic!("Not yet implemented!"),
        Rule::continue_statement | Rule::continue_statement__yield => {
            panic!("Not yet implemented!")
        }
        Rule::break_statement | Rule::break_statement__yield => panic!("Not yet implemented!"),
        Rule::throw_statement | Rule::break_statement__yield => panic!("Not yet implemented!"),
        Rule::if_statement
        | Rule::if_statement__yield
        | Rule::if_statement__return
        | Rule::if_statement__yield_return => panic!("Not yet implemented!"),
        Rule::with_statement
        | Rule::with_statement__yield
        | Rule::with_statement__return
        | Rule::with_statement__yield_return => panic!("Not yet implemented!"),
        Rule::try_statement
        | Rule::try_statement__yield
        | Rule::try_statement__return
        | Rule::try_statement__yield_return => panic!("Not yet implemented!"),
        Rule::variable_statement | Rule::variable_statement__yield => {
            let mut declarations = vec![];
            for var_pair in inner_pair.into_inner() {
                if var_pair.as_rule() == Rule::variable_declaration
                    || var_pair.as_rule() == Rule::variable_declaration__in
                {
                    declarations.push(get_lexical_binding_or_variable_declaration(var_pair)?)
                } else if var_pair.as_rule() != Rule::smart_semicolon {
                    // smart_semicolon at the end is expected but nothing else.
                    return Err(get_unexpected_error(23, &var_pair));
                }
            }
            Node::Declaration(DeclarationType::VarDeclaration(declarations))
        }
        Rule::breakable_statement
        | Rule::breakable_statement__yield
        | Rule::breakable_statement__return
        | Rule::breakable_statement__yield_return => panic!("Not yet implemented!"),
        Rule::block_statement
        | Rule::block_statement__yield
        | Rule::block_statement__return
        | Rule::block_statement__yield_return => panic!("Not yet implemented!"),
        Rule::expression_statement | Rule::expression_statement__yield => {
            panic!("Not yet implemented!")
        }
        Rule::labelled_statement
        | Rule::labelled_statement__yield
        | Rule::labelled_statement__return
        | Rule::labelled_statement__yield_return => panic!("Not yet implemented!"),
        Rule::empty_statement => panic!("Not yet implemented!"),
        Rule::return_statement | Rule::return_statement__yield => panic!("Not yet implemented!"),
        _ => return Err(get_unexpected_error(22, &inner_pair)),
    })
}

fn get_lexical_binding_or_variable_declaration(
    pair: Pair<Rule>,
) -> Result<BindingType, Error<Rule>> {
    let mut lexical_binding_inner_iter = pair.into_inner();
    let lexical_binding_inner = lexical_binding_inner_iter.next().unwrap();
    Ok(
        if lexical_binding_inner.as_rule() == Rule::binding_identifier
            || lexical_binding_inner.as_rule() == Rule::binding_identifier__yield
        {
            let id = Data::IdentifierName(lexical_binding_inner.as_str().to_string());
            if let Some(initializer) = lexical_binding_inner_iter.next() {
                BindingType::SimpleBinding {
                    identifier: id,
                    initializer: Some(Box::new(build_ast_from_assignment_expression(
                        initializer.into_inner().next().unwrap(),
                    )?)),
                }
            } else {
                BindingType::SimpleBinding {
                    identifier: id,
                    initializer: None,
                }
            }
        } else if lexical_binding_inner.as_rule() == Rule::binding_pattern
            || lexical_binding_inner.as_rule() == Rule::binding_pattern__yield
        {
            //TODO
            panic!("Not implemented yet!");
        } else {
            return Err(get_unexpected_error(23, &lexical_binding_inner));
        },
    )
}

fn build_ast_from_lexical_declaration(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let inner_pair = pair_iter.next().unwrap();
    let is_let = inner_pair.into_inner().next().unwrap().as_str() == "let";
    let mut declarations = vec![];
    let binding_list_pair = pair_iter.next().unwrap();
    for lexical_binding in binding_list_pair.into_inner() {
        declarations.push(get_lexical_binding_or_variable_declaration(
            lexical_binding,
        )?);
    }
    Ok(Node::Declaration(if is_let {
        DeclarationType::LetDeclaration(declarations)
    } else {
        DeclarationType::ConstDeclaration(declarations)
    }))
}

fn build_ast_from_generator_declaration(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    //TODO
    panic!("Not yet implemented!");
}

fn build_ast_from_function_declaration(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
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

fn build_ast_from_hoistable_declaration(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    if inner_pair.as_rule() == Rule::function_declaration {
        build_ast_from_function_declaration(inner_pair)
    } else {
        build_ast_from_generator_declaration(inner_pair)
    }
}

fn build_ast_from_declaration(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::hoistable_declaration | Rule::hoistable_declaration__yield => {
            build_ast_from_hoistable_declaration(inner_pair)
        }
        Rule::class_declaration | Rule::class_declaration__yield => {
            //TODO
            panic!("Not implemented yet!");
        }
        Rule::lexical_declaration__in | Rule::lexical_declaration__in_yield => {
            build_ast_from_lexical_declaration(inner_pair)
        }
        _ => return Err(get_unexpected_error(15, &inner_pair)),
    }
}

fn get_unexpected_error(id: i32, pair: &Pair<Rule>) -> Error<Rule> {
    let message = format!("Unexpected state reached [{:?}] - {}", pair.as_rule(), id);
    Error::new_from_span(ErrorVariant::CustomError { message }, pair.as_span())
}

fn build_ast_from_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut node_children = vec![];
    for inner_pair in pair.into_inner() {
        node_children.push(Box::new(build_ast_from_assignment_expression(inner_pair)?));
    }
    Ok(Node::Expression(node_children))
}

fn build_ast_from_call_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let pair = pair_iter.next().unwrap();
    let mut obj = if pair.as_rule() == Rule::super_call {
        Node::CallExpression {
            f: Box::new(Node::Terminal(Data::Super)),
            arguments: get_arguments(pair.into_inner().next().unwrap())?,
        }
    } else {
        Node::CallExpression {
            f: Box::new(build_ast_from_member_expression(pair)?),
            arguments: get_arguments(pair_iter.next().unwrap())?,
        }
    };
    for pair in pair_iter {
        obj = match pair.as_rule() {
            Rule::expression__in_yield | Rule::expression__in => Node::MemberAccessBracket {
                object: Box::new(obj),
                member: Box::new(build_ast_from_expression(pair)?),
            },
            Rule::identifier_name => Node::MemberAccessDot {
                object: Box::new(obj),
                member: get_identifier_name(pair),
            },
            Rule::template_literal | Rule::template_literal__yield => Node::TaggedLiteral {
                f: Box::new(obj),
                template: Box::new(build_ast_from_template_literal(pair)?),
            },
            Rule::arguments | Rule::arguments__yield => Node::CallExpression {
                f: Box::new(obj),
                arguments: get_arguments(pair)?,
            },
            _ => return Err(get_unexpected_error(14, &pair)),
        };
    }
    Ok(obj)
}

fn build_ast_from_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::null_literal => Node::Terminal(Data::NullLiteral),
        Rule::numeric_literal => build_ast_from_numeric_literal(inner_pair)?,
        Rule::string_literal => build_ast_from_string_literal(inner_pair)?,
        Rule::boolean_literal => {
            let bool = inner_pair.as_str();
            Node::Terminal(Data::BooleanLiteral(bool == "true"))
        }
        _ => return Err(get_unexpected_error(14, &inner_pair)),
    })
}

fn build_ast_from_array_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut arguments = vec![];
    for inner_pair in pair.into_inner() {
        arguments.push(if inner_pair.as_rule() == Rule::spread_element {
            let inner_pair = inner_pair.into_inner().next().unwrap();
            ArrayArgumentType::Spread(Box::new(build_ast_from_assignment_expression(inner_pair)?))
        } else {
            ArrayArgumentType::Regular(Box::new(build_ast_from_assignment_expression(inner_pair)?))
        });
    }
    Ok(Node::ArrayLiteral(arguments))
}

// fn build_ast_from_object_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
//     let mut p = vec![];
//     if let Some(inner_pair) = pair.into_inner().next() {
//         for pair in inner_pair.into_inner() {
//             p.push(build_ast_from_property_definition(pair)?);
//         }
//     }
//     Ok(Node::ObjectLiteral(p))
// }
//
// fn get_property_definition(pair: Pair<Rule>) -> Result<PropertyType, Error<Rule>> {
//     let inner_pair = pair.into_inner().next().unwrap();
//
// }

fn build_ast_from_function_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    //TODO
    Ok(Node::Terminal(Data::NullLiteral))
}

fn build_ast_from_primary_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::identifier_reference | Rule::identifier_reference__yield => {
            build_ast_from_identifier_reference(inner_pair)?
        }
        Rule::literal => build_ast_from_literal(inner_pair)?,
        Rule::this_exp => Node::Terminal(Data::This),
        Rule::array_literal | Rule::array_literal__yield => {
            build_ast_from_array_literal(inner_pair)?
        }
        Rule::object_literal | Rule::object_literal__yield => {
            // build_ast_from_object_literal(inner_pair)?
            panic!("Not implemented right now!");
        }
        Rule::generator_expression => {
            panic!("Not implemented right now!");
        }
        Rule::function_expression => build_ast_from_function_expression(inner_pair)?,
        Rule::class_expression | Rule::class_expression__yield => {
            panic!("Not implemented right now!");
        }
        Rule::regular_expression_literal => {
            panic!("Not implemented right now!");
        }
        Rule::template_literal | Rule::template_literal__yield => {
            build_ast_from_template_literal(inner_pair)?
        }
        Rule::cover_parenthesized_expression_and_arrow_parameter_list
        | Rule::cover_parenthesized_expression_and_arrow_parameter_list__yield => {
            panic!("Use-case not clear. Not implemented right now!");
        }
        _ => return Err(get_unexpected_error(13, &inner_pair)),
    })
}

fn build_ast_from_cover_parenthesized_expression_and_arrow_parameter_list(
    pair: Pair<Rule>,
) -> Result<Node, Error<Rule>> {
    // for inner_pair in pair.into_inner() {
    //     match inner_pair.as_rule() {
    //         Rule::binding_identifier | Rule::binding_identifier__yield => {
    //
    //         }
    //     }
    // }
    Ok(Node::Terminal(Data::NullLiteral))
}

fn build_ast_from_super_property(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::identifier_name => Node::MemberAccessDot {
            object: Box::new(Node::Terminal(Data::Super)),
            member: get_identifier_name(inner_pair),
        },
        _ => Node::MemberAccessBracket {
            object: Box::new(Node::Terminal(Data::Super)),
            member: Box::new(build_ast_from_expression(inner_pair)?),
        },
    })
}

fn build_ast_from_identifier_reference(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let id = pair.as_str().to_string();
    Ok(Node::Terminal(if id.as_str() == "yield" {
        Data::Yield
    } else {
        Data::IdentifierName(id)
    }))
}

fn get_identifier_name(pair: Pair<Rule>) -> Data {
    Data::IdentifierName(pair.as_str().to_string())
}

fn build_ast_from_member_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let pair_1 = pair_iter.next().unwrap();
    Ok(match pair_1.as_rule() {
        Rule::member_expression | Rule::member_expression__yield => Node::InstantiateObject {
            constructor: Box::new(build_ast_from_member_expression(pair_1)?),
            arguments: get_arguments(pair_iter.next().unwrap())?,
        },
        _ => {
            let mut obj = match pair_1.as_rule() {
                Rule::super_property | Rule::super_property__yield => {
                    build_ast_from_super_property(pair_1)?
                }
                Rule::meta_property => Node::Terminal(Data::NewDotTarget),
                Rule::primary_expression | Rule::primary_expression__yield => {
                    build_ast_from_primary_expression(pair_1)?
                }
                _ => return Err(get_unexpected_error(21, &pair_1)),
            };
            for pair in pair_iter {
                obj = match pair.as_rule() {
                    Rule::expression__in_yield | Rule::expression__in => {
                        Node::MemberAccessBracket {
                            object: Box::new(obj),
                            member: Box::new(build_ast_from_expression(pair)?),
                        }
                    }
                    Rule::identifier_name => Node::MemberAccessDot {
                        object: Box::new(obj),
                        member: get_identifier_name(pair),
                    },
                    Rule::template_literal | Rule::template_literal__yield => Node::TaggedLiteral {
                        f: Box::new(obj),
                        template: Box::new(build_ast_from_template_literal(pair)?),
                    },
                    _ => return Err(get_unexpected_error(12, &pair)),
                };
            }
            obj
        }
    })
}

fn get_arguments(pair: Pair<Rule>) -> Result<Vec<ArgumentType>, Error<Rule>> {
    let mut arguments = vec![];
    if let Some(argument_list_pair) = pair.into_inner().next() {
        let mut is_rest_exp = false;
        for inner_pair in argument_list_pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::rest_operator => {
                    is_rest_exp = true;
                }
                _ => {
                    if is_rest_exp {
                        arguments.push(ArgumentType::Rest(Box::new(
                            build_ast_from_assignment_expression(inner_pair)?,
                        )));
                        is_rest_exp = false;
                    } else {
                        arguments.push(ArgumentType::Regular(Box::new(
                            build_ast_from_assignment_expression(inner_pair)?,
                        )));
                    }
                }
            };
        }
    }
    Ok(arguments)
}

fn build_ast_from_new_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(
        if inner_pair.as_rule() == Rule::member_expression
            || inner_pair.as_rule() == Rule::member_expression__yield
        {
            build_ast_from_member_expression(inner_pair)?
        } else {
            Node::NewExpression(Box::new(build_ast_from_new_expression(inner_pair)?))
        },
    )
}

fn build_ast_from_postfix_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let lhs = build_ast_from_left_hand_side_expression(pair_iter.next().unwrap())?;
    if let Some(op_pair) = pair_iter.next() {
        Ok(Node::PostfixExpression(
            Box::new(lhs),
            match op_pair.as_str() {
                "++" => PostfixOp::PlusPlus,
                "--" => PostfixOp::MinusMinus,
                _ => return Err(get_unexpected_error(11, &op_pair)),
            },
        ))
    } else {
        Ok(lhs)
    }
}

fn build_ast_from_unary_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operators = vec![];
    let mut inner_pairs: Vec<Pair<Rule>> = pair.into_inner().collect();
    let postfix_node = build_ast_from_postfix_expression(inner_pairs.pop().unwrap())?;
    if inner_pairs.len() > 0 {
        for inner_pair in inner_pairs {
            operators.push(match inner_pair.as_str() {
                "delete" => UnaryOp::Delete,
                "void" => UnaryOp::Void,
                "typeof" => UnaryOp::Typeof,
                "++" => UnaryOp::PlusPlus,
                "--" => UnaryOp::MinusMinus,
                "+" => UnaryOp::Plus,
                "-" => UnaryOp::Minus,
                "~" => UnaryOp::BitwiseNot,
                "!" => UnaryOp::LogicalNot,
                _ => return Err(get_unexpected_error(10, &inner_pair)),
            });
        }
    }
    if operators.len() > 0 {
        Ok(Node::UnaryExpression(operators, Box::new(postfix_node)))
    } else {
        Ok(postfix_node)
    }
}

fn build_ast_from_multiplicative_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_unary_expression(pair_iter.next().unwrap())?;
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            operands.push(match inner_pair.as_str() {
                "*" => MultiplicativeOp::Multiply(Box::new(build_ast_from_unary_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "/" => MultiplicativeOp::Divide(Box::new(build_ast_from_unary_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "%" => MultiplicativeOp::Modulo(Box::new(build_ast_from_unary_expression(
                    pair_iter.next().unwrap(),
                )?)),
                _ => return Err(get_unexpected_error(9, &inner_pair)),
            });
        } else {
            break;
        }
    }
    if operands.len() > 0 {
        Ok(Node::MultiplicativeExpression(
            Box::new(first_one),
            operands,
        ))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_additive_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_multiplicative_expression(pair_iter.next().unwrap())?;
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            operands.push(match inner_pair.as_str() {
                "+" => AdditiveOp::Add(Box::new(build_ast_from_multiplicative_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "-" => AdditiveOp::Subtract(Box::new(build_ast_from_multiplicative_expression(
                    pair_iter.next().unwrap(),
                )?)),
                _ => return Err(get_unexpected_error(8, &inner_pair)),
            });
        } else {
            break;
        }
    }
    if operands.len() > 0 {
        Ok(Node::AdditiveExpression(Box::new(first_one), operands))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_shift_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_additive_expression(pair_iter.next().unwrap())?;
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            operands.push(match inner_pair.as_str() {
                "<<" => ShiftOp::LeftShift(Box::new(build_ast_from_additive_expression(
                    pair_iter.next().unwrap(),
                )?)),
                ">>" => ShiftOp::RightShift(Box::new(build_ast_from_additive_expression(
                    pair_iter.next().unwrap(),
                )?)),
                ">>>" => ShiftOp::UnsignedRightShift(Box::new(build_ast_from_additive_expression(
                    pair_iter.next().unwrap(),
                )?)),
                _ => return Err(get_unexpected_error(7, &inner_pair)),
            });
        } else {
            break;
        }
    }
    if operands.len() > 0 {
        Ok(Node::ShiftExpression(Box::new(first_one), operands))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_relational_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_shift_expression(pair_iter.next().unwrap())?;
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            operands.push(match inner_pair.as_str() {
                "<" => RelationalOp::LessThan(Box::new(build_ast_from_shift_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "<=" => RelationalOp::LessThanOrEqual(Box::new(build_ast_from_shift_expression(
                    pair_iter.next().unwrap(),
                )?)),
                ">" => RelationalOp::GreaterThan(Box::new(build_ast_from_shift_expression(
                    pair_iter.next().unwrap(),
                )?)),
                ">=" => RelationalOp::GreaterThanOrEqual(Box::new(
                    build_ast_from_shift_expression(pair_iter.next().unwrap())?,
                )),
                "instanceof" => RelationalOp::InstanceOf(Box::new(
                    build_ast_from_shift_expression(pair_iter.next().unwrap())?,
                )),
                "in" => RelationalOp::In(Box::new(build_ast_from_shift_expression(
                    pair_iter.next().unwrap(),
                )?)),
                _ => return Err(get_unexpected_error(6, &inner_pair)),
            });
        } else {
            break;
        }
    }
    if operands.len() > 0 {
        Ok(Node::RelationalExpression(Box::new(first_one), operands))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_equality_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_relational_expression(pair_iter.next().unwrap())?;
    loop {
        if let Some(inner_pair) = pair_iter.next() {
            operands.push(match inner_pair.as_str() {
                "===" => EqualityOp::StrictEqual(Box::new(build_ast_from_relational_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "!==" => EqualityOp::StrictUnequal(Box::new(build_ast_from_relational_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "==" => EqualityOp::SoftEqual(Box::new(build_ast_from_relational_expression(
                    pair_iter.next().unwrap(),
                )?)),
                "!=" => EqualityOp::StrictUnequal(Box::new(build_ast_from_relational_expression(
                    pair_iter.next().unwrap(),
                )?)),
                _ => return Err(get_unexpected_error(5, &inner_pair)),
            });
        } else {
            break;
        }
    }
    if operands.len() > 0 {
        Ok(Node::EqualityExpression(Box::new(first_one), operands))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_bitwise_and_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_equality_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands.pop().unwrap())
    } else {
        Ok(Node::BitwiseAndExpression(operands))
    }
}

fn build_ast_from_bitwise_xor_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_bitwise_and_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands.pop().unwrap())
    } else {
        Ok(Node::BitwiseXorExpression(operands))
    }
}

fn build_ast_from_bitwise_or_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_bitwise_xor_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands.pop().unwrap())
    } else {
        Ok(Node::BitwiseOrExpression(operands))
    }
}

fn build_ast_from_logical_and_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_bitwise_or_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands.pop().unwrap())
    } else {
        Ok(Node::LogicalAndExpression(operands))
    }
}

fn build_ast_from_logical_or_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_logical_and_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands.pop().unwrap())
    } else {
        Ok(Node::LogicalOrExpression(operands))
    }
}

fn build_ast_from_conditional_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let logical_or_pair = pair_iter.next().unwrap();
    let or_node = build_ast_from_logical_or_expression(logical_or_pair)?;
    if let Some(inner_pair) = pair_iter.next() {
        let truthy = build_ast_from_assignment_expression(inner_pair)?;
        let falsy = build_ast_from_assignment_expression(pair_iter.next().unwrap())?;
        Ok(Node::ConditionalExpression {
            condition: Box::new(or_node),
            if_true: Box::new(truthy),
            if_false: Box::new(falsy),
        })
    } else {
        Ok(or_node)
    }
}

fn build_ast_from_left_hand_side_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    match inner_pair.as_rule() {
        Rule::call_expression | Rule::call_expression__yield => {
            build_ast_from_call_expression(inner_pair)
        }
        Rule::new_expression | Rule::new_expression__yield => {
            build_ast_from_new_expression(inner_pair)
        }
        _ => Err(get_unexpected_error(4, &inner_pair)),
    }
}

fn build_ast_from_arrow_function(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    //TODO
    Ok(Node::Terminal(Data::NullLiteral))
}

fn build_ast_from_assignment_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::arrow_function
        | Rule::arrow_function__in
        | Rule::arrow_function__yield
        | Rule::arrow_function__in_yield => build_ast_from_arrow_function(inner_pair)?,
        Rule::left_hand_side_expression | Rule::left_hand_side_expression__yield => {
            build_ast_from_left_hand_side_expression(inner_pair)?
        }
        Rule::yield_expression | Rule::yield_expression__in => {
            match inner_pair.into_inner().next() {
                Some(inner_pair) => {
                    let n = build_ast_from_assignment_expression(inner_pair)?;
                    Node::YieldExpression(Some(Box::new(n)))
                }
                None => Node::YieldExpression(None),
            }
        }
        Rule::conditional_expression
        | Rule::conditional_expression__in
        | Rule::conditional_expression__yield
        | Rule::conditional_expression__in_yield => {
            build_ast_from_conditional_expression(inner_pair)?
        }
        _ => return Err(get_unexpected_error(20, &inner_pair)),
    })
}

fn build_ast_from_template_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let inner_pair = pair_iter.next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::no_substitution_template => {
            let s = inner_pair.as_str();
            Node::Terminal(Data::StringLiteral(String::from(&s[1..s.len() - 1])))
        }
        Rule::template_head => {
            let s = inner_pair.as_str();
            let mut node_children = vec![Box::new(Node::Terminal(Data::StringLiteral(
                String::from(&s[1..s.len() - 2]),
            )))];
            for template_pair in pair_iter {
                node_children.push(Box::new(match template_pair.as_rule() {
                    Rule::template_middle => {
                        Node::Terminal(Data::StringLiteral(String::from(&s[1..s.len() - 2])))
                    }
                    Rule::expression__in | Rule::expression__in_yield => {
                        build_ast_from_expression(template_pair)?
                    }
                    Rule::template_tail => {
                        Node::Terminal(Data::StringLiteral(String::from(&s[1..s.len() - 1])))
                    }
                    _ => {
                        return Err(get_unexpected_error(3, &template_pair));
                    }
                }));
            }
            Node::TemplateLiteral(node_children)
        }
        _ => {
            return Err(get_unexpected_error(2, &inner_pair));
        }
    })
}

fn build_ast_from_string_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let s = pair.as_str();
    Ok(Node::Terminal(Data::StringLiteral(String::from(
        &s[1..s.len() - 1],
    ))))
}

fn build_ast_from_numeric_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(Node::Terminal(match inner_pair.as_rule() {
        Rule::binary_integer_literal => {
            Data::IntegerLiteral(isize::from_str_radix(&inner_pair.as_str()[2..], 2).unwrap() as i32)
        }
        Rule::octal_integer_literal => {
            Data::IntegerLiteral(isize::from_str_radix(&inner_pair.as_str()[2..], 8).unwrap() as i32)
        }
        Rule::hex_integer_literal => Data::IntegerLiteral(
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
                    _ => return Err(get_unexpected_error(19, &decimal_pair)),
                }
            }
            if !is_float {
                Data::IntegerLiteral(num as i32)
            } else {
                Data::FloatLiteral(num)
            }
        }
        _ => {
            return Err(get_unexpected_error(1, &inner_pair));
        }
    }))
}
