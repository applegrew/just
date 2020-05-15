use super::ast::*;
use pest::error::{Error, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::time::Instant;

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
        string_pads.push('_');
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

pub fn parse_to_ast(script: &str) /*-> Result<Node, Error<Rule>>*/
{
    let pairs = JsParser::parse(Rule::script, script)?;
    let mut stack = Vec::with_capacity(100);
    stack.push(pairs);
    while !stack.is_empty() {
        let pairs = stack.pop().unwrap();
        let mut node_children = vec![];
        for pair in pairs {
            let n = match pair.as_rule() {
                Rule::numeric_literal => build_ast_from_numeric_literal(pair)?,
                Rule::string_literal => build_ast_from_string_literal(pair)?,
                Rule::boolean_literal => {
                    let bool = pair.as_str();
                    Node::Terminal(Data::BooleanLiteral(bool == "true"))
                }
                Rule::null_literal => Node::Terminal(Data::NullLiteral),
                Rule::template_literal => build_ast_from_template_literal(pair)?,
            };
            node_children.push(Box::new(n));
        }
        //let parent_node = Node::Group(node_children);
    }
}

fn get_unexpected_error(id: i32, pair: Pair<Rule>) -> Error<Rule> {
    let message = format!("Unexpected state reached - {}", id);
    Error::new_from_span(ErrorVariant::CustomError { message }, pair.as_span())
}

fn build_ast_from_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut node_children = vec![];
    for inner_pair in pair.into_inner() {
        node_children.push(Box::new(build_ast_from_assignment_expression(inner_pair)?));
    }
    Ok(Node::Group(NodeType::Expression(node_children)))
}

fn build_ast_from_call_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let pair = pair_iter.next().unwrap();
    let mut obj = if pair.as_rule() == Rule::super_call {
        Node::Group(NodeType::CallExpression {
            f: Box::new(Node::Terminal(Data::Super)),
            arguments: get_arguments(pair.into_inner().next().unwrap())?,
        })
    } else {
        Node::Group(NodeType::CallExpression {
            f: Box::new(build_ast_from_member_expression(pair)?),
            arguments: get_arguments(pair_iter.next().unwrap())?,
        })
    };
    for pair in pair_iter {
        obj = match pair.as_rule() {
            Rule::expression__in_yield | Rule::expression__in => {
                Node::Group(NodeType::MemberAccessBracket {
                    object: Box::new(obj),
                    member: Box::new(build_ast_from_expression(pair)?),
                })
            }
            Rule::identifier_name => Node::Group(NodeType::MemberAccessDot {
                object: Box::new(obj),
                member: get_identifier_name(pair),
            }),
            Rule::template_literal | Rule::template_literal__yield => {
                Node::Group(NodeType::TaggedLiteral {
                    f: Box::new(obj),
                    template: Box::new(build_ast_from_template_literal(pair)?),
                })
            }
            Rule::arguments | Rule::arguments__yield => {
                Node::Group(NodeType::CallExpression {
                    f: Box::new(obj),
                    arguments: get_arguments(pair)?,
                })
            }
            _ => return Err(get_unexpected_error(14, pair)),
        };
    }
    Ok(obj)
}

fn build_ast_from_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::null_literal => Node::Terminal(Data::NullLiteral),
        Rule::numeric_literal => build_ast_from_numeric_literal(pair)?,
        Rule::string_literal => build_ast_from_string_literal(pair)?,
        Rule::boolean_literal => {
            let bool = pair.as_str();
            Node::Terminal(Data::BooleanLiteral(bool == "true"))
        }
        _ => return Err(get_unexpected_error(14, inner_pair)),
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
    Ok(Node::Group(NodeType::ArrayLiteral(arguments)))
}

// fn build_ast_from_object_literal(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
//     let mut p = vec![];
//     if let Some(inner_pair) = pair.into_inner().next() {
//         for pair in inner_pair.into_inner() {
//             p.push(build_ast_from_property_definition(pair)?);
//         }
//     }
//     Ok(Node::Group(NodeType::ObjectLiteral(p)))
// }
//
// fn get_property_definition(pair: Pair<Rule>) -> Result<PropertyType, Error<Rule>> {
//     let inner_pair = pair.into_inner().next().unwrap();
//
// }

fn build_ast_from_function_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {}

fn build_ast_from_primary_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(match inner_pair.as_rule() {
        Rule::identifier_reference | Rule::identifier_reference__yield => {
            build_ast_from_identifier_reference(inner_pair)?
        }
        Rule::literal => build_ast_from_literal(inner_pair),
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
        _ => return Err(get_unexpected_error(13, inner_pair)),
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
}

fn build_ast_from_super_property(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let inner_pair = pair.into_inner().next().unwrap();
    Ok(Node::Group(match inner_pair.as_rule() {
        Rule::identifier_name => NodeType::MemberAccessDot {
            object: Box::new(Node::Terminal(Data::Super)),
            member: get_identifier_name(inner_pair),
        },
        _ => NodeType::MemberAccessBracket {
            object: Box::new(Node::Terminal(Data::Super)),
            member: Box::new(build_ast_from_expression(inner_pair)?),
        },
    }))
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
        Rule::member_expression | Rule::member_expression__yield => {
            Node::Group(NodeType::InstantiateObject {
                constructor: Box::new(build_ast_from_member_expression(pair_1)?),
                arguments: get_arguments(pair_iter.next().unwrap())?,
            })
        }
        _ => {
            let mut obj = Box::new(match pair_1.as_rule() {
                Rule::super_property | Rule::super_property__yield => {
                    build_ast_from_super_property(pair_1)?
                }
                Rule::meta_property => Node::Terminal(Data::New_Dot_Target),
                Rule::primary_expression | Rule::primary_expression__yield => {
                    build_ast_from_primary_expression(pair_1)?
                }
            });
            for pair in pair_iter {
                obj = Box::new(match pair.as_rule() {
                    Rule::expression__in_yield | Rule::expression__in => {
                        Node::Group(NodeType::MemberAccessBracket {
                            object: obj,
                            member: Box::new(build_ast_from_expression(pair)?),
                        })
                    }
                    Rule::identifier_name => Node::Group(NodeType::MemberAccessDot {
                        object: obj,
                        member: get_identifier_name(pair),
                    }),
                    Rule::template_literal | Rule::template_literal__yield => {
                        Node::Group(NodeType::TaggedLiteral {
                            f: obj,
                            template: Box::new(build_ast_from_template_literal(pair)?),
                        })
                    }
                    _ => return Err(get_unexpected_error(12, pair)),
                });
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
    let inner_pairs = pair.into_inner().collect();
    let member_node = build_ast_from_member_expression(inner_pairs.last().unwrap())?;
    if inner_pairs.len() > 1 {
        let mut n = Node::Group(NodeType::NewExpression(Box::new(member_node)));
        for _ in [1..inner_pairs.len() - 1] {
            n = Node::Group(NodeType::NewExpression(Box::new(n)));
        }
        Ok(n)
    } else {
        Ok(member_node)
    }
}

fn build_ast_from_postfix_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let lhs = build_ast_from_left_hand_side_expression(pair_iter.next().unwrap())?;
    if let Some(op_pair) = pair_iter.next() {
        Ok(Node::Group(NodeType::PostfixExpression(
            Box::new(lhs),
            match op_pair.as_str() {
                "++" => PostfixOp::PlusPlus,
                "--" => PostfixOp::MinusMinus,
                _ => return Err(get_unexpected_error(11, op_pair)),
            },
        )))
    } else {
        Ok(lhs)
    }
}

fn build_ast_from_unary_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operators = vec![];
    let inner_pairs = pair.into_inner().collect();
    if inner_pairs.len() > 1 {
        for inner_pair in &inner_pairs[..inner_pairs.len() - 1] {
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
                _ => return Err(get_unexpected_error(10, inner_pair)),
            });
        }
    }
    let postfix_node = build_ast_from_postfix_expression(inner_pairs.last().unwrap())?;
    if operators.len() > 0 {
        Ok(Node::Group(NodeType::UnaryExpression(
            operators,
            Box::new(postfix_node),
        )))
    } else {
        Ok(postfix_node)
    }
}

fn build_ast_from_multiplicative_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_unary_expression(pair_iter.next().unwrap())?;
    for inner_pair in pair_iter {
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
            _ => return Err(get_unexpected_error(9, inner_pair)),
        });
    }
    if operands.len() > 0 {
        Ok(Node::Group(NodeType::MultiplicativeExpression(
            Box::new(first_one),
            operands,
        )))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_additive_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_multiplicative_expression(pair_iter.next().unwrap())?;
    for inner_pair in pair_iter {
        operands.push(match inner_pair.as_str() {
            "+" => AdditiveOp::Add(Box::new(build_ast_from_multiplicative_expression(
                pair_iter.next().unwrap(),
            )?)),
            "-" => AdditiveOp::Subtract(Box::new(build_ast_from_multiplicative_expression(
                pair_iter.next().unwrap(),
            )?)),
            _ => return Err(get_unexpected_error(8, inner_pair)),
        });
    }
    if operands.len() > 0 {
        Ok(Node::Group(NodeType::AdditiveExpression(
            Box::new(first_one),
            operands,
        )))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_shift_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_additive_expression(pair_iter.next().unwrap())?;
    for inner_pair in pair_iter {
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
            _ => return Err(get_unexpected_error(7, inner_pair)),
        });
    }
    if operands.len() > 0 {
        Ok(Node::Group(NodeType::ShiftExpression(
            Box::new(first_one),
            operands,
        )))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_relational_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_shift_expression(pair_iter.next().unwrap())?;
    for inner_pair in pair_iter {
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
            ">=" => RelationalOp::GreaterThanOrEqual(Box::new(build_ast_from_shift_expression(
                pair_iter.next().unwrap(),
            )?)),
            "instanceof" => RelationalOp::InstanceOf(Box::new(build_ast_from_shift_expression(
                pair_iter.next().unwrap(),
            )?)),
            "in" => RelationalOp::In(Box::new(build_ast_from_shift_expression(
                pair_iter.next().unwrap(),
            )?)),
            _ => return Err(get_unexpected_error(6, inner_pair)),
        });
    }
    if operands.len() > 0 {
        Ok(Node::Group(NodeType::RelationalExpression(
            Box::new(first_one),
            operands,
        )))
    } else {
        Ok(first_one)
    }
}

fn build_ast_from_equality_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    let mut pair_iter = pair.into_inner();
    let first_one = build_ast_from_relational_expression(pair_iter.next().unwrap())?;
    for inner_pair in pair_iter {
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
            _ => return Err(get_unexpected_error(5, inner_pair)),
        });
    }
    if operands.len() > 0 {
        Ok(Node::Group(NodeType::EqualityExpression(
            Box::new(first_one),
            operands,
        )))
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
        Ok(*operands[0])
    } else {
        Ok(Node::Group(NodeType::BitwiseAndExpression(operands)))
    }
}

fn build_ast_from_bitwise_xor_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_bitwise_and_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands[0])
    } else {
        Ok(Node::Group(NodeType::BitwiseXorExpression(operands)))
    }
}

fn build_ast_from_bitwise_or_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_bitwise_xor_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands[0])
    } else {
        Ok(Node::Group(NodeType::BitwiseOrExpression(operands)))
    }
}

fn build_ast_from_logical_and_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_bitwise_or_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands[0])
    } else {
        Ok(Node::Group(NodeType::LogicalAndExpression(operands)))
    }
}

fn build_ast_from_logical_or_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut operands = vec![];
    for inner_pair in pair.into_inner() {
        operands.push(Box::new(build_ast_from_logical_and_expression(inner_pair)?));
    }
    if operands.len() == 1 {
        Ok(*operands[0])
    } else {
        Ok(Node::Group(NodeType::LogicalOrExpression(operands)))
    }
}

fn build_ast_from_conditional_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut pair_iter = pair.into_inner();
    let logical_or_pair = pair_iter.next().unwrap();
    let or_node = build_ast_from_logical_or_expression(logical_or_pair)?;
    if let Some(inner_pair) = pair_iter.next() {
        let truthy = build_ast_from_assignment_expression(inner_pair)?;
        let falsy = build_ast_from_assignment_expression(pair_iter.next().unwrap())?;
        Ok(Node::Group(NodeType::ConditionalExpression {
            condition: Box::new(or_node),
            if_true: Box::new(truthy),
            if_false: Box::new(falsy),
        }))
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
        _ => Err(get_unexpected_error(4, inner_pair)),
    }
}

fn build_ast_from_arrow_function(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {}

fn build_ast_from_assignment_expression(pair: Pair<Rule>) -> Result<Node, Error<Rule>> {
    let mut node_children: Vec<Box<Node>> = vec![];
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
                    Node::Group(NodeType::YieldExpression(Some(Box::new(n))))
                }
                None => Node::Group(NodeType::YieldExpression(None)),
            }
        }
        Rule::conditional_expression
        | Rule::conditional_expression__in
        | Rule::conditional_expression__yield
        | Rule::conditional_expression__in_yield => {
            build_ast_from_conditional_expression(inner_pair)?
        }
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
                        return Err(get_unexpected_error(3, template_pair));
                    }
                }));
            }
            Node::Group(NodeType::TemplateLiteral(node_children))
        }
        _ => {
            return Err(get_unexpected_error(2, inner_pair));
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
            for decimal_pair in pair.into_inner() {
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
                }
            }
            if !is_float {
                Data::IntegerLiteral(num as i32)
            } else {
                Data::FloatLiteral(num)
            }
        }
        _ => {
            return Err(get_unexpected_error(1, inner_pair));
        }
    }))
}
