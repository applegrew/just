use super::api::JsParser;
use super::api::Rule;

use pest::consumes_to;
use pest::fails_with;
use pest::parses_to;
use std::time::Instant;

#[test]
fn test_decimal_number_with_no_dot() {
    parses_to! {
        parser: JsParser,
        input: "10",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 2, [
                decimal_literal(0, 2, [
                    decimal_integer_literal(0, 2)
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_dot() {
    parses_to! {
        parser: JsParser,
        input: "10.0",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 4, [
                decimal_literal(0, 4, [
                    decimal_integer_literal(0, 2),
                    decimal_digits(3, 4)
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_dot_with_longer_fraction() {
    parses_to! {
        parser: JsParser,
        input: "10.001",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 6, [
                decimal_literal(0, 6, [
                    decimal_integer_literal(0, 2),
                    decimal_digits(3, 6)
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_dot_at_start() {
    parses_to! {
        parser: JsParser,
        input: ".1",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 2, [
                decimal_literal(0, 2, [
                    decimal_digits(1, 2)
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_dot_with_exp() {
    parses_to! {
        parser: JsParser,
        input: "1.123e10",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 8, [
                decimal_literal(0, 8, [
                    decimal_integer_literal(0, 1),
                    decimal_digits(2, 5),
                    exponent_part(5, 8, [
                        signed_integer(6, 8, [
                            decimal_digits(6, 8),
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_dot_at_start_with_exp() {
    parses_to! {
        parser: JsParser,
        input: ".123E1",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 6, [
                decimal_literal(0, 6, [
                    decimal_digits(1, 4),
                    exponent_part(4, 6, [
                        signed_integer(5, 6, [
                            decimal_digits(5, 6),
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_no_dot_with_exp() {
    parses_to! {
        parser: JsParser,
        input: "12E007",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 6, [
                decimal_literal(0, 6, [
                    decimal_integer_literal(0, 2),
                    exponent_part(2, 6, [
                        signed_integer(3, 6, [
                            decimal_digits(3, 6),
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_no_dot_with_positive_exp() {
    parses_to! {
        parser: JsParser,
        input: "12E+7",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 5, [
                decimal_literal(0, 5, [
                    decimal_integer_literal(0, 2),
                    exponent_part(2, 5, [
                        signed_integer(3, 5, [
                            decimal_digits(4, 5),
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_decimal_number_with_no_dot_with_negative_exp() {
    parses_to! {
        parser: JsParser,
        input: "12E-7",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 5, [
                decimal_literal(0, 5, [
                    decimal_integer_literal(0, 2),
                    exponent_part(2, 5, [
                        signed_integer(3, 5, [
                            decimal_digits(4, 5),
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_binary_number() {
    parses_to! {
        parser: JsParser,
        input: "0b10111",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 7, [
                binary_integer_literal(0, 7)
            ])
        ]
    };
    parses_to! {
        parser: JsParser,
        input: "0B10111",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 7, [
                binary_integer_literal(0, 7)
            ])
        ]
    };
}

#[test]
fn test_octal_number() {
    parses_to! {
        parser: JsParser,
        input: "0o01234567",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 10, [
                octal_integer_literal(0, 10)
            ])
        ]
    };
    parses_to! {
        parser: JsParser,
        input: "0O01234567",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 10, [
                octal_integer_literal(0, 10)
            ])
        ]
    };
}

#[test]
fn test_hex_number() {
    parses_to! {
        parser: JsParser,
        input: "0x0123456789abcdef",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 18, [
                hex_integer_literal(0, 18)
            ])
        ]
    };
    parses_to! {
        parser: JsParser,
        input: "0X0123456789ABCDEF",
        rule: Rule::numeric_literal,
        tokens: [
            numeric_literal(0, 18, [
                hex_integer_literal(0, 18)
            ])
        ]
    };
}

#[test]
fn test_string1() {
    parses_to! {
        parser: JsParser,
        input: "'test single string'",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 20)
        ]
    };
}

#[test]
fn test_string2() {
    parses_to! {
        parser: JsParser,
        input: "\"test single string\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 20)
        ]
    };
}

#[test]
fn test_string3() {
    parses_to! {
        parser: JsParser,
        input: "'test single\" string'",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 21)
        ]
    };
}

#[test]
fn test_string4() {
    parses_to! {
        parser: JsParser,
        input: "\"test single' string\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 21)
        ]
    };
}

#[test]
fn test_string5() {
    parses_to! {
        parser: JsParser,
        input: "\"test single\\\" string\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 22)
        ]
    };
}

#[test]
fn test_string6() {
    parses_to! {
        parser: JsParser,
        input: "\"test single\\\n string\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 22)
        ]
    };
}

#[test]
fn test_string7() {
    fails_with! {
        parser: JsParser,
        input: "\"test single\n string\"",
        rule: Rule::string_literal,
        positives: vec![Rule::string_literal],
        negatives: vec![],
        pos: 0
    };
}

#[test]
fn test_string8() {
    parses_to! {
        parser: JsParser,
        input: "\"test single\\t string\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 22)
        ]
    };
}

#[test]
fn test_string9() {
    parses_to! {
        parser: JsParser,
        input: "\"আমি বাঙালি\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 30)
        ]
    };
}

#[test]
fn test_string10() {
    parses_to! {
        parser: JsParser,
        input: "\"\"",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 2)
        ]
    };
    parses_to! {
        parser: JsParser,
        input: "''",
        rule: Rule::string_literal,
        tokens: [
            string_literal(0, 2)
        ]
    };
}

#[test]
fn test_regex_exp1() {
    parses_to! {
        parser: JsParser,
        input: "/regexExp/",
        rule: Rule::regular_expression_literal,
        tokens: [
            regular_expression_literal(0, 10, [
                regular_expression_body(1, 9)
            ])
        ]
    };
}

#[test]
fn test_regex_exp2() {
    parses_to! {
        parser: JsParser,
        input: "/regexExp/flag",
        rule: Rule::regular_expression_literal,
        tokens: [
            regular_expression_literal(0, 14, [
                regular_expression_body(1, 9),
                regular_expression_flags(10, 14)
            ])
        ]
    };
}

#[test]
fn test_regex_exp3() {
    parses_to! {
        parser: JsParser,
        input: "/ab+c(?<=y)x\\n*{x,y}()-!/",
        rule: Rule::regular_expression_literal,
        tokens: [
            regular_expression_literal(0, 25, [
                regular_expression_body(1, 24)
            ])
        ]
    };
}

#[test]
fn test_regex_exp4() {
    parses_to! {
        parser: JsParser,
        input: "/\\//",
        rule: Rule::regular_expression_literal,
        tokens: [
            regular_expression_literal(0, 4, [
                regular_expression_body(1, 3)
            ])
        ]
    };
}

#[test]
fn test_template_string1() {
    parses_to! {
        parser: JsParser,
        input: "``",
        rule: Rule::template_literal,
        tokens: [
            template_literal(0, 2, [
                no_substitution_template(0, 2)
            ])
        ]
    };
}

#[test]
fn test_template_string2() {
    parses_to! {
        parser: JsParser,
        input: "`Testing template with embedded quotes (') and double quotes (\") and new lines \n and tabs \t and escaped backquotes \\` and more - আমি বাঙালি`",
        rule: Rule::template_literal,
        tokens: [
            template_literal(0, 158, [
                no_substitution_template(0, 158)
            ])
        ]
    };
}

#[test]
fn test_template_string3() {
    parses_to! {
        parser: JsParser,
        input: "`Testing template with ${tokens} in them ${token[1]} xyz`",
        rule: Rule::template_literal,
        tokens: [
            template_literal(0, 57, [
                template_head(0, 25, [
                    template_characters(1, 23)
                ]),
                expression__in(25, 31, [
                    assignment_expression__in(25, 31, [
                        conditional_expression__in(25, 31, [
                            logical_or_expression__in(25, 31, [
                                logical_and_expression__in(25, 31, [
                                    bitwise_or_expression__in(25, 31, [
                                        bitwise_xor_expression__in(25, 31, [
                                            bitwise_and_expression__in(25, 31, [
                                                equality_expression__in(25, 31, [
                                                    relational_expression__in(25, 31, [
                                                        shift_expression(25, 31, [
                                                            additive_expression(25, 31, [
                                                                multiplicative_expression(25, 31, [
                                                                    unary_expression(25, 31, [
                                                                        postfix_expression(25, 31, [
                                                                            left_hand_side_expression(25, 31, [
                                                                                new_expression(25, 31, [
                                                                                    member_expression(25, 31, [
                                                                                        primary_expression(25, 31, [
                                                                                            identifier_reference(25, 31)
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                template_middle(31, 43, [
                    template_characters(32, 41)
                ]),
                expression__in(43, 51, [
                    assignment_expression__in(43, 51, [
                        conditional_expression__in(43, 51, [
                            logical_or_expression__in(43, 51, [
                                logical_and_expression__in(43, 51, [
                                    bitwise_or_expression__in(43, 51, [
                                        bitwise_xor_expression__in(43, 51, [
                                            bitwise_and_expression__in(43, 51, [
                                                equality_expression__in(43, 51, [
                                                    relational_expression__in(43, 51, [
                                                        shift_expression(43, 51, [
                                                            additive_expression(43, 51, [
                                                                multiplicative_expression(43, 51, [
                                                                    unary_expression(43, 51, [
                                                                        postfix_expression(43, 51, [
                                                                            left_hand_side_expression(43, 51, [
                                                                                new_expression(43, 51, [
                                                                                    member_expression(43, 51, [
                                                                                        primary_expression(43, 48, [
                                                                                            identifier_reference(43, 48)
                                                                                        ]),
                                                                                        expression__in(49, 50, [
                                                                                            assignment_expression__in(49, 50, [
                                                                                                conditional_expression__in(49, 50, [
                                                                                                    logical_or_expression__in(49, 50, [
                                                                                                        logical_and_expression__in(49, 50, [
                                                                                                            bitwise_or_expression__in(49, 50, [
                                                                                                                bitwise_xor_expression__in(49, 50, [
                                                                                                                    bitwise_and_expression__in(49, 50, [
                                                                                                                        equality_expression__in(49, 50, [
                                                                                                                            relational_expression__in(49, 50, [
                                                                                                                                shift_expression(49, 50, [
                                                                                                                                    additive_expression(49, 50, [
                                                                                                                                        multiplicative_expression(49, 50, [
                                                                                                                                            unary_expression(49, 50, [
                                                                                                                                                postfix_expression(49, 50, [
                                                                                                                                                    left_hand_side_expression(49, 50, [
                                                                                                                                                        new_expression(49, 50, [
                                                                                                                                                            member_expression(49, 50, [
                                                                                                                                                                primary_expression(49, 50, [
                                                                                                                                                                    literal(49, 50, [
                                                                                                                                                                        numeric_literal(49, 50, [
                                                                                                                                                                            decimal_literal(49, 50, [
                                                                                                                                                                                decimal_integer_literal(49, 50)
                                                                                                                                                                            ])
                                                                                                                                                                        ])
                                                                                                                                                                    ])
                                                                                                                                                                ])
                                                                                                                                                            ])
                                                                                                                                                        ])
                                                                                                                                                    ])
                                                                                                                                                ])
                                                                                                                                            ])
                                                                                                                                        ])
                                                                                                                                    ])
                                                                                                                                ])
                                                                                                                            ])
                                                                                                                        ])
                                                                                                                    ])
                                                                                                                ])
                                                                                                            ])
                                                                                                        ])
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                template_tail(51, 57, [
                    template_characters(52, 56)
                ])
            ])
        ]
    };
}

#[test]
fn test_template_string4() {
    parses_to! {
        parser: JsParser,
        input: "`Testing template with ${f1()}`",
        rule: Rule::template_literal,
        tokens: [
            template_literal(0, 31, [
                template_head(0, 25, [
                    template_characters(1, 23)
                ]),
                expression__in(25, 29, [
                    assignment_expression__in(25, 29, [
                        conditional_expression__in(25, 29, [
                            logical_or_expression__in(25, 29, [
                                logical_and_expression__in(25, 29, [
                                    bitwise_or_expression__in(25, 29, [
                                        bitwise_xor_expression__in(25, 29, [
                                            bitwise_and_expression__in(25, 29, [
                                                equality_expression__in(25, 29, [
                                                    relational_expression__in(25, 29, [
                                                        shift_expression(25, 29, [
                                                            additive_expression(25, 29, [
                                                                multiplicative_expression(25, 29, [
                                                                    unary_expression(25, 29, [
                                                                        postfix_expression(25, 29, [
                                                                            left_hand_side_expression(25, 29, [
                                                                                call_expression(25, 29, [
                                                                                    member_expression(25, 27, [
                                                                                        primary_expression(25, 27, [
                                                                                            identifier_reference(25, 27)
                                                                                        ])
                                                                                    ]),
                                                                                    arguments(27, 29)
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                template_tail(29, 31)
            ])
        ]
    };
}

#[test]
fn test_template_string5() {
    fails_with! {
        parser: JsParser,
        input: "`Testing invalid template with non-expression scripts like ${f1(); f2()}`",
        rule: Rule::template_literal,
        positives: vec![Rule::template_middle, Rule::template_tail, Rule::template_literal,
            Rule::arguments, Rule::postfix_operator, Rule::multiplicative_operator, Rule::additive_operator,
            Rule::shift_operator, Rule::relational_operator__in, Rule::equality_operator, Rule::assignment_operator],
        negatives: vec![],
        pos: 65
    };
}

#[test]
fn test_template_string6() {
    fails_with! {
        parser: JsParser,
        input: "`Testing template with ${invalid_tokens_syntax`",
        rule: Rule::template_literal,
        positives: vec![Rule::template_characters],
        negatives: vec![],
        pos: 47
    };
}

#[test]
fn test_array1() {
    parses_to! {
        parser: JsParser,
        input: "[1, 22,3]",
        rule: Rule::array_literal,
        tokens: [
            array_literal(0, 9, [
                assignment_expression__in(1, 2, [
                    conditional_expression__in(1, 2, [
                        logical_or_expression__in(1, 2, [
                            logical_and_expression__in(1, 2, [
                                bitwise_or_expression__in(1, 2, [
                                    bitwise_xor_expression__in(1, 2, [
                                        bitwise_and_expression__in(1, 2, [
                                            equality_expression__in(1, 2, [
                                                relational_expression__in(1, 2, [
                                                    shift_expression(1, 2, [
                                                        additive_expression(1, 2, [
                                                            multiplicative_expression(1, 2, [
                                                                unary_expression(1, 2, [
                                                                    postfix_expression(1, 2, [
                                                                        left_hand_side_expression(1, 2, [
                                                                            new_expression(1, 2, [
                                                                                member_expression(1, 2, [
                                                                                    primary_expression(1, 2, [
                                                                                        literal(1, 2, [
                                                                                            numeric_literal(1, 2, [
                                                                                                decimal_literal(1, 2, [
                                                                                                    decimal_integer_literal(1, 2)
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                assignment_expression__in(4, 6, [
                    conditional_expression__in(4, 6, [
                        logical_or_expression__in(4, 6, [
                            logical_and_expression__in(4, 6, [
                                bitwise_or_expression__in(4, 6, [
                                    bitwise_xor_expression__in(4, 6, [
                                        bitwise_and_expression__in(4, 6, [
                                            equality_expression__in(4, 6, [
                                                relational_expression__in(4, 6, [
                                                    shift_expression(4, 6, [
                                                        additive_expression(4, 6, [
                                                            multiplicative_expression(4, 6, [
                                                                unary_expression(4, 6, [
                                                                    postfix_expression(4, 6, [
                                                                        left_hand_side_expression(4, 6, [
                                                                            new_expression(4, 6, [
                                                                                member_expression(4, 6, [
                                                                                    primary_expression(4, 6, [
                                                                                        literal(4, 6, [
                                                                                            numeric_literal(4, 6, [
                                                                                                decimal_literal(4, 6, [
                                                                                                    decimal_integer_literal(4, 6)
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                assignment_expression__in(7, 8, [
                    conditional_expression__in(7, 8, [
                        logical_or_expression__in(7, 8, [
                            logical_and_expression__in(7, 8, [
                                bitwise_or_expression__in(7, 8, [
                                    bitwise_xor_expression__in(7, 8, [
                                        bitwise_and_expression__in(7, 8, [
                                            equality_expression__in(7, 8, [
                                                relational_expression__in(7, 8, [
                                                    shift_expression(7, 8, [
                                                        additive_expression(7, 8, [
                                                            multiplicative_expression(7, 8, [
                                                                unary_expression(7, 8, [
                                                                    postfix_expression(7, 8, [
                                                                        left_hand_side_expression(7, 8, [
                                                                            new_expression(7, 8, [
                                                                                member_expression(7, 8, [
                                                                                    primary_expression(7, 8, [
                                                                                        literal(7, 8, [
                                                                                            numeric_literal(7, 8, [
                                                                                                decimal_literal(7, 8, [
                                                                                                    decimal_integer_literal(7, 8)
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_function1() {
    parses_to! {
        parser: JsParser,
        input: r#"function test1() {
                a
            }"#,
        rule: Rule::function_expression,
        tokens: [
            function_expression(0, 50, [
                binding_identifier(9, 14),
                formal_parameters(15, 15),
                function_body(35, 49, [
                    statement__return(35, 37, [
                        expression_statement(35, 37, [
                            expression__in(35, 36, [
                                assignment_expression__in(35, 36, [
                                    conditional_expression__in(35, 36, [
                                        logical_or_expression__in(35, 36, [
                                            logical_and_expression__in(35, 36, [
                                                bitwise_or_expression__in(35, 36, [
                                                    bitwise_xor_expression__in(35, 36, [
                                                        bitwise_and_expression__in(35, 36, [
                                                            equality_expression__in(35, 36, [
                                                                relational_expression__in(35, 36, [
                                                                    shift_expression(35, 36, [
                                                                        additive_expression(35, 36, [
                                                                            multiplicative_expression(35, 36, [
                                                                                unary_expression(35, 36, [
                                                                                    postfix_expression(35, 36, [
                                                                                        left_hand_side_expression(35, 36, [
                                                                                            new_expression(35, 36, [
                                                                                                member_expression(35, 36, [
                                                                                                    primary_expression(35, 36, [
                                                                                                        identifier_reference(35, 36)
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                            smart_semicolon(36, 37)
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_function2() {
    parses_to! {
        parser: JsParser,
        input: r#"function test2() {
                return a
            }"#,
        rule: Rule::function_expression,
        tokens: [
            function_expression(0, 57, [
                binding_identifier(9, 14),
                formal_parameters(15, 15),
                function_body(35, 56, [
                    statement__return(35, 44, [
                        return_statement(35, 44, [
                            expression__in(42, 43, [
                                assignment_expression__in(42, 43, [
                                    conditional_expression__in(42, 43, [
                                        logical_or_expression__in(42, 43, [
                                            logical_and_expression__in(42, 43, [
                                                bitwise_or_expression__in(42, 43, [
                                                    bitwise_xor_expression__in(42, 43, [
                                                        bitwise_and_expression__in(42, 43, [
                                                            equality_expression__in(42, 43, [
                                                                relational_expression__in(42, 43, [
                                                                    shift_expression(42, 43, [
                                                                        additive_expression(42, 43, [
                                                                            multiplicative_expression(42, 43, [
                                                                                unary_expression(42, 43, [
                                                                                    postfix_expression(42, 43, [
                                                                                        left_hand_side_expression(42, 43, [
                                                                                            new_expression(42, 43, [
                                                                                                member_expression(42, 43, [
                                                                                                    primary_expression(42, 43, [
                                                                                                        identifier_reference(42, 43)
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                            smart_semicolon(43, 44)
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_function3() {
    parses_to! {
        parser: JsParser,
        input: r#"function test2(x, ...y) {
        }"#,
        rule: Rule::function_expression,
        tokens: [
            function_expression(0, 35, [
                binding_identifier(9, 14),
                formal_parameters(15, 22, [
                    formal_parameter(15, 16, [
                        binding_element(15, 16, [
                            single_name_binding(15, 16, [
                                binding_identifier(15, 16)
                            ])
                        ])
                    ]),
                    function_rest_parameter(18, 22, [
                        binding_rest_element(18, 22, [
                            binding_identifier(21, 22)
                        ])
                    ])
                ]),
                function_body(34, 34)
            ])
        ]
    };
}

#[test]
fn test_var1() {
    parses_to! {
        parser: JsParser,
        input: "var evens = [],\n fives = [];",
        rule: Rule::variable_statement,
        tokens: [
            variable_statement(0, 28, [
                variable_declaration_list__in(4, 27, [
                    variable_declaration__in(4, 14, [
                        binding_identifier(4, 9),
                        initializer__in(10, 14, [
                            assignment_expression__in(12, 14, [
                                conditional_expression__in(12, 14, [
                                    logical_or_expression__in(12, 14, [
                                        logical_and_expression__in(12, 14, [
                                            bitwise_or_expression__in(12, 14, [
                                                bitwise_xor_expression__in(12, 14, [
                                                    bitwise_and_expression__in(12, 14, [
                                                        equality_expression__in(12, 14, [
                                                            relational_expression__in(12, 14, [
                                                                shift_expression(12, 14, [
                                                                    additive_expression(12, 14, [
                                                                        multiplicative_expression(12, 14, [
                                                                            unary_expression(12, 14, [
                                                                                postfix_expression(12, 14, [
                                                                                    left_hand_side_expression(12, 14, [
                                                                                        new_expression(12, 14, [
                                                                                            member_expression(12, 14, [
                                                                                                primary_expression(12, 14, [
                                                                                                    array_literal(12, 14)
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]),
                    variable_declaration__in(17, 27, [
                        binding_identifier(17, 22),
                        initializer__in(23, 27, [
                            assignment_expression__in(25, 27, [
                                conditional_expression__in(25, 27, [
                                    logical_or_expression__in(25, 27, [
                                        logical_and_expression__in(25, 27, [
                                            bitwise_or_expression__in(25, 27, [
                                                bitwise_xor_expression__in(25, 27, [
                                                    bitwise_and_expression__in(25, 27, [
                                                        equality_expression__in(25, 27, [
                                                            relational_expression__in(25, 27, [
                                                                shift_expression(25, 27, [
                                                                    additive_expression(25, 27, [
                                                                        multiplicative_expression(25, 27, [
                                                                            unary_expression(25, 27, [
                                                                                postfix_expression(25, 27, [
                                                                                    left_hand_side_expression(25, 27, [
                                                                                        new_expression(25, 27, [
                                                                                            member_expression(25, 27, [
                                                                                                primary_expression(25, 27, [
                                                                                                    array_literal(25, 27)
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]),
                ]),
                smart_semicolon(27, 28)
            ])
        ]
    };
}

#[test]
fn test_identifier1() {
    parses_to! {
        parser: JsParser,
        input: "thisBinding",
        rule: Rule::identifier,
        tokens: [
            identifier(0, 11)
        ]
    }
}

#[test]
fn test_expression_statement1() {
    parses_to! {
        parser: JsParser,
        input: "some.call",
        rule: Rule::expression_statement,
        tokens: [
            expression_statement(0, 9, [
                expression__in(0, 9, [
                    assignment_expression__in(0, 9, [
                        conditional_expression__in(0, 9, [
                            logical_or_expression__in(0, 9, [
                                logical_and_expression__in(0, 9, [
                                    bitwise_or_expression__in(0, 9, [
                                        bitwise_xor_expression__in(0, 9, [
                                            bitwise_and_expression__in(0, 9, [
                                                equality_expression__in(0, 9, [
                                                    relational_expression__in(0, 9, [
                                                        shift_expression(0, 9, [
                                                            additive_expression(0, 9, [
                                                                multiplicative_expression(0, 9, [
                                                                    unary_expression(0, 9, [
                                                                        postfix_expression(0, 9, [
                                                                            left_hand_side_expression(0, 9, [
                                                                                new_expression(0, 9, [
                                                                                    member_expression(0, 9, [
                                                                                        primary_expression(0, 4, [
                                                                                            identifier_reference(0, 4)
                                                                                        ]),
                                                                                        identifier_name(5, 9)
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                smart_semicolon(9, 9)
            ])
        ]
    }
}

#[test]
fn test_expression_statement2() {
    parses_to! {
        parser: JsParser,
        input: "thisBinding.call()();",
        rule: Rule::expression_statement,
        tokens: [
            expression_statement(0, 21, [
                expression__in(0, 20, [
                    assignment_expression__in(0, 20, [
                        conditional_expression__in(0, 20, [
                            logical_or_expression__in(0, 20, [
                                logical_and_expression__in(0, 20, [
                                    bitwise_or_expression__in(0, 20, [
                                        bitwise_xor_expression__in(0, 20, [
                                            bitwise_and_expression__in(0, 20, [
                                                equality_expression__in(0, 20, [
                                                    relational_expression__in(0, 20, [
                                                        shift_expression(0, 20, [
                                                            additive_expression(0, 20, [
                                                                multiplicative_expression(0, 20, [
                                                                    unary_expression(0, 20, [
                                                                        postfix_expression(0, 20, [
                                                                            left_hand_side_expression(0, 20, [
                                                                                call_expression(0, 20, [
                                                                                    member_expression(0, 16, [
                                                                                        primary_expression(0, 11, [
                                                                                            identifier_reference(0, 11)
                                                                                        ]),
                                                                                        identifier_name(12, 16)
                                                                                    ]),
                                                                                    arguments(16, 18),
                                                                                    arguments(18, 20)
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ]),
                smart_semicolon(20, 21)
            ])
        ]
    };
}

#[test]
fn test_script1() {
    parses_to! {
        parser: JsParser,
        input: "thisBinding.call({ string: \'bound\' })();\n",
        rule: Rule::script,
        tokens: [
            statement(0, 41, [
                expression_statement(0, 41, [
                    expression__in(0, 39, [
                        assignment_expression__in(0, 39, [
                            conditional_expression__in(0, 39, [
                                logical_or_expression__in(0, 39, [
                                    logical_and_expression__in(0, 39, [
                                        bitwise_or_expression__in(0, 39, [
                                            bitwise_xor_expression__in(0, 39, [
                                                bitwise_and_expression__in(0, 39, [
                                                    equality_expression__in(0, 39, [
                                                        relational_expression__in(0, 39, [
                                                            shift_expression(0, 39, [
                                                                additive_expression(0, 39, [
                                                                    multiplicative_expression(0, 39, [
                                                                        unary_expression(0, 39, [
                                                                            postfix_expression(0, 39, [
                                                                                left_hand_side_expression(0, 39, [
                                                                                    call_expression(0, 39, [
                                                                                        member_expression(0, 16, [
                                                                                            primary_expression(0, 11, [
                                                                                                identifier_reference(0, 11)
                                                                                            ]),
                                                                                            identifier_name(12, 16)
                                                                                        ]),
                                                                                        arguments(16, 37, [
                                                                                            argument_list(17, 36, [
                                                                                                assignment_expression__in(17, 36, [
                                                                                                    conditional_expression__in(17, 36, [
                                                                                                        logical_or_expression__in(17, 36, [
                                                                                                            logical_and_expression__in(17, 36, [
                                                                                                                bitwise_or_expression__in(17, 36, [
                                                                                                                    bitwise_xor_expression__in(17, 36, [
                                                                                                                        bitwise_and_expression__in(17, 36, [
                                                                                                                            equality_expression__in(17, 36, [
                                                                                                                                relational_expression__in(17, 36, [
                                                                                                                                    shift_expression(17, 36, [
                                                                                                                                        additive_expression(17, 36, [
                                                                                                                                            multiplicative_expression(17, 36, [
                                                                                                                                                unary_expression(17, 36, [
                                                                                                                                                    postfix_expression(17, 36, [
                                                                                                                                                        left_hand_side_expression(17, 36, [
                                                                                                                                                            new_expression(17, 36, [
                                                                                                                                                                member_expression(17, 36, [
                                                                                                                                                                    primary_expression(17, 36, [
                                                                                                                                                                        object_literal(17, 36, [
                                                                                                                                                                            property_definition_list(19, 35, [
                                                                                                                                                                                property_definition(19, 35, [
                                                                                                                                                                                    property_name(19, 25, [
                                                                                                                                                                                        literal_property_name(19, 25, [
                                                                                                                                                                                            identifier_name(19, 25)
                                                                                                                                                                                        ])
                                                                                                                                                                                    ]),
                                                                                                                                                                                    assignment_expression__in(27, 35, [
                                                                                                                                                                                        conditional_expression__in(27, 35, [
                                                                                                                                                                                            logical_or_expression__in(27, 35, [
                                                                                                                                                                                                logical_and_expression__in(27, 35, [
                                                                                                                                                                                                    bitwise_or_expression__in(27, 35, [
                                                                                                                                                                                                        bitwise_xor_expression__in(27, 35, [
                                                                                                                                                                                                            bitwise_and_expression__in(27, 35, [
                                                                                                                                                                                                                equality_expression__in(27, 35, [
                                                                                                                                                                                                                    relational_expression__in(27, 35, [
                                                                                                                                                                                                                        shift_expression(27, 35, [
                                                                                                                                                                                                                            additive_expression(27, 35, [
                                                                                                                                                                                                                                multiplicative_expression(27, 35, [
                                                                                                                                                                                                                                    unary_expression(27, 35, [
                                                                                                                                                                                                                                        postfix_expression(27, 35, [
                                                                                                                                                                                                                                            left_hand_side_expression(27, 35, [
                                                                                                                                                                                                                                                new_expression(27, 35, [
                                                                                                                                                                                                                                                    member_expression(27, 35, [
                                                                                                                                                                                                                                                        primary_expression(27, 34, [
                                                                                                                                                                                                                                                            literal(27, 34, [
                                                                                                                                                                                                                                                                string_literal(27, 34)
                                                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                                ])
                                                                                                                                                                                                                            ])
                                                                                                                                                                                                                        ])
                                                                                                                                                                                                                    ])
                                                                                                                                                                                                                ])
                                                                                                                                                                                                            ])
                                                                                                                                                                                                        ])
                                                                                                                                                                                                    ])
                                                                                                                                                                                                ])
                                                                                                                                                                                            ])
                                                                                                                                                                                        ])
                                                                                                                                                                                    ])
                                                                                                                                                                                ])
                                                                                                                                                                            ])
                                                                                                                                                                        ])
                                                                                                                                                                    ])
                                                                                                                                                                ])
                                                                                                                                                            ])
                                                                                                                                                        ])
                                                                                                                                                    ])
                                                                                                                                                ])
                                                                                                                                            ])
                                                                                                                                        ])
                                                                                                                                    ])
                                                                                                                                ])
                                                                                                                            ])
                                                                                                                        ])
                                                                                                                    ])
                                                                                                                ])
                                                                                                            ])
                                                                                                        ])
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ]),
                                                                                        arguments(37, 39)
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]),
                    smart_semicolon(39, 41)
                ])
            ])
        ]
    }
}

#[test]
fn test_script2_var_object() {
    parses_to! {
        parser: JsParser,
        input: r#"var x={
            x:'y'
            }"#,
        rule: Rule::script,
        tokens: [
            statement(0, 39, [
                variable_statement(0, 39, [
                    variable_declaration_list__in(4, 39, [
                        variable_declaration__in(4, 39, [
                            binding_identifier(4, 5),
                            initializer__in(5, 39, [
                                assignment_expression__in(6, 39, [
                                    conditional_expression__in(6, 39, [
                                        logical_or_expression__in(6, 39, [
                                            logical_and_expression__in(6, 39, [
                                                bitwise_or_expression__in(6, 39, [
                                                    bitwise_xor_expression__in(6, 39, [
                                                        bitwise_and_expression__in(6, 39, [
                                                            equality_expression__in(6, 39, [
                                                                relational_expression__in(6, 39, [
                                                                    shift_expression(6, 39, [
                                                                        additive_expression(6, 39, [
                                                                            multiplicative_expression(6, 39, [
                                                                                unary_expression(6, 39, [
                                                                                    postfix_expression(6, 39, [
                                                                                        left_hand_side_expression(6, 39, [
                                                                                            new_expression(6, 39, [
                                                                                                member_expression(6, 39, [
                                                                                                    primary_expression(6, 39, [
                                                                                                        object_literal(6, 39, [
                                                                                                            property_definition_list(20, 25, [
                                                                                                                property_definition(20, 25, [
                                                                                                                    property_name(20, 21, [
                                                                                                                        literal_property_name(20, 21, [
                                                                                                                            identifier_name(20, 21)
                                                                                                                        ])
                                                                                                                    ]),
                                                                                                                    assignment_expression__in(22, 25, [
                                                                                                                        conditional_expression__in(22, 25, [
                                                                                                                            logical_or_expression__in(22, 25, [
                                                                                                                                logical_and_expression__in(22, 25, [
                                                                                                                                    bitwise_or_expression__in(22, 25, [
                                                                                                                                        bitwise_xor_expression__in(22, 25, [
                                                                                                                                            bitwise_and_expression__in(22, 25, [
                                                                                                                                                equality_expression__in(22, 25, [
                                                                                                                                                    relational_expression__in(22, 25, [
                                                                                                                                                        shift_expression(22, 25, [
                                                                                                                                                            additive_expression(22, 25, [
                                                                                                                                                                multiplicative_expression(22, 25, [
                                                                                                                                                                    unary_expression(22, 25, [
                                                                                                                                                                        postfix_expression(22, 25, [
                                                                                                                                                                            left_hand_side_expression(22, 25, [
                                                                                                                                                                                new_expression(22, 25, [
                                                                                                                                                                                    member_expression(22, 25, [
                                                                                                                                                                                        primary_expression(22, 25, [
                                                                                                                                                                                            literal(22, 25, [
                                                                                                                                                                                                string_literal(22, 25)
                                                                                                                                                                                            ])
                                                                                                                                                                                        ])
                                                                                                                                                                                    ])
                                                                                                                                                                                ])
                                                                                                                                                                            ])
                                                                                                                                                                        ])
                                                                                                                                                                    ])
                                                                                                                                                                ])
                                                                                                                                                            ])
                                                                                                                                                        ])
                                                                                                                                                    ])
                                                                                                                                                ])
                                                                                                                                            ])
                                                                                                                                        ])
                                                                                                                                    ])
                                                                                                                                ])
                                                                                                                            ])
                                                                                                                        ])
                                                                                                                    ])
                                                                                                                ])
                                                                                                            ])
                                                                                                        ])
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                    ]),
                    smart_semicolon(39, 39)
                ])
            ])
        ]
    }
}

#[test]
fn test_script3_for() {
    parses_to! {
        parser: JsParser,
        input: r#"var x=y
        for (var x of f) {}"#,
        rule: Rule::script,
        tokens: [
            statement(0, 8, [
                variable_statement(0, 8, [
                    variable_declaration_list__in(4, 7, [
                        variable_declaration__in(4, 7, [
                            binding_identifier(4, 5),
                            initializer__in(5, 7, [
                                assignment_expression__in(6, 7, [
                                    conditional_expression__in(6, 7, [
                                        logical_or_expression__in(6, 7, [
                                            logical_and_expression__in(6, 7, [
                                                bitwise_or_expression__in(6, 7, [
                                                    bitwise_xor_expression__in(6, 7, [
                                                        bitwise_and_expression__in(6, 7, [
                                                            equality_expression__in(6, 7, [
                                                                relational_expression__in(6, 7, [
                                                                    shift_expression(6, 7, [
                                                                        additive_expression(6, 7, [
                                                                            multiplicative_expression(6, 7, [
                                                                                unary_expression(6, 7, [
                                                                                    postfix_expression(6, 7, [
                                                                                        left_hand_side_expression(6, 7, [
                                                                                            new_expression(6, 7, [
                                                                                                member_expression(6, 7, [
                                                                                                    primary_expression(6, 7, [
                                                                                                        identifier_reference(6, 7)
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                    ]),
                    smart_semicolon(7, 8)
                ])
            ]),
            statement(16, 35, [
                breakable_statement(16, 35, [
                    iteration_statement(16, 35, [
                        for_binding(25, 26, [
                            binding_identifier(25, 26)
                        ]),
                        assignment_expression__in(30, 31, [
                            conditional_expression__in(30, 31, [
                                logical_or_expression__in(30, 31, [
                                    logical_and_expression__in(30, 31, [
                                        bitwise_or_expression__in(30, 31, [
                                            bitwise_xor_expression__in(30, 31, [
                                                bitwise_and_expression__in(30, 31, [
                                                    equality_expression__in(30, 31, [
                                                        relational_expression__in(30, 31, [
                                                            shift_expression(30, 31, [
                                                                additive_expression(30, 31, [
                                                                    multiplicative_expression(30, 31, [
                                                                        unary_expression(30, 31, [
                                                                            postfix_expression(30, 31, [
                                                                                left_hand_side_expression(30, 31, [
                                                                                    new_expression(30, 31, [
                                                                                        member_expression(30, 31, [
                                                                                            primary_expression(30, 31, [
                                                                                                identifier_reference(30, 31)
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                        statement(33, 35, [
                            block_statement(33, 35, [
                                block(33, 35)
                            ])
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_class1() {
    parses_to! {
        parser: JsParser,
        input: r#"class Mesh {
                constructor(g) {
                }

                update() {}
            }"#,
        rule: Rule::class_declaration,
        tokens: [
            class_declaration(0, 106, [
                binding_identifier(6, 10),
                class_tail(11, 106, [
                    class_body(29, 93, [
                        class_element(29, 63, [
                            method_definition(29, 63, [
                                property_name(29, 40, [
                                    literal_property_name(29, 40, [
                                        identifier_name(29, 40)
                                    ])
                                ]),
                                formal_parameters(41, 42, [
                                    formal_parameter(41, 42, [
                                        binding_element(41, 42, [
                                            single_name_binding(41, 42, [
                                                binding_identifier(41, 42)
                                            ])
                                        ])
                                    ])
                                ]),
                                function_body(62, 62)
                            ])
                        ]),
                        class_element(81, 92, [
                            method_definition(81, 92, [
                                property_name(81, 87, [
                                    literal_property_name(81, 87, [
                                        identifier_name(81, 87)
                                    ])
                                ]),
                                formal_parameters(88, 88),
                                function_body(91, 91)
                            ])
                        ])
                    ])
                ])
            ])
        ]
    }
}

#[test]
fn test_generator1() {
    parses_to! {
        parser: JsParser,
        input: r#"function *g() {
                yield 10;
            }"#,
        rule: Rule::generator_declaration,
        tokens: [
            generator_declaration(0, 55, [
                binding_identifier(10, 11),
                formal_parameters(12, 12),
                generator_body(32, 54, [
                    function_body__yield(32, 54, [
                        statement__yield_return(32, 42, [
                            expression_statement__yield(32, 42, [
                                expression__in_yield(32, 40, [
                                    assignment_expression__in_yield(32, 40, [
                                        yield_expression__in(32, 40, [
                                            assignment_expression__in_yield(38, 40, [
                                                conditional_expression__in_yield(38, 40, [
                                                    logical_or_expression__in_yield(38, 40, [
                                                        logical_and_expression__in_yield(38, 40, [
                                                            bitwise_or_expression__in_yield(38, 40, [
                                                                bitwise_xor_expression__in_yield(38, 40, [
                                                                    bitwise_and_expression__in_yield(38, 40, [
                                                                        equality_expression__in_yield(38, 40, [
                                                                            relational_expression__in_yield(38, 40, [
                                                                                shift_expression__yield(38, 40, [
                                                                                    additive_expression__yield(38, 40, [
                                                                                        multiplicative_expression__yield(38, 40, [
                                                                                            unary_expression__yield(38, 40, [
                                                                                                postfix_expression__yield(38, 40, [
                                                                                                    left_hand_side_expression__yield(38, 40, [
                                                                                                        new_expression__yield(38, 40, [
                                                                                                            member_expression__yield(38, 40, [
                                                                                                                primary_expression__yield(38, 40, [
                                                                                                                    literal(38, 40, [
                                                                                                                        numeric_literal(38, 40, [
                                                                                                                            decimal_literal(38, 40, [
                                                                                                                                decimal_integer_literal(38, 40)
                                                                                                                            ])
                                                                                                                        ])
                                                                                                                    ])
                                                                                                                ])
                                                                                                            ])
                                                                                                        ])
                                                                                                    ])
                                                                                                ])
                                                                                            ])
                                                                                        ])
                                                                                    ])
                                                                                ])
                                                                            ])
                                                                        ])
                                                                    ])
                                                                ])
                                                            ])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ]),
                                smart_semicolon(40, 42)
                            ])
                        ])
                    ])
                ])
            ])
        ]
    };
}

#[test]
fn test_perf1() {
    let start = Instant::now();
    let result = JsParser::parse_to_token_tree("[[[[]]]]");
    let end = Instant::now();
    match result {
        Ok(_) => {
            assert!(
                end.saturating_duration_since(start).as_millis() < 800,
                "Script taking too long to run."
            );
        }
        Err(e) => {
            assert!(false, "There was an error {}", e);
        }
    }
}
