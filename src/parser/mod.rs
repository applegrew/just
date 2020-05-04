use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
pub struct JsParser;

#[cfg(test)]
mod tests {
    use super::*;
    //use pest::Parser;
    use pest::parses_to;
    use pest::fails_with;
    use pest::consumes_to;

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
            positives: vec![Rule::template_middle, Rule::template_tail, Rule::template_literal, Rule::arguments, Rule::multiplicative_operator, Rule::assignment_operator],
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
            positives: vec![Rule::assignment_expression__in],
            negatives: vec![],
            pos: 25
        };
    }

    #[test]
    fn test_arrays1() {
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
}
