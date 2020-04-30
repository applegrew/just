use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
pub struct JsParser;

#[cfg(test)]
mod tests {
    use super::*;
    //use pest::Parser;
    use pest::parses_to;
    use pest::consumes_to;

    #[test]
    fn test_numbers() {
        parses_to! {
            parser: JsParser,
            input: "10",
            rule: Rule::number,
            tokens: [
                number(0, 2, [
                    decimal_number(0, 2, [
                        decimal_number_with_no_dot(0, 2)
                    ])
                ])
            ]
        };
    }

}
