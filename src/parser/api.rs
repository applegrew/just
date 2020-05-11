use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::time::Instant;
use crate::parser::ast::Script;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
pub struct JsParser;

pub struct JsParseError {

}

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

pub fn parse_to_ast(script: &str) -> Result<Script, JsParseError> {

}