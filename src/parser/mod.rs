extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "parser/js_grammar.pest"] // relative to src
struct JSParser;
