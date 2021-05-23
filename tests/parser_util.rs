extern crate just;

use just::parser::JsParser;
use std::cmp;
use std::fs;
use std::time::Instant;

fn diff_tree(message: &str, expected_tree: &str, actual_tree: &str) {
    let expected_lines: Vec<&str> = expected_tree.trim().split("\n").collect();
    let actual_lines: Vec<&str> = actual_tree.trim().split("\n").collect();
    let expected_total_lines = expected_lines.len();
    let actual_total_lines = actual_lines.len();
    let max_len = cmp::max(expected_total_lines, actual_total_lines);
    let mut last_diff_line = 0;
    let mut diff_start = 0;
    let mut diff_in_expected = vec![];
    let mut diff_in_actual = vec![];
    let mut has_diffs = false;
    for line_num in 1..max_len + 1 {
        let mut expected_line = "";
        let mut actual_line = "";
        if line_num <= expected_total_lines {
            expected_line = expected_lines[line_num - 1];
        }
        if line_num <= actual_total_lines {
            actual_line = actual_lines[line_num - 1];
        }
        if actual_line != expected_line {
            if line_num - 1 != last_diff_line {
                print_diff(
                    &diff_in_expected,
                    &diff_in_actual,
                    diff_start,
                    last_diff_line,
                );
                diff_in_expected.clear();
                diff_in_actual.clear();
                diff_start = line_num;
            }
            diff_in_expected.push(expected_line);
            diff_in_actual.push(actual_line);
            last_diff_line = line_num;
            has_diffs = true;
        }
    }
    print_diff(
        &diff_in_expected,
        &diff_in_actual,
        diff_start,
        last_diff_line,
    );
    assert!(!has_diffs, "Test failed: {}", message);
}

fn print_diff(
    diff_in_expected: &Vec<&str>,
    diff_in_actual: &Vec<&str>,
    diff_start: usize,
    diff_line: usize,
) {
    if diff_in_expected.len() != 0 || diff_in_actual.len() != 0 {
        let mut check = true;
        println!("\n====Expected lines====Line#{}-{}", diff_start, diff_line);
        for line in diff_in_expected {
            if check {
                if line.trim() != "" {
                    println!("{}", line);
                    check = false;
                }
            } else {
                println!("{}", line);
            }
        }
        check = true;
        println!("\n====Actual lines====Line#{}-{}", diff_start, diff_line);
        for line in diff_in_actual {
            if check {
                if line.trim() != "" {
                    println!("{}", line);
                    check = false;
                }
            } else {
                println!("{}", line);
            }
        }
    }
}

pub fn assert_parse(test_name: &str) {
    let unparsed_file =
        fs::read_to_string(format!("tests/artifacts/test_js/test_{}.js", test_name))
            .expect("Cannot read test file");
    let expected_p_tree =
        fs::read_to_string(format!("tests/artifacts/parse_trees/{}.txt", test_name))
            .expect("Cannot read assert parse tree file");
    let expected_as_tree = fs::read_to_string(format!(
        "tests/artifacts/abstract_syntax_trees/{}.txt",
        test_name
    ))
    .expect("Cannot read assert ast file");
    let start = Instant::now();
    let result = JsParser::parse_to_token_tree(unparsed_file.as_str()).unwrap();
    let end_1 = Instant::now();
    let ast_result = JsParser::parse_to_ast_formatted_string(unparsed_file.as_str()).unwrap();
    let end_2 = Instant::now();
    let total_time_1 = end_1.saturating_duration_since(start);
    let total_time_2 = end_2.saturating_duration_since(end_1);
    diff_tree("For Parse tree", expected_p_tree.as_str(), result.as_str());
    diff_tree("For AST", expected_as_tree.as_str(), ast_result.as_str());
    println!(
        "Test {} took {}ms and {}ms",
        test_name,
        total_time_1.as_millis(),
        total_time_2.as_millis()
    );
}
