//! Benchmark runner for the JavaScript interpreter.
//!
//! Compares performance against Node.js for reference.

extern crate just;

use just::parser::JsParser;
use just::runner::eval::statement::execute_statement;
use just::runner::plugin::types::EvalContext;
use just::runner::ds::value::{JsValue, JsNumberType};
use std::time::{Duration, Instant};

/// Run a benchmark and return the execution time.
fn run_benchmark(name: &str, code: &str, iterations: u32) -> Duration {
    let ast = JsParser::parse_to_ast_from_str(code)
        .expect(&format!("Failed to parse benchmark: {}", name));

    let start = Instant::now();

    for _ in 0..iterations {
        let mut ctx = EvalContext::new();
        for stmt in &ast.body {
            let _ = execute_statement(stmt, &mut ctx);
        }
    }

    start.elapsed()
}

/// Get the result of running code.
fn run_and_get_var(code: &str, var_name: &str) -> JsValue {
    let ast = JsParser::parse_to_ast_from_str(code).unwrap();
    let mut ctx = EvalContext::new();
    for stmt in &ast.body {
        let _ = execute_statement(stmt, &mut ctx);
    }
    ctx.get_binding(var_name).unwrap_or(JsValue::Undefined)
}

// ============================================================================
// Benchmark definitions
// ============================================================================

const BENCH_FIBONACCI: &str = r#"
var n = 20;
var a = 0;
var b = 1;
for (var i = 0; i < n; i = i + 1) {
    var temp = a;
    a = b;
    b = temp + b;
}
"#;

const BENCH_LOOP_SUM: &str = r#"
var sum = 0;
for (var i = 0; i < 10000; i = i + 1) {
    sum = sum + i;
}
"#;

const BENCH_NESTED_LOOPS: &str = r#"
var count = 0;
for (var i = 0; i < 100; i = i + 1) {
    for (var j = 0; j < 100; j = j + 1) {
        count = count + 1;
    }
}
"#;

const BENCH_BITWISE: &str = r#"
var result = 0;
for (var i = 0; i < 1000; i = i + 1) {
    result = (result ^ i) & 0xFFFF;
}
"#;

const BENCH_CONDITIONALS: &str = r#"
var count = 0;
for (var i = 0; i < 1000; i = i + 1) {
    if (i % 2 === 0) {
        count = count + 1;
    } else {
        count = count + 2;
    }
}
"#;

const BENCH_WHILE_LOOP: &str = r#"
var i = 0;
var sum = 0;
while (i < 5000) {
    sum = sum + i;
    i = i + 1;
}
"#;

const BENCH_ARITHMETIC: &str = r#"
var result = 0;
for (var i = 1; i < 1000; i = i + 1) {
    result = result + i * 2 - i / 2;
}
"#;

const BENCH_FACTORIAL: &str = r#"
var n = 12;
var result = 1;
for (var i = 2; i <= n; i = i + 1) {
    result = result * i;
}
"#;

const BENCH_PRIME_SIEVE: &str = r#"
var count = 0;
for (var n = 2; n < 100; n = n + 1) {
    var isPrime = true;
    for (var i = 2; i * i <= n; i = i + 1) {
        if (n % i === 0) {
            isPrime = false;
            break;
        }
    }
    if (isPrime) {
        count = count + 1;
    }
}
"#;

const BENCH_GCD: &str = r#"
var result = 0;
for (var k = 0; k < 100; k = k + 1) {
    var a = 48;
    var b = 18;
    while (b !== 0) {
        var temp = b;
        b = a % b;
        a = temp;
    }
    result = result + a;
}
"#;

fn main() {
    println!("=======================================================");
    println!("  Just JavaScript Engine - Performance Benchmarks");
    println!("=======================================================\n");

    let benchmarks: Vec<(&str, &str, u32)> = vec![
        ("Fibonacci (n=20)", BENCH_FIBONACCI, 1000),
        ("Loop Sum (10K iterations)", BENCH_LOOP_SUM, 100),
        ("Nested Loops (100x100)", BENCH_NESTED_LOOPS, 100),
        ("Bitwise Operations (1K)", BENCH_BITWISE, 500),
        ("Conditionals (1K)", BENCH_CONDITIONALS, 500),
        ("While Loop (5K)", BENCH_WHILE_LOOP, 100),
        ("Arithmetic (1K)", BENCH_ARITHMETIC, 500),
        ("Factorial (n=12)", BENCH_FACTORIAL, 5000),
        ("Prime Sieve (<100)", BENCH_PRIME_SIEVE, 200),
        ("GCD (100 iterations)", BENCH_GCD, 200),
    ];

    println!("{:<30} {:>15} {:>15}", "Benchmark", "Total Time", "Per Iteration");
    println!("{}", "-".repeat(62));

    let mut total_time = Duration::ZERO;

    for (name, code, iterations) in &benchmarks {
        let duration = run_benchmark(name, code, *iterations);
        let per_iter = duration / *iterations;
        total_time += duration;

        println!(
            "{:<30} {:>12.2?} {:>12.2?}",
            name, duration, per_iter
        );
    }

    println!("{}", "-".repeat(62));
    println!("{:<30} {:>12.2?}", "TOTAL", total_time);

    // Verify correctness
    println!("\n=======================================================");
    println!("  Correctness Verification");
    println!("=======================================================\n");

    let verifications: Vec<(&str, &str, &str, i64)> = vec![
        ("Fibonacci", BENCH_FIBONACCI, "a", 6765),
        ("Loop Sum", BENCH_LOOP_SUM, "sum", 49995000),
        ("Nested Loops", BENCH_NESTED_LOOPS, "count", 10000),
        ("Factorial", BENCH_FACTORIAL, "result", 479001600),
        ("Prime Count", BENCH_PRIME_SIEVE, "count", 25),
    ];

    for (name, code, var, expected) in verifications {
        let result = run_and_get_var(code, var);
        let actual = match result {
            JsValue::Number(JsNumberType::Integer(n)) => n,
            _ => -1,
        };
        let status = if actual == expected { "✓" } else { "✗" };
        println!("{} {}: expected {}, got {}", status, name, expected, actual);
    }

    println!("\n=======================================================");
    println!("  Reference: Node.js Equivalent Commands");
    println!("=======================================================\n");

    println!("To compare with Node.js, run these commands:\n");
    println!("# Fibonacci benchmark");
    println!("node -e \"let start=Date.now(); for(let k=0;k<1000;k++){{let a=0,b=1;for(let i=0;i<20;i++){{let t=a;a=b;b=t+b}}}}; console.log(Date.now()-start+'ms')\"\n");
    println!("# Loop Sum benchmark");
    println!("node -e \"let start=Date.now(); for(let k=0;k<100;k++){{let sum=0;for(let i=0;i<10000;i++)sum+=i}}; console.log(Date.now()-start+'ms')\"\n");
}
