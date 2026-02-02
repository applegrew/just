<img src="logo/just-logo-full.png" alt="logo image" />

# just - JS on Rust

A ground-up implementation of an ES6 JavaScript engine, from parser to executor, written in Rust.

This is an academic/experimental project rather than a production-ready engine.

## Overview

The grammar has been coded in [Pest](https://pest.rs/) language as per the ECMAScript 2015 (ES6) specification at https://262.ecma-international.org/6.0/.

The AST complies with [ESTree](https://github.com/estree/estree) specification. You can visualize similar AST structures at https://astexplorer.net/.

## Building & Testing

```bash
# Build
cargo build

# Run all tests (267 tests)
cargo test

# Run parser unit tests only
cargo test --package just --lib parser::unit_tests

# Run integration tests
cargo test --test test_integration

# Run benchmarks
cargo run --release --bin benchmark
```

## Project Status

### Parser (Mostly Complete)

The parser supports most ES6 syntax:

**Literals**
- Strings (with escape sequences, unicode escapes)
- Numbers (decimal, hex `0x`, binary `0b`, octal `0o`, floats, scientific notation)
- Booleans, null
- Regular expressions
- Template literals

**Expressions**
- Identifiers and keywords
- Object literals `{}`
- Array literals `[]`
- Function expressions
- Arrow functions
- Class expressions
- Member expressions (dot and bracket notation)
- Call expressions
- Unary, binary, logical, conditional expressions
- Assignment expressions (including compound: `+=`, `-=`, etc.)
- Update expressions (`++`, `--` prefix and postfix)
- Spread elements `...`
- Destructuring patterns (object and array)
- Generator expressions
- `super` and `new.target`

**Statements**
- Variable declarations (`var`, `let`, `const`)
- Block statements
- If/else
- Loops: `while`, `do-while`, `for`, `for-in`, `for-of`
- `switch`/`case`
- `try`/`catch`/`finally`
- `throw`
- `return`, `break`, `continue`
- Function and class declarations
- Generator declarations

**Not Supported**
- Async/await (ES2017)
- Labeled statements

### Runner/Evaluator

The runtime supports core JavaScript execution:

**Expressions**
- Literal evaluation (all types)
- Unary operators: `typeof`, `void`, `!`, `-`, `+`, `~`
- Binary arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `<`, `>`, `<=`, `>=`, `===`, `!==`, `==`, `!=`
- Bitwise: `&`, `|`, `^`, `<<`, `>>`, `>>>`
- Logical: `&&`, `||` (with short-circuit evaluation)
- Conditional (ternary): `? :`
- Sequence (comma): `,`
- Update expressions: `++x`, `x++`, `--x`, `x--`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`, `>>>=`
- Type coercion for operations

**Variables & Scoping**
- Variable declarations (`var`, `let`, `const`)
- Lexical scoping for `let`/`const`
- Function-scoped `var` with hoisting behavior
- Block scope support

**Objects & Arrays**
- Object literal creation
- Array literal creation
- Property access (dot notation: `obj.prop`)
- Computed property access (bracket notation: `obj["prop"]`)
- String property access (`str.length`, `str[0]`)

**Functions**
- Function declarations
- Function calls with arguments
- Return values
- Closure support (functions capture their environment)
- Parameter binding

**Control Flow**
- `if`/`else` statements
- `while` loops
- `do-while` loops
- `for` loops
- `for-in` loops (iterate over object keys)
- `for-of` loops (iterate over iterable values)
- `switch`/`case` statements with fall-through
- `break` and `continue`
- `try`/`catch`/`finally` exception handling
- `throw` statements

**Not Yet Implemented**
- `new` expressions (object construction)
- Classes and inheritance
- Generators and `yield`
- `eval()`
- `delete` operator
- `in` and `instanceof` operators
- Getters/setters
- Spread in function calls and arrays
- Destructuring assignment

### Built-in Objects

**console**
- `log()`, `error()`, `warn()`, `info()`

**Math** (Fully implemented)
- Constants: `E`, `PI`, `LN2`, `LN10`, `LOG2E`, `LOG10E`, `SQRT1_2`, `SQRT2`
- Methods: `abs`, `floor`, `ceil`, `round`, `trunc`, `sign`, `min`, `max`, `sqrt`, `cbrt`, `pow`, `exp`, `expm1`, `log`, `log10`, `log2`, `log1p`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `hypot`, `random`, `clz32`, `imul`, `fround`

**JSON**
- `parse()`, `stringify()`

**Number**
- Constants: `MAX_VALUE`, `MIN_VALUE`, `POSITIVE_INFINITY`, `NEGATIVE_INFINITY`, `NaN`, `MAX_SAFE_INTEGER`, `MIN_SAFE_INTEGER`, `EPSILON`
- Methods: `isNaN`, `isFinite`, `isInteger`, `isSafeInteger`, `parseFloat`, `parseInt`, `toString`, `toFixed`, `toExponential`, `toPrecision`

**String**
- `charAt`, `charCodeAt`, `substring`, `slice`, `indexOf`, `lastIndexOf`, `includes`, `startsWith`, `endsWith`, `split`, `trim`, `trimStart`, `trimEnd`, `toUpperCase`, `toLowerCase`, `repeat`, `padStart`, `padEnd`, `replace`, `concat`, `fromCharCode`

**Array** (Registered, limited functionality)
- `push`, `pop`, `shift`, `unshift`, `splice`, `reverse`, `sort`, `slice`, `concat`, `indexOf`, `includes`, `join`, `forEach`, `map`, `filter`, `reduce`, `find`, `every`, `some`, `isArray`

**Object** (Registered, limited functionality)
- `toString`, `valueOf`, `hasOwnProperty`, `keys`, `values`, `entries`, `assign`

**Error Types**
- `Error`, `TypeError`, `ReferenceError`, `SyntaxError`, `RangeError`, `EvalError`, `URIError`

## Architecture

```
src/
├── parser/
│   ├── api.rs              # Parser API and AST builder
│   ├── ast.rs              # AST type definitions (ESTree compliant)
│   ├── js_grammar.pest     # Pest grammar for ES6
│   └── static_semantics.rs # Static semantic analysis
└── runner/
    ├── eval/               # Expression evaluation
    ├── ds/                 # Data structures (objects, functions, etc.)
    └── plugin/             # Built-in object registry
```

## Development

This project uses JetBrains IntelliJ IDEA. Project files are included in the repository.

## License

This project is licensed under the MIT License.
