//! JIT compilation module for the JavaScript engine.
//!
//! Provides a bytecode compiler and stack-based VM as an alternative
//! execution path to the tree-walking interpreter. The pipeline is:
//!
//! ```text
//! JavaScript source → Parser → AST → Compiler → Bytecode → VM → Result
//! ```
//!
//! The bytecode representation eliminates per-node dispatch overhead
//! and improves cache locality compared to recursive AST evaluation.

pub mod bytecode;
pub mod compiler;
pub mod vm;

use crate::parser::ast::ProgramData;
use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::JsValue;
use crate::runner::plugin::registry::BuiltInRegistry;
use crate::runner::plugin::types::EvalContext;

use self::bytecode::Chunk;
use self::compiler::Compiler;
use self::vm::{Vm, VmResult};

/// Compile an AST program into bytecode.
pub fn compile(program: &ProgramData) -> Chunk {
    let compiler = Compiler::new();
    compiler.compile_program(program)
}

/// Execute a compiled bytecode chunk.
pub fn execute(chunk: &Chunk, ctx: EvalContext, registry: &BuiltInRegistry) -> Result<JsValue, JErrorType> {
    let mut vm = Vm::new(chunk, ctx, registry);
    match vm.run() {
        VmResult::Ok(val) => Ok(val),
        VmResult::Error(e) => Err(e),
    }
}

/// Compile and execute a program in one step.
pub fn compile_and_run(
    program: &ProgramData,
    ctx: EvalContext,
    registry: &BuiltInRegistry,
) -> Result<JsValue, JErrorType> {
    let chunk = compile(program);
    execute(&chunk, ctx, registry)
}

/// Compile and execute, returning the EvalContext for variable inspection.
pub fn compile_and_run_with_ctx(
    program: &ProgramData,
    registry: &BuiltInRegistry,
) -> (Result<JsValue, JErrorType>, EvalContext) {
    let chunk = compile(program);
    let ctx = EvalContext::new();
    let mut vm = Vm::new(&chunk, ctx, registry);
    let result = match vm.run() {
        VmResult::Ok(val) => Ok(val),
        VmResult::Error(e) => Err(e),
    };
    let ctx_out = vm.into_ctx();
    (result, ctx_out)
}
