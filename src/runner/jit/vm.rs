//! Stack-based bytecode virtual machine.
//!
//! Executes the bytecode emitted by the compiler. Uses a flat instruction
//! dispatch loop instead of recursive AST walking, which eliminates
//! per-node function call overhead and improves cache locality.

use crate::runner::ds::error::JErrorType;
use crate::runner::ds::value::{JsNumberType, JsValue};
use crate::runner::plugin::registry::BuiltInRegistry;
use crate::runner::plugin::types::EvalContext;

use super::bytecode::{Chunk, OpCode};

/// Result of VM execution.
pub enum VmResult {
    /// Normal completion with a value.
    Ok(JsValue),
    /// Runtime error.
    Error(JErrorType),
}

/// The bytecode virtual machine.
pub struct Vm<'a> {
    chunk: &'a Chunk,
    /// Instruction pointer.
    ip: usize,
    /// Operand stack.
    stack: Vec<JsValue>,
    /// Evaluation context (variables, scoping, heap).
    ctx: EvalContext,
    /// Built-in object registry for method calls.
    registry: &'a BuiltInRegistry,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk, ctx: EvalContext, registry: &'a BuiltInRegistry) -> Self {
        Vm {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(256),
            ctx,
            registry,
        }
    }

    /// Consume the VM and return the evaluation context (for variable inspection after execution).
    pub fn into_ctx(self) -> EvalContext {
        self.ctx
    }

    /// Run the bytecode to completion.
    pub fn run(&mut self) -> VmResult {
        loop {
            if self.ip >= self.chunk.code.len() {
                return VmResult::Ok(self.stack.pop().unwrap_or(JsValue::Undefined));
            }

            let instr = &self.chunk.code[self.ip];
            let op = instr.op;
            let operand = instr.operand;
            let operand2 = instr.operand2;
            self.ip += 1;

            match op {
                // ── Constants & Literals ──────────────────────
                OpCode::Constant => {
                    let val = self.chunk.constants[operand as usize].clone();
                    self.stack.push(val);
                }
                OpCode::Undefined => self.stack.push(JsValue::Undefined),
                OpCode::Null => self.stack.push(JsValue::Null),
                OpCode::True => self.stack.push(JsValue::Boolean(true)),
                OpCode::False => self.stack.push(JsValue::Boolean(false)),

                // ── Arithmetic ───────────────────────────────
                OpCode::Add => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.js_add(a, b));
                }
                OpCode::Sub => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.js_sub(a, b));
                }
                OpCode::Mul => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.js_mul(a, b));
                }
                OpCode::Div => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.js_div(a, b));
                }
                OpCode::Mod => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.js_mod(a, b));
                }
                OpCode::Negate => {
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.js_negate(a));
                }

                // ── Bitwise ──────────────────────────────────
                OpCode::BitAnd => {
                    let bv = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let b = self.to_i32(bv);
                    let a = self.to_i32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer((a & b) as i64)));
                }
                OpCode::BitOr => {
                    let bv = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let b = self.to_i32(bv);
                    let a = self.to_i32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer((a | b) as i64)));
                }
                OpCode::BitXor => {
                    let bv = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let b = self.to_i32(bv);
                    let a = self.to_i32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer((a ^ b) as i64)));
                }
                OpCode::BitNot => {
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.to_i32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer((!a) as i64)));
                }
                OpCode::ShiftLeft => {
                    let bv = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let b = self.to_u32(bv);
                    let a = self.to_i32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer(
                        (a << (b & 0x1f)) as i64,
                    )));
                }
                OpCode::ShiftRight => {
                    let bv = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let b = self.to_u32(bv);
                    let a = self.to_i32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer(
                        (a >> (b & 0x1f)) as i64,
                    )));
                }
                OpCode::UShiftRight => {
                    let bv = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let av = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let b = self.to_u32(bv);
                    let a = self.to_u32(av);
                    self.stack.push(JsValue::Number(JsNumberType::Integer(
                        (a >> (b & 0x1f)) as i64,
                    )));
                }

                // ── Comparison ───────────────────────────────
                OpCode::StrictEqual => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(self.js_strict_equal(&a, &b)));
                }
                OpCode::StrictNotEqual => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(!self.js_strict_equal(&a, &b)));
                }
                OpCode::Equal => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(self.js_abstract_equal(&a, &b)));
                }
                OpCode::NotEqual => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(!self.js_abstract_equal(&a, &b)));
                }
                OpCode::LessThan => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(self.js_less_than(&a, &b)));
                }
                OpCode::LessEqual => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    // a <= b  is  !(b < a)
                    self.stack.push(JsValue::Boolean(!self.js_less_than(&b, &a)));
                }
                OpCode::GreaterThan => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(self.js_less_than(&b, &a)));
                }
                OpCode::GreaterEqual => {
                    let b = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(!self.js_less_than(&a, &b)));
                }

                // ── Logical / Unary ──────────────────────────
                OpCode::Not => {
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Boolean(!self.is_truthy(&a)));
                }
                OpCode::TypeOf => {
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let type_str = match &a {
                        JsValue::Undefined => "undefined",
                        JsValue::Null => "object",
                        JsValue::Boolean(_) => "boolean",
                        JsValue::Number(_) => "number",
                        JsValue::String(_) => "string",
                        JsValue::Symbol(_) => "symbol",
                        JsValue::Object(_) => "object",
                    };
                    self.stack.push(JsValue::String(type_str.to_string()));
                }
                OpCode::Void => {
                    self.stack.pop();
                    self.stack.push(JsValue::Undefined);
                }
                OpCode::UnaryPlus => {
                    let a = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(self.to_number_value(a));
                }

                // ── Variables ────────────────────────────────
                OpCode::GetVar => {
                    let name = self.chunk.get_name(operand);
                    match self.ctx.get_binding(name) {
                        Ok(val) => self.stack.push(val),
                        Err(e) => return VmResult::Error(e),
                    }
                }
                OpCode::SetVar => {
                    let name = self.chunk.get_name(operand);
                    let val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    if let Err(e) = self.ctx.set_binding(name, val) {
                        return VmResult::Error(e);
                    }
                }
                OpCode::DeclareVar => {
                    let name = self.chunk.get_name(operand);
                    if !self.ctx.has_var_binding(name) {
                        if let Err(e) = self.ctx.create_var_binding(name) {
                            return VmResult::Error(e);
                        }
                    }
                }
                OpCode::DeclareLet => {
                    let name = self.chunk.get_name(operand);
                    if let Err(e) = self.ctx.create_binding(name, false) {
                        return VmResult::Error(e);
                    }
                }
                OpCode::DeclareConst => {
                    let name = self.chunk.get_name(operand);
                    if let Err(e) = self.ctx.create_binding(name, true) {
                        return VmResult::Error(e);
                    }
                }
                OpCode::InitVar => {
                    let name = self.chunk.get_name(operand);
                    let val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    // First try initialize (works when binding is uninitialized).
                    // If that silently no-ops (binding already initialized),
                    // fall back to set (works when binding is already initialized).
                    if let Err(e) = self.ctx.initialize_var_binding(name, val.clone()) {
                        return VmResult::Error(e);
                    }
                    // Also set, in case initialize was a no-op (already initialized).
                    // set_var_binding will error on uninitialized, so ignore that error.
                    let _ = self.ctx.set_var_binding(name, val);
                }
                OpCode::InitBinding => {
                    let name = self.chunk.get_name(operand);
                    let val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    if let Err(e) = self.ctx.initialize_binding(name, val) {
                        return VmResult::Error(e);
                    }
                }

                // ── Control Flow ─────────────────────────────
                OpCode::Jump => {
                    self.ip = operand as usize;
                }
                OpCode::JumpIfFalse => {
                    let val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    if !self.is_truthy(&val) {
                        self.ip = operand as usize;
                    }
                }
                OpCode::JumpIfTrue => {
                    let val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    if self.is_truthy(&val) {
                        self.ip = operand as usize;
                    }
                }

                OpCode::LoopStart => {
                    // No-op marker
                }

                // ── Stack manipulation ───────────────────────
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::Dup => {
                    if let Some(top) = self.stack.last() {
                        self.stack.push(top.clone());
                    }
                }

                // ── Scope ────────────────────────────────────
                OpCode::PushScope => {
                    self.ctx.push_block_scope();
                }
                OpCode::PopScope => {
                    self.ctx.pop_block_scope();
                }

                // ── Objects & Properties ─────────────────────
                OpCode::GetProp => {
                    let _obj = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let _prop_name = self.chunk.get_name(operand);
                    // Simplified: property access not fully supported in JIT yet
                    self.stack.push(JsValue::Undefined);
                }
                OpCode::SetProp => {
                    let _val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let _obj = self.stack.pop().unwrap_or(JsValue::Undefined);
                    // Simplified
                    self.stack.push(JsValue::Undefined);
                }
                OpCode::GetElem => {
                    let _key = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let _obj = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Undefined);
                }
                OpCode::SetElem => {
                    let _val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let _key = self.stack.pop().unwrap_or(JsValue::Undefined);
                    let _obj = self.stack.pop().unwrap_or(JsValue::Undefined);
                    self.stack.push(JsValue::Undefined);
                }

                // ── Function calls ───────────────────────────
                OpCode::Call => {
                    let argc = operand as usize;
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap_or(JsValue::Undefined));
                    }
                    args.reverse();
                    let _callee = self.stack.pop().unwrap_or(JsValue::Undefined);
                    // TODO: implement function call dispatch
                    self.stack.push(JsValue::Undefined);
                }
                OpCode::CallMethod => {
                    let argc = operand as usize;
                    let method_name = self.chunk.get_name(operand2);
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.stack.pop().unwrap_or(JsValue::Undefined));
                    }
                    args.reverse();
                    let object = self.stack.pop().unwrap_or(JsValue::Undefined);

                    // Try to resolve via built-in registry
                    let obj_name = self.resolve_object_name(&object);
                    if let Some(ref name) = obj_name {
                        if let Some(builtin_fn) = self.registry.get_method(name, method_name) {
                            match builtin_fn.call(&mut self.ctx, object, args) {
                                Ok(result) => {
                                    self.stack.push(result);
                                    continue;
                                }
                                Err(e) => return VmResult::Error(e),
                            }
                        }
                    }
                    // Fallback
                    self.stack.push(JsValue::Undefined);
                }

                // ── Misc ─────────────────────────────────────
                OpCode::Return => {
                    let val = self.stack.pop().unwrap_or(JsValue::Undefined);
                    return VmResult::Ok(val);
                }
                OpCode::Halt => {
                    return VmResult::Ok(self.stack.pop().unwrap_or(JsValue::Undefined));
                }

                // ── Pre/Post increment/decrement ─────────────
                OpCode::PreIncVar => {
                    let name = self.chunk.get_name(operand);
                    match self.ctx.get_binding(name) {
                        Ok(val) => {
                            let num = self.to_f64(&val) + 1.0;
                            let new_val = self.f64_to_jsvalue(num);
                            if let Err(e) = self.ctx.set_binding(name, new_val.clone()) {
                                return VmResult::Error(e);
                            }
                            self.stack.push(new_val);
                        }
                        Err(e) => return VmResult::Error(e),
                    }
                }
                OpCode::PreDecVar => {
                    let name = self.chunk.get_name(operand);
                    match self.ctx.get_binding(name) {
                        Ok(val) => {
                            let num = self.to_f64(&val) - 1.0;
                            let new_val = self.f64_to_jsvalue(num);
                            if let Err(e) = self.ctx.set_binding(name, new_val.clone()) {
                                return VmResult::Error(e);
                            }
                            self.stack.push(new_val);
                        }
                        Err(e) => return VmResult::Error(e),
                    }
                }
                OpCode::PostIncVar => {
                    let name = self.chunk.get_name(operand);
                    match self.ctx.get_binding(name) {
                        Ok(val) => {
                            let old_val = val.clone();
                            let num = self.to_f64(&val) + 1.0;
                            let new_val = self.f64_to_jsvalue(num);
                            if let Err(e) = self.ctx.set_binding(name, new_val) {
                                return VmResult::Error(e);
                            }
                            self.stack.push(old_val);
                        }
                        Err(e) => return VmResult::Error(e),
                    }
                }
                OpCode::PostDecVar => {
                    let name = self.chunk.get_name(operand);
                    match self.ctx.get_binding(name) {
                        Ok(val) => {
                            let old_val = val.clone();
                            let num = self.to_f64(&val) - 1.0;
                            let new_val = self.f64_to_jsvalue(num);
                            if let Err(e) = self.ctx.set_binding(name, new_val) {
                                return VmResult::Error(e);
                            }
                            self.stack.push(old_val);
                        }
                        Err(e) => return VmResult::Error(e),
                    }
                }

                OpCode::GetVarForUpdate => {
                    let name = self.chunk.get_name(operand);
                    match self.ctx.get_binding(name) {
                        Ok(val) => self.stack.push(val),
                        Err(e) => return VmResult::Error(e),
                    }
                }
            }
        }
    }

    // ════════════════════════════════════════════════════════════
    // Helper methods
    // ════════════════════════════════════════════════════════════

    fn resolve_object_name(&self, value: &JsValue) -> Option<String> {
        // For built-in registry lookup, we need to figure out which
        // built-in object this is. This is a simplified heuristic.
        match value {
            JsValue::String(_) => Some("String".to_string()),
            JsValue::Number(_) => Some("Number".to_string()),
            JsValue::Object(_) => {
                // Could be console, Math, etc. — check by variable name
                // This is a limitation; full implementation would tag objects.
                None
            }
            _ => None,
        }
    }

    // ── Type coercion helpers ────────────────────────────────

    fn is_truthy(&self, val: &JsValue) -> bool {
        match val {
            JsValue::Undefined | JsValue::Null => false,
            JsValue::Boolean(b) => *b,
            JsValue::Number(n) => match n {
                JsNumberType::Integer(i) => *i != 0,
                JsNumberType::Float(f) => *f != 0.0 && !f.is_nan(),
                JsNumberType::NaN => false,
                JsNumberType::PositiveInfinity | JsNumberType::NegativeInfinity => true,
            },
            JsValue::String(s) => !s.is_empty(),
            _ => true,
        }
    }

    fn to_f64(&self, val: &JsValue) -> f64 {
        match val {
            JsValue::Number(JsNumberType::Integer(i)) => *i as f64,
            JsValue::Number(JsNumberType::Float(f)) => *f,
            JsValue::Number(JsNumberType::NaN) => f64::NAN,
            JsValue::Number(JsNumberType::PositiveInfinity) => f64::INFINITY,
            JsValue::Number(JsNumberType::NegativeInfinity) => f64::NEG_INFINITY,
            JsValue::Boolean(true) => 1.0,
            JsValue::Boolean(false) => 0.0,
            JsValue::Null => 0.0,
            JsValue::Undefined => f64::NAN,
            JsValue::String(s) => s.parse::<f64>().unwrap_or(f64::NAN),
            _ => f64::NAN,
        }
    }

    fn to_i32(&self, val: JsValue) -> i32 {
        let n = self.to_f64(&val);
        if n.is_nan() || n.is_infinite() || n == 0.0 {
            0
        } else {
            (n as i64 & 0xFFFFFFFF) as i32
        }
    }

    fn to_u32(&self, val: JsValue) -> u32 {
        let n = self.to_f64(&val);
        if n.is_nan() || n.is_infinite() || n == 0.0 {
            0
        } else {
            (n as i64 & 0xFFFFFFFF) as u32
        }
    }

    fn f64_to_jsvalue(&self, n: f64) -> JsValue {
        if n.is_nan() {
            JsValue::Number(JsNumberType::NaN)
        } else if n.is_infinite() {
            if n > 0.0 {
                JsValue::Number(JsNumberType::PositiveInfinity)
            } else {
                JsValue::Number(JsNumberType::NegativeInfinity)
            }
        } else if n.fract() == 0.0 && n >= i64::MIN as f64 && n <= i64::MAX as f64 {
            JsValue::Number(JsNumberType::Integer(n as i64))
        } else {
            JsValue::Number(JsNumberType::Float(n))
        }
    }

    fn to_number_value(&self, val: JsValue) -> JsValue {
        self.f64_to_jsvalue(self.to_f64(&val))
    }

    // ── Arithmetic helpers ───────────────────────────────────

    fn js_add(&self, a: JsValue, b: JsValue) -> JsValue {
        // String concatenation takes priority
        match (&a, &b) {
            (JsValue::String(sa), JsValue::String(sb)) => {
                JsValue::String(format!("{}{}", sa, sb))
            }
            (JsValue::String(sa), _) => {
                JsValue::String(format!("{}{}", sa, self.to_display_string(&b)))
            }
            (_, JsValue::String(sb)) => {
                JsValue::String(format!("{}{}", self.to_display_string(&a), sb))
            }
            _ => {
                let na = self.to_f64(&a);
                let nb = self.to_f64(&b);
                self.f64_to_jsvalue(na + nb)
            }
        }
    }

    fn js_sub(&self, a: JsValue, b: JsValue) -> JsValue {
        self.f64_to_jsvalue(self.to_f64(&a) - self.to_f64(&b))
    }

    fn js_mul(&self, a: JsValue, b: JsValue) -> JsValue {
        self.f64_to_jsvalue(self.to_f64(&a) * self.to_f64(&b))
    }

    fn js_div(&self, a: JsValue, b: JsValue) -> JsValue {
        let nb = self.to_f64(&b);
        if nb == 0.0 {
            let na = self.to_f64(&a);
            if na == 0.0 || na.is_nan() {
                JsValue::Number(JsNumberType::NaN)
            } else if na > 0.0 {
                JsValue::Number(JsNumberType::PositiveInfinity)
            } else {
                JsValue::Number(JsNumberType::NegativeInfinity)
            }
        } else {
            self.f64_to_jsvalue(self.to_f64(&a) / nb)
        }
    }

    fn js_mod(&self, a: JsValue, b: JsValue) -> JsValue {
        let na = self.to_f64(&a);
        let nb = self.to_f64(&b);
        if nb == 0.0 {
            JsValue::Number(JsNumberType::NaN)
        } else {
            self.f64_to_jsvalue(na % nb)
        }
    }

    fn js_negate(&self, a: JsValue) -> JsValue {
        let n = self.to_f64(&a);
        self.f64_to_jsvalue(-n)
    }

    // ── Comparison helpers ───────────────────────────────────

    fn js_strict_equal(&self, a: &JsValue, b: &JsValue) -> bool {
        match (a, b) {
            (JsValue::Undefined, JsValue::Undefined) => true,
            (JsValue::Null, JsValue::Null) => true,
            (JsValue::Boolean(a), JsValue::Boolean(b)) => a == b,
            (JsValue::Number(a), JsValue::Number(b)) => {
                let fa = self.num_to_f64(a);
                let fb = self.num_to_f64(b);
                if fa.is_nan() || fb.is_nan() {
                    false
                } else {
                    fa == fb
                }
            }
            (JsValue::String(a), JsValue::String(b)) => a == b,
            _ => false,
        }
    }

    fn js_abstract_equal(&self, a: &JsValue, b: &JsValue) -> bool {
        // Simplified abstract equality
        match (a, b) {
            (JsValue::Undefined, JsValue::Null) | (JsValue::Null, JsValue::Undefined) => true,
            _ => {
                // Fall back to strict equality for same types
                if std::mem::discriminant(a) == std::mem::discriminant(b) {
                    self.js_strict_equal(a, b)
                } else {
                    // Numeric comparison
                    let na = self.to_f64(a);
                    let nb = self.to_f64(b);
                    if na.is_nan() || nb.is_nan() {
                        false
                    } else {
                        na == nb
                    }
                }
            }
        }
    }

    fn js_less_than(&self, a: &JsValue, b: &JsValue) -> bool {
        match (a, b) {
            (JsValue::String(sa), JsValue::String(sb)) => sa < sb,
            _ => {
                let na = self.to_f64(a);
                let nb = self.to_f64(b);
                if na.is_nan() || nb.is_nan() {
                    false
                } else {
                    na < nb
                }
            }
        }
    }

    fn num_to_f64(&self, n: &JsNumberType) -> f64 {
        match n {
            JsNumberType::Integer(i) => *i as f64,
            JsNumberType::Float(f) => *f,
            JsNumberType::NaN => f64::NAN,
            JsNumberType::PositiveInfinity => f64::INFINITY,
            JsNumberType::NegativeInfinity => f64::NEG_INFINITY,
        }
    }

    fn to_display_string(&self, val: &JsValue) -> String {
        match val {
            JsValue::Undefined => "undefined".to_string(),
            JsValue::Null => "null".to_string(),
            JsValue::Boolean(b) => b.to_string(),
            JsValue::Number(n) => match n {
                JsNumberType::Integer(i) => i.to_string(),
                JsNumberType::Float(f) => f.to_string(),
                JsNumberType::NaN => "NaN".to_string(),
                JsNumberType::PositiveInfinity => "Infinity".to_string(),
                JsNumberType::NegativeInfinity => "-Infinity".to_string(),
            },
            JsValue::String(s) => s.clone(),
            _ => "[object Object]".to_string(),
        }
    }
}
