use crate::parser::{BinaryOp, Expr, ExprKind};
use std::{collections::HashMap, path::Path};

use super::types::*;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
    AddressSpace, OptimizationLevel,
};

/// The root compiler
pub struct Compiler<'a> {
    pub(crate) context: &'a Context,
    pub(crate) module: Module<'a>,
    pub(crate) builder: Builder<'a>,
    pub(crate) func_map: HashMap<String, FunctionVal<'a>>,
    pub(crate) var_map: HashMap<String, Variable<'a>>,
    pub(crate) exec_engine: ExecutionEngine<'a>,
}

impl<'a> Compiler<'a> {
    /// Create new compiler
    pub fn new(ctx: &'a Context) -> Self {
        let module = ctx.create_module("dynamik");
        let builder = ctx.create_builder();
        let exec_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .expect("Failed to create execute engine");
        let main_fun_ty = ctx.i32_type().fn_type(&[], false);
        let main_fun = module.add_function("main", main_fun_ty, None);
        let entry = ctx.append_basic_block(main_fun, "entry");
        builder.position_at_end(entry);

        let mut fun_map = HashMap::new();
        // Creating the function
        fun_map.insert(
            "main".into(),
            FunctionVal {
                block: Some(entry),
                value: main_fun,
            },
        );
        Self {
            builder,
            module,
            context: ctx,
            exec_engine,
            func_map: fun_map,
            var_map: HashMap::new(),
        }
    }
    fn get_type(&self, s: String) -> BasicTypeEnum<'a> {
        match s.as_str() {
            "int" => self.context.i64_type().into(),
            "float" => self.context.f64_type().into(),
            "string" => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            "bool" => self.context.bool_type().into(),
            _ => unreachable!(),
        }
    }
    pub fn compile(&mut self, ast: &[Expr]) {
        for node in ast {
            self.handle(node.clone());
        }

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)));

        self.module.print_to_stderr();

        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &triple,
                "generic", // cpu
                "",        // features
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();
        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new("./output.o"))
            .unwrap();
    }
    /// Handle the node of AST
    fn handle(&mut self, node: Expr) -> Value<'a> {
        match node.inner {
            ExprKind::Int(i) => {
                let integer = self.context.i64_type().const_int(i.unsigned_abs(), false);
                Value::Int(integer)
            }
            ExprKind::Float(f) => {
                let float = self.context.f64_type().const_float(f);
                Value::Float(float)
            }
            ExprKind::Ident(i) => {
                let ident = self.var_map.get(&i).unwrap();
                // Get the variable value
                match ident.clone().var_type {
                    BasicTypeEnum::FloatType(_) => Value::Float(
                        self.builder
                            .build_load(ident.var_type, ident.value, "")
                            .into_float_value(),
                    ),
                    BasicTypeEnum::IntType(_) => Value::Int(
                        self.builder
                            .build_load(ident.var_type, ident.value, "")
                            .into_int_value(),
                    ),
                    BasicTypeEnum::PointerType(_) => Value::Pointer(
                        self.builder
                            .build_load(ident.var_type, ident.value, "")
                            .into_pointer_value(),
                    ),
                    _ => todo!(),
                }
            }
            ExprKind::Binary(left, op, right) => {
                let left = self.handle(*left);
                let right = self.handle(*right);
                match op {
                    BinaryOp::Add => {
                        if left.is_float() {
                            Value::Float(self.builder.build_float_add(
                                left.as_float(),
                                right.as_float(),
                                "f_add",
                            ))
                        } else {
                            Value::Int(self.builder.build_int_add(
                                left.as_int(),
                                right.as_int(),
                                "i_add",
                            ))
                        }
                    }
                    BinaryOp::Mul => {
                        if left.is_float() {
                            Value::Float(self.builder.build_float_mul(
                                left.as_float(),
                                right.as_float(),
                                "f_mul",
                            ))
                        } else {
                            Value::Int(self.builder.build_int_mul(
                                left.as_int(),
                                right.as_int(),
                                "i_mul",
                            ))
                        }
                    }
                    BinaryOp::Div => {
                        if left.is_float() {
                            Value::Float(self.builder.build_float_div(
                                left.as_float(),
                                right.as_float(),
                                "f_div",
                            ))
                        } else {
                            Value::Int(self.builder.build_int_signed_div(
                                left.as_int(),
                                right.as_int(),
                                "i_div",
                            ))
                        }
                    }
                    BinaryOp::Sub => {
                        if left.is_float() {
                            Value::Float(self.builder.build_float_sub(
                                left.as_float(),
                                right.as_float(),
                                "f_sub",
                            ))
                        } else {
                            Value::Int(self.builder.build_int_sub(
                                left.as_int(),
                                right.as_int(),
                                "i_sub",
                            ))
                        }
                    }
                    BinaryOp::Less => Value::Bool(self.builder.build_int_compare(
                        inkwell::IntPredicate::ULT,
                        left.as_int(),
                        right.as_int(),
                        "less",
                    )),
                    BinaryOp::Greater => Value::Bool(self.builder.build_int_compare(
                        inkwell::IntPredicate::UGT,
                        left.as_int(),
                        right.as_int(),
                        "greater",
                    )),
                    BinaryOp::LessEq => Value::Bool(self.builder.build_int_compare(
                        inkwell::IntPredicate::ULE,
                        left.as_int(),
                        right.as_int(),
                        "less_equal",
                    )),
                    BinaryOp::GreaterEq => Value::Bool(self.builder.build_int_compare(
                        inkwell::IntPredicate::UGE,
                        left.as_int(),
                        right.as_int(),
                        "greater_equal",
                    )),
                    BinaryOp::NotEq => Value::Bool(self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        left.as_int(),
                        right.as_int(),
                        "not_equal",
                    )),
                    BinaryOp::Eq => Value::Bool(self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        left.as_int(),
                        right.as_int(),
                        "equal",
                    )),
                    BinaryOp::Or => {
                        Value::Bool(self.builder.build_or(left.as_bool(), right.as_bool(), "or"))
                    }
                    BinaryOp::And => Value::Bool(self.builder.build_and(
                        left.as_bool(),
                        right.as_bool(),
                        "and",
                    )),
                }
            }
            ExprKind::FunctionCall(name, params_) => match (*name).inner {
                ExprKind::Ident(name) => {
                    let fn_value = self.func_map.get(&name).unwrap().value;
                    let mut params = Vec::new();
                    for param in params_ {
                        params.push(BasicMetadataValueEnum::from(
                            self.handle(param).as_basic_value(),
                        ))
                    }
                    let t = self
                        .builder
                        .build_call(fn_value, &params, "")
                        .try_as_basic_value()
                        .left();
                    match t {
                        Some(t) => match t {
                            inkwell::values::BasicValueEnum::IntValue(i) => Value::Int(i),
                            inkwell::values::BasicValueEnum::FloatValue(f) => Value::Float(f),
                            inkwell::values::BasicValueEnum::PointerValue(p) => Value::Pointer(p),
                            _ => unreachable!(),
                        },
                        None => Value::Int(self.context.i8_type().get_undef()),
                    }
                }
                _ => unreachable!(),
            },
            ExprKind::ExternFunction(name, params_ty, ret_ty, is_var) => {
                let mut param_tys = Vec::new();
                for p in params_ty {
                    param_tys.push(BasicMetadataTypeEnum::from(self.get_type(p)));
                }
                if let Some(ret) = ret_ty {
                    let ty = self.get_type(ret);
                    let fn_ty = ty.fn_type(&param_tys, is_var);
                    let fn_value = self.module.add_function(&name, fn_ty, None);
                    self.func_map.insert(
                        name,
                        FunctionVal {
                            block: None,
                            value: fn_value,
                        },
                    );
                } else {
                    let ty = self.context.void_type();
                    let fn_ty = ty.fn_type(&param_tys, is_var);
                    let fn_value = self.module.add_function(&name, fn_ty, None);
                    self.func_map.insert(
                        name,
                        FunctionVal {
                            block: None,
                            value: fn_value,
                        },
                    );
                }
                Value::Int(self.context.i64_type().const_int(0, false))
            }
            ExprKind::Let(name, ty, val) => {
                let ty = self.get_type(ty);
                let var_alloca = self.builder.build_alloca(ty, &name);
                if let Some(v) = *val {
                    let val = self.handle(v).as_basic_value();
                    self.builder.build_store(var_alloca, val);
                }
                self.var_map.insert(
                    name,
                    Variable {
                        value: var_alloca,
                        var_type: ty,
                    },
                );
                Value::Int(self.context.i64_type().const_int(0, false))
            }
            ExprKind::Bool(b) => {
                let bool = self.context.bool_type().const_int(b.into(), false);
                Value::Bool(bool)
            }
            ExprKind::String(s) => {
                let str = self
                    .builder
                    .build_global_string_ptr(s.trim_start_matches('"').trim_end_matches('"'), "")
                    .as_pointer_value();
                Value::Pointer(str)
            }
            _ => todo!(),
        }
    }
}
