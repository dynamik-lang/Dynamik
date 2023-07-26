use crate::parser::{BinaryOp, Expr, ExprKind};
use std::{collections::HashMap, path::Path};

use super::types::*;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, FunctionLookupError, JitFunction, UnsafeFunctionPointer},
    module::{Linkage, Module},
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum},
    AddressSpace, OptimizationLevel,
};

/// The root compiler
pub struct Compiler<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) exec_engine: ExecutionEngine<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) fn_map: HashMap<String, FunctionVal<'ctx>>,
    processed: bool,
}

impl<'ctx> Compiler<'ctx> {
    /// Create new compiler
    pub fn new(context: &'ctx Context, opt_level: OptimizationLevel) -> Self {
        let module = context.create_module("dynamik");
        let exec_engine = module.create_jit_execution_engine(opt_level).unwrap();
        let builder = context.create_builder();
        let main_fun_ty = context.i32_type().fn_type(&[], false);
        let main_fun = module.add_function("main", main_fun_ty, None);
        let entry = context.append_basic_block(main_fun, "entry");
        builder.position_at_end(entry);

        let mut fn_map = HashMap::new();

        // Creating the function
        fn_map.insert(
            "__main__".into(),
            FunctionVal {
                block: Some(entry),
                value: main_fun,
            },
        );

        Self {
            builder,
            module,
            context,
            fn_map,
            exec_engine,
            processed: false,
        }
    }

    fn get_type(&self, s: String) -> BasicTypeEnum<'ctx> {
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

    pub fn process(&mut self, ast: &[Expr]) -> Result<(), String> {
        if self.processed {
            return Err("Already processed".into());
        }

        self.processed = true;

        let main_fn = self.fn_map["__main__"];
        let mut var_map = HashMap::new();

        for node in ast {
            self.handle(node.clone(), &mut var_map, main_fn);
        }

        let main_fn = self.fn_map["__main__"];

        self.builder.position_at_end(main_fn.block.unwrap());

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)));

        // self.module.print_to_stderr();

        Ok(())
    }

    pub fn compile(&mut self, file_name: &str, opt_level: OptimizationLevel) {
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");

        let target_triplet = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triplet).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triplet,
                "generic", // cpu
                "",
                opt_level,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(file_name))
            .unwrap();
    }

    pub fn get_jit_function<F: UnsafeFunctionPointer>(
        &mut self,
        fn_name: &str,
    ) -> Result<JitFunction<'ctx, F>, FunctionLookupError> {
        unsafe { self.exec_engine.get_function(fn_name) }
    }

    pub fn jit_run(&mut self) {
        unsafe {
            self.exec_engine
                .run_function_as_main(self.fn_map["__main__"].value, &[])
        };
    }

    /// Handle the node of AST
    fn handle(
        &mut self,
        node: Expr,
        var_map: &mut HashMap<String, Variable<'ctx>>,
        current_function: FunctionVal<'ctx>,
    ) -> Value<'ctx> {
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
                let ident = var_map.get(&i).unwrap();
                // Get the variable value
                match ident.clone().var_type {
                    BasicTypeEnum::FloatType(_) => Value::Float(
                        self.builder
                            .build_load(ident.var_type, ident.ptr, "")
                            .into_float_value(),
                    ),
                    BasicTypeEnum::IntType(_) => Value::Int(
                        self.builder
                            .build_load(ident.var_type, ident.ptr, "")
                            .into_int_value(),
                    ),
                    BasicTypeEnum::PointerType(_) => Value::Pointer(
                        self.builder
                            .build_load(ident.var_type, ident.ptr, "")
                            .into_pointer_value(),
                    ),
                    _ => todo!(),
                }
            }
            ExprKind::Binary(left, op, right) => {
                let left = self.handle(*left, var_map, current_function);
                let right = self.handle(*right, var_map, current_function);

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
                        }
                         else {
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
                    let function = *self.fn_map.get(&name).unwrap();
                    let mut params = Vec::new();

                    for param in params_ {
                        params.push(BasicMetadataValueEnum::from(
                            self.handle(param, var_map, function).as_basic_value(),
                        ))
                    }

                    let return_value = self
                        .builder
                        .build_call(function.value, &params, "")
                        .try_as_basic_value()
                        .left();

                    match return_value {
                        Some(t) => match t {
                            BasicValueEnum::IntValue(i) => Value::Int(i),
                            BasicValueEnum::FloatValue(f) => Value::Float(f),
                            BasicValueEnum::PointerValue(p) => Value::Pointer(p),
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
                    param_tys.push(self.get_type(p).into());
                }

                let fn_type = if let Some(ret_ty) = ret_ty {
                    self.get_type(ret_ty).fn_type(&param_tys, is_var)
                } else {
                    self.context.void_type().fn_type(&param_tys, is_var)
                };

                let function = self
                    .module
                    .add_function(&name, fn_type, Some(Linkage::External));

                self.fn_map.insert(
                    name,
                    FunctionVal {
                        value: function,
                        block: None,
                    },
                );

                Value::Int(self.context.i64_type().const_int(0, false))
            }

            ExprKind::Let(name, ty, val) => {
                let ty = self.get_type(ty);
                let var_alloca = self.builder.build_alloca(ty, &name);
                if let Some(v) = *val {
                    let val = self.handle(v, var_map, current_function).as_basic_value();
                    self.builder.build_store(var_alloca, val);
                }

                var_map.insert(
                    name,
                    Variable {
                        ptr: var_alloca,
                        var_type: ty,
                    },
                );

                Value::Int(self.context.i64_type().const_int(0, false))
            }

            ExprKind::Function(name, args, return_type, inner) => {
                let mut params = Vec::with_capacity(args.len());
                let mut param_names = Vec::with_capacity(args.len());

                for (arg_name, arg_type) in args.into_iter() {
                    param_names.push(arg_name);
                    params.push(self.get_type(arg_type));
                }

                let return_type = self.get_type(return_type.unwrap_or("void".into()));
                let fn_type = return_type.fn_type(
                    params
                        .iter()
                        .map(|i| i.clone().into())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                );

                let function = self.module.add_function(&name, fn_type, None);
                let fn_entry = self.context.append_basic_block(function, "entry");

                let function = FunctionVal {
                    value: function,
                    block: Some(fn_entry),
                };

                self.builder.position_at_end(fn_entry);

                let mut function_scoped_var_map = var_map.clone();
                for (param_idx, param_name) in param_names.into_iter().enumerate() {
                    let param_value = function.value.get_nth_param(param_idx as _).unwrap();
                    let param_type = params[param_idx];

                    let param_ptr = self.builder.build_alloca(param_type, "");

                    self.builder.build_store(param_ptr, param_value);

                    function_scoped_var_map.insert(
                        param_name,
                        Variable {
                            ptr: param_ptr,
                            var_type: param_type,
                        },
                    );
                }

                for node in inner {
                    self.handle(node, var_map, function);
                }

                Value::Int(self.context.i64_type().get_undef())
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

            ExprKind::If(condition, then_ast, else_ast) => {
                let cmp = self.handle(*condition, var_map, current_function).as_bool();

                let then_block = self
                    .context
                    .append_basic_block(current_function.value, "then");
                let else_block = self
                    .context
                    .append_basic_block(current_function.value, "else");
                let after_block = self
                    .context
                    .append_basic_block(current_function.value, "after");

                self.builder
                    .build_conditional_branch(cmp, then_block, else_block);

                // then block
                self.builder.position_at_end(then_block);
                for node in then_ast {
                    self.handle(node, var_map, current_function);
                }

                self.builder.build_unconditional_branch(after_block);

                // else block
                self.builder.position_at_end(else_block);
                if let Some(else_ast) = else_ast {
                    for node in else_ast {
                        self.handle(node, var_map, current_function);
                    }
                }

                self.builder.build_unconditional_branch(after_block);

                // after block
                self.builder.position_at_end(after_block);
                *self
                    .fn_map
                    .get_mut("__main__")
                    .unwrap()
                    .block
                    .as_mut()
                    .unwrap() = after_block;

                Value::Int(self.context.i64_type().get_undef())
            }

            ExprKind::Return(ret) => {
                match ret.as_ref() {
                    Some(r) => {
                        let val = self.handle(r.clone(), var_map, current_function);
                        self.builder.build_return(Some(&val.as_basic_value()));
                    }

                    _ => {
                        self.builder.build_return(None);
                    }
                }

                Value::Int(self.context.i64_type().get_undef())
            }

            i => unimplemented!("unimplemented: {i:?}"),
        }
    }
}
