use crate::parser::{BinaryOp, Expr, ExprKind};
use std::{collections::HashMap, path::Path};

use super::types::*;

const BASE_MOD: &str = "__base__";
const MAIN_FN: &str = "__base__::__main__";

use inkwell::module::Module;
use inkwell::values::FunctionValue;
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, FunctionLookupError, JitFunction, UnsafeFunctionPointer},
    module::Linkage,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum},
    AddressSpace, OptimizationLevel,
};

/// The root compiler
pub struct Compiler<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) target_machine: TargetMachine,
    pub(crate) exec_engine: ExecutionEngine<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) fn_map: HashMap<String, FunctionValue<'ctx>>,
    processed: bool,
}

impl<'ctx> Compiler<'ctx> {
    /// Create new compiler
    pub fn new(context: &'ctx Context, opt_level: OptimizationLevel) -> Self {
        let module = context.create_module("dynamik");
        let exec_engine = module.create_jit_execution_engine(opt_level).unwrap();
        let builder = context.create_builder();

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

        let mut fn_map = HashMap::new();

        let main_fn_ty = context.i32_type().fn_type(&[], false);
        let main_fn = module.add_function("main", main_fn_ty, None);
        let entry = context.append_basic_block(main_fn, "entry");

        builder.position_at_end(entry);

        fn_map.insert(MAIN_FN.to_string(), main_fn);

        Self {
            builder,
            target_machine,
            fn_map,
            module,
            context,
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

        let main_fn = self.fn_map[MAIN_FN];

        let mut var_map = HashMap::new();

        for node in ast {
            self.handle(
                node.clone(),
                &mut var_map,
                BASE_MOD,
                FunctionVal {
                    block: main_fn.get_last_basic_block(),
                    value: main_fn,
                },
            );
        }

        self.builder
            .position_at_end(main_fn.get_last_basic_block().unwrap());
        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)));

        // let _ = self.module.print_to_file("output.ll");

        Ok(())
    }

    pub fn compile(&mut self, file_name: &str, _opt_level: OptimizationLevel) {
        self.target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(file_name))
            .unwrap();
    }

    pub fn get_jit_function<F: UnsafeFunctionPointer>(
        &mut self,
        fn_name: &str,
    ) -> Result<JitFunction<'ctx, F>, FunctionLookupError> {
        if !self.processed {
            panic!("Not processed");
        }

        unsafe { self.exec_engine.get_function(fn_name) }
    }

    pub fn jit_run(&mut self) {
        // self.merged_module.print_to_stderr();
        unsafe {
            self.exec_engine
                .run_function_as_main(self.module.get_function("main").unwrap(), &[])
        };
    }

    /// Handle the node of AST
    fn handle(
        &mut self,
        node: Expr,
        var_map: &mut HashMap<String, Variable<'ctx>>,
        current_mod: &str,
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
                // println!("{i:?}");
                let ident = var_map.get(&i).unwrap();
                // Get the variable value
                match ident.var_type {
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
                let left = self.handle(*left, var_map, current_mod, current_function);
                let right = self.handle(*right, var_map, current_mod, current_function);

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

            ExprKind::FunctionCall(mod_name, name, params_) => {
                // println!("{mod_name:?}, {name:?}");
                let mod_name = mod_name.unwrap_or(current_mod.to_string());

                let function = self.fn_map[&format!("{mod_name}::{name}")];
                let mut params = Vec::new();

                for param in params_ {
                    params.push(BasicMetadataValueEnum::from(
                        self.handle(
                            param,
                            var_map,
                            current_mod,
                            FunctionVal {
                                block: function.get_last_basic_block(),
                                value: function,
                            },
                        )
                        .as_basic_value(),
                    ));
                }

                let return_value = self
                    .builder
                    .build_call(function, &params, "")
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

                let function = self.module.add_function(&name, fn_type, Some(Linkage::External));
                self.fn_map.insert(format!("{current_mod}::{name}"), function);

                Value::Int(self.context.i64_type().const_int(0, false))
            }

            ExprKind::Let(name, ty, val) => {
                let ty = self.get_type(ty);
                let var_alloca = self.builder.build_alloca(ty, &name);
                if let Some(v) = *val {
                    let val = self
                        .handle(v, var_map, current_mod, current_function)
                        .as_basic_value();
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

                let full_name = format!("{current_mod}::{name}");

                let function = self.module.add_function(&full_name, fn_type, None);

                self.fn_map.insert(full_name, function);

                let fn_entry = self.context.append_basic_block(function, "entry");

                self.builder.position_at_end(fn_entry);

                let mut function_scoped_var_map = var_map.clone();
                for (param_idx, param_name) in param_names.into_iter().enumerate() {
                    let param_value = function.get_nth_param(param_idx as _).unwrap();
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
                    self.handle(
                        node,
                        &mut function_scoped_var_map,
                        current_mod,
                        FunctionVal {
                            block: function.get_last_basic_block(),
                            value: function,
                        },
                    );
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
                let cmp = self
                    .handle(*condition, var_map, current_mod, current_function)
                    .as_bool();

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
                    self.handle(node, var_map, current_mod, current_function);
                }

                self.builder.build_unconditional_branch(after_block);

                // else block
                self.builder.position_at_end(else_block);
                if let Some(else_ast) = else_ast {
                    for node in else_ast {
                        self.handle(node, var_map, current_mod, current_function);
                    }
                }

                self.builder.build_unconditional_branch(after_block);

                // after block
                self.builder.position_at_end(after_block);

                Value::Int(self.context.i64_type().get_undef())
            }

            ExprKind::Return(ret) => {
                match ret.as_ref() {
                    Some(r) => {
                        let val = self.handle(r.clone(), var_map, current_mod, current_function);
                        self.builder.build_return(Some(&val.as_basic_value()));
                    }

                    _ => {
                        self.builder.build_return(None);
                    }
                }

                Value::Int(self.context.i64_type().get_undef())
            }

            ExprKind::Assignment(var_name, val) => {
                let val = self.handle(*val, var_map, current_mod, current_function);
                let variable_alloca = var_map[&var_name].ptr;

                self.builder
                    .build_store(variable_alloca, val.as_basic_value());

                Value::Int(self.context.i64_type().get_undef())
            }

            ExprKind::While(condition, inner) => {
                let loop_start_block = self
                    .context
                    .append_basic_block(current_function.value, "loop_start");
                let loop_continute_block = self
                    .context
                    .append_basic_block(current_function.value, "loop_continue");
                let after_block = self
                    .context
                    .append_basic_block(current_function.value, "after");

                self.builder.build_unconditional_branch(loop_start_block);

                // loop start block
                self.builder.position_at_end(loop_start_block);
                let cmp = self
                    .handle(*condition, var_map, current_mod, current_function)
                    .as_bool();
                self.builder
                    .build_conditional_branch(cmp, loop_continute_block, after_block);

                // loop continue block
                self.builder.position_at_end(loop_continute_block);
                for node in inner {
                    self.handle(node, var_map, current_mod, current_function);
                }

                self.builder.build_unconditional_branch(loop_start_block);

                // loop after block
                self.builder.position_at_end(after_block);

                Value::Int(self.context.i64_type().get_undef())
            }

            ExprKind::Mod(mod_name, inner) => {
                let mut var_map = HashMap::new();
                for node in inner {
                    assert!(matches!(node.inner, ExprKind::Function(..) | ExprKind::ExternFunction(..)), "Modules can only contain function and global constants (not implemented yet)");
                    self.handle(node, &mut var_map, &mod_name, current_function);
                }

                self.builder
                    .position_at_end(current_function.block.unwrap());

                Value::Int(self.context.i64_type().get_undef())
            }

            i => unimplemented!("unimplemented: {i:?}"),
        }
    }
}
