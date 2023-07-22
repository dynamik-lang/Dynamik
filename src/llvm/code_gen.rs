use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, FloatValue, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::parser::{BinaryOp, Expr, ExprKind};

use super::helper as _;

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    pub(crate) var_map: HashMap<String, PointerValue<'ctx>>,
    pub(crate) fn_map: HashMap<String, (FunctionValue<'ctx>, BasicBlock<'ctx>)>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("dynamik");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        let main_fn_type = context.i32_type().fn_type(&[], false);
        let main_fn = module.add_function("main", main_fn_type, None);

        let entry = context.append_basic_block(main_fn, "entry");
        builder.position_at_end(entry);

        let mut fn_map = HashMap::new();
        fn_map.insert("__main__".to_string(), (main_fn, entry));

        Self {
            context,
            module,
            builder,
            execution_engine,
            var_map: HashMap::new(),
            fn_map,
        }
    }

    fn process(&mut self, ast: &[Expr], var_map: &mut HashMap<String, PointerValue<'ctx>>) {
        let i64_t = self.context.i64_type().as_basic_type_enum();
        let f64_t = self.context.f64_type().as_basic_type_enum();

        let get_type = |t: String| match t.as_str() {
            "int" => Some(i64_t),
            "float" => Some(f64_t),

            _ => None,
        };

        for node in ast {
            match &node.inner {
                ExprKind::Let(var_name, var_type, var_value) => {
                    self.define_var(var_name, var_type, var_value);
                }

                ExprKind::Function(name, args, return_type, inner) => {
                    let num_parms = args.len();
                    let parameters_names = args
                        .iter()
                        .map(|(name, _)| name.clone())
                        .collect::<Vec<_>>();

                    let parameters = args.iter().map(|(_name, i)| i.clone()).collect::<Vec<_>>();
                    let parameters = parameters
                        .into_iter()
                        .map(|i| {
                            if let Some(n_type) = get_type(i) {
                                n_type.into()
                            } else {
                                unimplemented!("custom types are not implemented")
                            }
                        })
                        .collect::<Vec<_>>();

                    let fn_type;
                    if let Some(return_type) = return_type {
                        let t = get_type(return_type.to_string());

                        if let Some(n_type) = t {
                            fn_type = if return_type.starts_with('i') {
                                n_type.into_int_type().fn_type(&parameters, false)
                            } else {
                                n_type.into_float_type().fn_type(&parameters, false)
                            };
                        } else {
                            unimplemented!("custom types are not implemented")
                        }
                    } else {
                        fn_type = self.context.void_type().fn_type(&parameters, false);
                    };

                    let function = self.module.add_function(name, fn_type, None);
                    let entry = self.context.append_basic_block(function, "entry");

                    self.builder.position_at_end(entry);
                    self.fn_map.insert(name.to_string(), (function, entry));

                    let mut var_map_new = var_map.clone();

                    parameters_names.iter().enumerate().for_each(|(i, name)| {
                        var_map_new.insert(
                            name.to_string(),
                            function.get_nth_param(i as _).unwrap().into_pointer_value(),
                        );
                    });

                    self.process(inner, &mut var_map_new);

                    self.builder
                        .position_at_end(self.fn_map.get("__main__").unwrap().1);
                }

                // for testing
                // b @ ExprKind::Binary(..) => {
                //     let p = self.builder.build_alloca(self.context.i64_type(), "");
                //     self.eval(b.clone(), p);
                // }

                _ => {}
            }
        }
    }

    pub fn jit_run(&mut self, ast: &[Expr]) {
        let mut var_map = HashMap::new();
        self.process(ast, &mut var_map);
        self.module.print_to_stderr();
    }

    // pub fn compile(&self) {}

    pub(crate) fn get_number_type(
        context: &'ctx Context,
        type_name: &str,
    ) -> Option<BasicTypeEnum<'ctx>> {
        let i64_t = context.i64_type().as_basic_type_enum();
        let f64_t = context.f64_type().as_basic_type_enum();

        match type_name {
            "int" => Some(i64_t),
            "float" => Some(f64_t),

            _ => None,
        }
    }

    pub(crate) fn get_type(binary_op: &ExprKind) -> VarType {
        use ExprKind::*;
        let Binary(lhs, _, rhs) = binary_op else {
            panic!("cannot eval anything except BinaryOp");
        };

        let (lhs, rhs) = (&lhs.inner, &rhs.inner);

        match (lhs, rhs) {
            (Int(_), _) | (_, Int(_)) => VarType::Int,
            (Float(_), _) | (_, Float(_)) => VarType::Float,
            (b @ Binary(..), _) | (_, b @ Binary(..)) => Self::get_type(&b),

            _ => unreachable!("Hamza not doing his work"),
        }
    }

    pub(crate) fn eval(&self, binary_op: ExprKind, ptr: PointerValue) {
        use BinaryOp::*;
        use ExprKind::*;

        let Binary(lhs, op, rhs) = binary_op else {
            panic!("cannot eval anything except BinaryOp");
        };

        let (lhs, rhs) = (lhs.inner, rhs.inner);

        match (lhs, op, rhs) {
            (Int(lhs), op, Int(rhs)) => {
                let (lhs, rhs) = (self.create_int(lhs), self.create_int(rhs));
                let res = match op {
                    Add => self.builder.build_int_add(lhs, rhs, "binary_op_int_add"),

                    Sub => self.builder.build_int_sub(lhs, rhs, "binary_op_int_sub"),

                    Div => self
                        .builder
                        .build_int_signed_div(lhs, rhs, "binary_op_int_div"),

                    Mul => self.builder.build_int_mul(lhs, rhs, "binary_op_int_mul"),

                    _ => unreachable!(),
                };

                self.builder.build_store(ptr, res);
            }

            (Float(lhs), op, Float(rhs)) => {
                let (lhs, rhs) = (self.create_float(lhs), self.create_float(rhs));
                let res = match op {
                    Add => self
                        .builder
                        .build_float_add(lhs, rhs, "binary_op_float_add"),

                    Sub => self
                        .builder
                        .build_float_sub(lhs, rhs, "binary_op_float_sub"),

                    Div => self
                        .builder
                        .build_float_div(lhs, rhs, "binary_op_float_div"),

                    Mul => self
                        .builder
                        .build_float_mul(lhs, rhs, "binary_op_float_sub"),

                    _ => unreachable!(),
                };

                self.builder.build_store(ptr, res);
            }

            (b @ Binary(..), op, Int(other)) | (Int(other), op, b @ Binary(..)) => {
                let new_ptr = self
                    .builder
                    .build_alloca(self.context.f64_type(), "new_alloca");
                self.eval(b, new_ptr);

                let lhs = self
                    .builder
                    .build_load(self.context.i64_type(), new_ptr, "");

                let (lhs, rhs) = (lhs.into_int_value(), self.create_int(other));

                let res = match op {
                    Add => self.builder.build_int_add(lhs, rhs, ""),
                    Sub => self.builder.build_int_sub(lhs, rhs, ""),
                    Div => self.builder.build_int_signed_div(lhs, rhs, ""),
                    Mul => self.builder.build_int_mul(lhs, rhs, ""),

                    _ => unreachable!(),
                };

                self.builder.build_store(ptr, res);
            }

            (b @ Binary(..), op, Float(other)) | (Float(other), op, b @ Binary(..)) => {
                let new_ptr = self
                    .builder
                    .build_alloca(self.context.f64_type(), "new_alloca");
                self.eval(b, new_ptr);

                let lhs = self
                    .builder
                    .build_load(self.context.f64_type(), new_ptr, "");

                let (lhs, rhs) = (lhs.into_float_value(), self.create_float(other));

                let res = match op {
                    Add => self.builder.build_float_add(lhs, rhs, ""),
                    Sub => self.builder.build_float_sub(lhs, rhs, ""),
                    Div => self.builder.build_float_div(lhs, rhs, ""),
                    Mul => self.builder.build_float_mul(lhs, rhs, ""),

                    _ => unreachable!(),
                };

                self.builder.build_store(ptr, res);
            }

            (lhs_b @ Binary(..), op, rhs_b @ Binary(..)) => {
                let ty = Self::get_type(&lhs_b);
                let (new_ptr_lhs, new_ptr_rhs) = match ty {
                    VarType::Int => (
                        self.builder
                            .build_alloca(self.context.i64_type(), "lhs_alloca"),
                        self.builder
                            .build_alloca(self.context.i64_type(), "rhs_alloca"),
                    ),

                    VarType::Float => (
                        self.builder
                            .build_alloca(self.context.f64_type(), "lhs_alloca"),
                        self.builder
                            .build_alloca(self.context.f64_type(), "rhs_alloca"),
                    ),
                };

                self.eval(lhs_b, new_ptr_lhs);
                self.eval(rhs_b, new_ptr_rhs);

                let res = match ty {
                    VarType::Int => {
                        let (lhs, rhs) = (
                            self.builder
                                .build_load(self.context.i64_type(), new_ptr_lhs, "")
                                .into_int_value(),
                            self.builder
                                .build_load(self.context.i64_type(), new_ptr_rhs, "")
                                .into_int_value(),
                        );

                        match op {
                            Add => self.builder.build_int_add(lhs, rhs, ""),
                            Sub => self.builder.build_int_sub(lhs, rhs, ""),
                            Div => self.builder.build_int_signed_div(lhs, rhs, ""),
                            Mul => self.builder.build_int_mul(lhs, rhs, ""),

                            _ => unreachable!(),
                        }.as_basic_value_enum()
                    }

                    VarType::Float => {
                        let (lhs, rhs) = (
                            self.builder
                                .build_load(self.context.f64_type(), new_ptr_lhs, "")
                                .into_float_value(),
                            self.builder
                                .build_load(self.context.f64_type(), new_ptr_rhs, "")
                                .into_float_value(),
                        );

                        match op {
                            Add => self.builder.build_float_add(lhs, rhs, ""),
                            Sub => self.builder.build_float_sub(lhs, rhs, ""),
                            Div => self.builder.build_float_div(lhs, rhs, ""),
                            Mul => self.builder.build_float_mul(lhs, rhs, ""),

                            _ => unreachable!(),
                        }.as_basic_value_enum()
                    }
                };

                self.builder.build_store(ptr, res);
            }

            _ => unreachable!("Hamza not doing his work"),
        }
    }

    pub(crate) fn create_int(&self, val: i64) -> IntValue {
        let i = self.context.i64_type().const_int(val.unsigned_abs(), false);

        if val < 0 {
            return i.const_neg();
        }

        i
    }

    pub(crate) fn create_float(&self, val: f64) -> FloatValue {
        self.context.f64_type().const_float(val)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum VarType {
    Int,
    Float,
}
