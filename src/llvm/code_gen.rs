use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::BasicValue;
use inkwell::{AddressSpace, OptimizationLevel};

use crate::parser::Expr;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("bfc_jit");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap();

        Self {
            context,
            module,
            builder,
            execution_engine,
        }
    }

    pub fn process(&self, ast: &[Expr]) {
        for node in ast {
            match node {
                Expr::Let(var_name, var_type, var_value) => {
                    let var_type = self.get_int_type(var_type); // don't worry i will handler more types later
                    let var_alloca = self.builder.build_alloca(var_type, var_name);

                    if var_value.is_none() {
                        continue;
                    }

                    self.builder.build_store(
                        var_alloca,
                        match var_value.unwrap() {
                            Expr::Int(i) => {
                                if i < 0 {
                                    unimplemented!("negative numbers are not implemented yet");
                                }

                                var_type.into_int_type().const_int(
                                    i as _,
                                    if i < 0 { true } else { false },
                                );
                            }

                            Expr::Float(i) => {
                                if i < 0.0 {
                                    unimplemented!("negative numbers are not implemented yet");
                                }

                                var_type.into_float_type().const_int(
                                    i as _,
                                    if i < 0.0 { true } else { false },
                                );
                            }
                            _ => unreachable!(),
                        },
                    );
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn jit_run(&self) {}

    pub fn compile(&self) {}

    fn get_int_type(&self, type_name: &str) -> BasicTypeEnum {
        let i8_t = self.context.i8_type().as_basic_type_enum();
        let i16_t = self.context.i16_type().as_basic_type_enum();
        let i32_t = self.context.i32_type().as_basic_type_enum();
        let i64_t = self.context.i64_type().as_basic_type_enum();

        let f16_t = self.context.f16_type().as_basic_type_enum();
        let f32_t = self.context.f32_type().as_basic_type_enum();
        let f64_t = self.context.f64_type().as_basic_type_enum();

        match type_name {
            "i8" => i8_t,
            "i16" => i64_t,
            "i32" => i32_t,
            "i64" => i64_t,

            "f16" => f16_t,
            "f32" => f32_t,
            "f64" => f64_t,

            _ => panic!("invalid type of number found"),
        }
    }
}
