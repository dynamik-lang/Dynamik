use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::BasicValue;
use inkwell::{AddressSpace, OptimizationLevel};

use crate::parser::Expr;

use super::helper as _;

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("dynamik");
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
        let i32_type = self.get_number_type("i32").into_int_type();
        let main_fn_type = self.context.i32_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);

        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        for node in ast {
            match node {
                Expr::Let(var_name, var_type, var_value) => {
                    self.define_var(var_name, var_type, var_value)
                }

                // Expr::Function(, , , )
                _ => unreachable!(),
            }
        }
    }

    pub fn jit_run(&self) {}

    pub fn compile(&self) {}

    pub(crate) fn get_number_type(&self, type_name: &str) -> BasicTypeEnum {
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
