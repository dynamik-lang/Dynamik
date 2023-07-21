use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::parser::{Expr, ExprKind};

use super::helper as _;

pub struct CodeGen<'ctx> {
    pub(crate) context: &'ctx Context,
    pub(crate) module: Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,
    pub(crate) execution_engine: ExecutionEngine<'ctx>,
    // pub(crate) var_map: HashMap<String, PointerValue<'ctx>>,
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
            // var_map: HashMap::new(),
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

    pub(crate) fn get_number_type(&self, type_name: &str) -> Option<BasicTypeEnum> {
        let i64_t = self.context.i64_type().as_basic_type_enum();
        let f64_t = self.context.f64_type().as_basic_type_enum();

        match type_name {
            "int" => Some(i64_t),
            "float" => Some(f64_t),

            _ => None,
        }
    }
}
