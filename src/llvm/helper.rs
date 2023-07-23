use std::collections::HashMap;
use std::str::FromStr;

use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;

use crate::parser::Expr;
use crate::parser::ExprKind;

use super::code_gen::CodeGen;
use super::code_gen::VarType;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn define_var(
        &mut self,
        var_name: &str,
        var_type: &str,
        var_value: &Option<Expr>,
        var_map: &mut HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    ) {
        let var_type = match VarType::from_str(var_type).unwrap() {
            VarType::Int => self.context.i64_type().as_basic_type_enum(),
            VarType::Float => self.context.i64_type().as_basic_type_enum(),
            VarType::Bool => self.context.bool_type().as_basic_type_enum(),
        };

        let var_alloca = self.builder.build_alloca(var_type, var_name);

        var_map.insert(var_name.to_string(), (var_alloca, var_type));

        if var_value.is_some() {
            match &var_value.as_ref().unwrap().inner {
                ExprKind::Int(i) => {
                    self.builder.build_store(var_alloca, self.create_int(*i));
                }

                ExprKind::Float(i) => {
                    self.builder.build_store(var_alloca, self.create_float(*i));
                }

                ExprKind::Bool(b) => {
                    self.builder.build_store(var_alloca, self.create_bool(*b));
                }

                b @ ExprKind::Binary(..) => {
                    self.eval(b, var_alloca, var_map);
                }

                ExprKind::Ident(ident) => {
                    let (ident_alloca, ident_type) = var_map.get(ident).unwrap();
                    let stored_value = self.builder.build_load(*ident_type, *ident_alloca, "");

                    self.builder.build_store(var_alloca, stored_value);
                }

                ExprKind::FunctionCall(function_name, args) => {
                    let ExprKind::Ident(ref function_name) = function_name.as_ref().inner else {
                        panic!("function name is not a string");
                    };

                    let return_value = self.call_function(function_name, args, var_map).unwrap();

                    self.builder.build_store(var_alloca, return_value);
                }

                _ => unreachable!(),
            }
        }
    }

    pub(crate) fn call_function(
        &self,
        function_name: &str,
        args: &[Expr],
        var_map: &mut HashMap<String, (PointerValue<'ctx>, BasicTypeEnum<'ctx>)>,
    ) -> Option<BasicValueEnum> {
        // will be removed in the future
        let function = if function_name == "printf" {
            self.fn_map.get("printf").unwrap().0
        } else {
            self.fn_map.get(function_name).unwrap().0
        };

        let args = args
            .iter()
            .map(|i| match &i.inner {
                ExprKind::Int(x) => self.create_int(*x).into(),
                ExprKind::Float(x) => self.create_float(*x).into(),
                ExprKind::Bool(x) => self.create_bool(*x).into(),
                ExprKind::String(s) => self
                    .builder
                    .build_global_string_ptr(s.trim_start_matches('"').trim_end_matches('"'), "")
                    .as_basic_value_enum()
                    .into(),
                ExprKind::Ident(i) => {
                    let (var_ptr, var_ty) = var_map.get(i).unwrap();
                    self.builder.build_load(*var_ty, *var_ptr, "").into()
                }
                b @ ExprKind::Binary(..) => {
                    let ty = VarType::create(b, var_map);
                    let ty = match ty {
                        VarType::Int => self.context.i64_type().as_basic_type_enum(),
                        VarType::Float => self.context.f64_type().as_basic_type_enum(),
                        VarType::Bool => self.context.bool_type().as_basic_type_enum(),
                    };

                    let alloca = self.builder.build_alloca(ty, "");
                    self.eval(b, alloca, var_map);
                    self.builder.build_load(ty, alloca, "").into()
                }
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        // the return value
        self.builder
            .build_call(function, args.as_slice(), "")
            .try_as_basic_value()
            .left()
    }
}
