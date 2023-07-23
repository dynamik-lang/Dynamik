use std::str::FromStr;

use inkwell::types::BasicType;

use crate::parser::Expr;
use crate::parser::ExprKind;

use super::code_gen::CodeGen;
use super::code_gen::VarType;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn define_var(&mut self, var_name: &str, var_type: &str, var_value: &Option<Expr>) {
        let var_type = match VarType::from_str(var_type).unwrap() {
            VarType::Int => self.context.i64_type().as_basic_type_enum(),
            VarType::Float => self.context.i64_type().as_basic_type_enum(),
            VarType::Bool => self.context.bool_type().as_basic_type_enum(),
        };

        let var_alloca = self.builder.build_alloca(var_type, var_name);

        self.var_map.insert(var_name.to_string(), var_alloca);

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
                    self.eval(b, var_alloca);
                }

                _ => unreachable!(),
            }
        }
    }
}
