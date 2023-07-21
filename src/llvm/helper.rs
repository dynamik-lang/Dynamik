use inkwell::values::{BasicValue, PointerValue};
use std::collections::HashMap;

use crate::parser::BinaryOp;
use crate::parser::Expr;
use crate::parser::ExprKind;

use super::code_gen::CodeGen;

impl<'ctx> CodeGen<'ctx> {
    pub(crate) fn define_var(&self, var_name: &str, var_type: &str, var_value: &Box<Option<Expr>>) {
        // static mut VAR_MAP: HashMap<String, PointerValue> = HashMap::new();

        let var_type = self.get_number_type(var_type); // don't worry i will handler more types later (stfu)
        let var_alloca = self.builder.build_alloca(var_type, var_name);

        if var_value.is_some() {
            match &var_value.as_ref().as_ref().unwrap().inner {
                ExprKind::Int(i) => {
                    if *i < 0 {
                        unimplemented!("negative numbers are not implemented yet");
                    }

                    let val = var_type.into_int_type().const_int(i.unsigned_abs(), false);

                    if *i < 0 {
                        val.const_neg().as_basic_value_enum();
                    } else {
                        val.as_basic_value_enum();
                    }

                    self.builder.build_store(var_alloca, val);
                }

                ExprKind::Float(i) => {
                    if *i < 0.0 {
                        unimplemented!("negative numbers are not implemented yet");
                    }

                    self.builder.build_store(
                        var_alloca,
                        var_type
                            .into_float_type()
                            .const_float(*i)
                            .as_basic_value_enum(),
                    );
                }

                ExprKind::Binary(a, op, b) => {
                    todo!()
                }
                _ => unreachable!(),
            }
        }
    }
}
