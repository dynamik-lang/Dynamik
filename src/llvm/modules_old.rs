use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;

use super::types::Value;


pub(crate) struct FunctionModule<'ctx> {
    context: &'ctx Context,
    modules: HashMap<String, Module<'ctx>>,
}

impl<'ctx> FunctionModule<'ctx> {
    pub(crate) fn new(context: &'ctx Context) -> Self {
        let mut modules = HashMap::new();

        let base_module = context.create_module(BASE_MOD);
        modules.insert(BASE_MOD.to_string(), base_module);

        Self { context, modules }
    }

    pub(crate) fn get_modules(&self) -> Vec<Module<'ctx>> {
        for m in self.modules.values() {
            m.print_to_stderr();
        }
        self.modules.values().map(|i| i.clone()).collect()
    }

    pub(crate) fn get_function(
        &self,
        module_name: &str,
        fn_name: &str,
    ) -> Option<FunctionValue<'ctx>> {
        let Some(module ) = self.modules.get(module_name) else {
            return None;
        };

        module.get_function(if fn_name == "__main__" {
            "main"
        } else {
            fn_name
        })
    }

    pub(crate) fn append_function(
        &self,
        module_name: &str,
        fn_name: &str,
        fn_ty: FunctionType<'ctx>,
        linkage: Option<Linkage>,
    ) -> Option<FunctionValue<'ctx>> {
        let Some(module ) = self.modules.get(module_name) else {
            return None;
        };

        let fn_val = module.add_function(
            if fn_name == "__main__" {
                "main"
            } else {
                fn_name
            },
            fn_ty,
            linkage,
        );

        Some(fn_val)
    }

    pub(crate) fn append_module(&mut self, module_name: &str) -> Option<()> {
        if self.modules.contains_key(module_name) {
            return None;
        }

        let _ = self
            .modules
            .insert(module_name.into(), self.context.create_module(module_name));

        Some(())
    }
}
