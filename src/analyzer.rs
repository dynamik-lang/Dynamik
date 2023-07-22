use std::{collections::HashMap, ops::Range};

use crate::parser::{Expr, ExprKind};
use miette::{miette, LabeledSpan, Report};
#[derive(Clone, Copy, Debug)]
pub enum ScopeVal {
    Function(usize),
    Variable,
}
pub struct Analyzer {
    pub ast: Vec<Expr>,
    scopes: Vec<HashMap<String, ScopeVal>>,
    errors: Vec<Report>,
    in_func: usize,
    source: String,
}
impl Analyzer {
    pub fn new(ast: Vec<Expr>, source: &str) -> Self {
        Self {
            ast,
            errors: Vec::new(),
            source: source.to_owned(),
            in_func: 0,
            scopes: vec![HashMap::new()],
        }
    }
    pub fn analyze(&mut self) -> bool {
        for node in self.ast.clone() {
            self.check(node);
        }

        for error in &self.errors {
            println!("{:?}", error)
        }
        self.errors.is_empty()
    }
    fn check(&mut self, ast: Expr) {
        match ast.inner {
            ExprKind::Ident(ident) => {
                if self.get(ident.clone()).is_none() {
                    self.basic_err(
                        format!("variable {} is not found in the current scope", ident),
                        ast.span,
                    );
                }
            }
            ExprKind::If(condition, block, else_block) => {
                self.check(*condition);
                self.start_scope();
                for e in block {
                    self.check(e);
                }
                self.end_scope();
                if let Some(block) = else_block {
                    self.start_scope();
                    for e in block {
                        self.check(e)
                    }
                    self.end_scope();
                }
            }
            ExprKind::ExternFunction(name, params, _) => {
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name, ScopeVal::Function(params.len()));
            }
            ExprKind::FunctionCall(callee, params) => match callee.inner {
                ExprKind::Ident(name) => {
                    if let Some(ScopeVal::Function(size)) = self.get(name) {
                        if params.len() != size {
                            self.basic_err(
                                format!(
                                    "expected function to have {} arguments found {}",
                                    size,
                                    params.len()
                                ),
                                ast.span,
                            )
                        }
                    }
                }
                _ => self.basic_err("This is not callable".into(), callee.span),
            },
            ExprKind::Function(name, params, _, block) => {
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name, ScopeVal::Function(params.len()));
                self.start_scope();
                self.in_func += 1;

                for (p_name, _) in params {
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(p_name, ScopeVal::Variable);
                }
                for stmt in block {
                    self.check(stmt);
                }
                self.in_func -= 1;
                self.end_scope();
            }
            ExprKind::Let(name, _, expr) => {
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name, ScopeVal::Variable);
                if let Some(e) = *expr {
                    self.check(e)
                }
            }
            ExprKind::Return(expr) => {
                if self.in_func < 1 {
                    self.basic_err("Cannot return outside a function".into(), ast.span)
                }
                if let Some(e) = *expr {
                    self.check(e)
                }
            }
            ExprKind::Binary(lhs, _, rhs) => {
                self.check(*lhs);
                self.check(*rhs);
            }
            _ => {}
        }
    }

    fn get(&self, ident: String) -> Option<ScopeVal> {
        self.scopes
            .iter()
            .rev()
            .find_map(|map| map.get(&ident))
            .cloned()
    }
    fn start_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
    fn basic_err(&mut self, message: String, span: Range<usize>) {
        self.errors.push(
            miette!(
                labels = vec![LabeledSpan::at(span, message)],
                "Analyzer Error"
            )
            .with_source_code(self.source.clone()),
        )
    }
}
