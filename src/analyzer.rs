use crate::{
    parser::{parser, Expr, ExprKind, LogosToken},
    typechecker::TypeChecker,
};
use chumsky::prelude::*;
use chumsky::Parser;
use chumsky::{input::Stream, span::SimpleSpan};
use logos::Logos;
use miette::{miette, LabeledSpan, Report};
use std::{collections::HashMap, ops::Range};
#[derive(Clone, Copy, Debug)]
pub enum ScopeVal {
    Function(usize),
    FunctionVariadic,
    Variable,
}
pub struct Analyzer {
    pub ast: Vec<Expr>,
    scopes: Vec<HashMap<String, ScopeVal>>,
    errors: Vec<Report>,
    modules: Vec<String>,
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
            modules: vec![],
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
            ExprKind::Unary(_, x) => {
                self.check(*x);
            }
            ExprKind::Mod(x, b) => {
                self.check_module(x.clone(), b, x);
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
            ExprKind::ExternFunction(name, params, _, is_var) => {
                let ty = if is_var {
                    ScopeVal::FunctionVariadic
                } else {
                    ScopeVal::Function(params.len())
                };
                self.scopes.last_mut().unwrap().insert(name, ty);
            }
            ExprKind::FunctionCall(mod_name, name, params) => {
                let name = if mod_name.is_some() && mod_name.as_ref().unwrap().len() > 0 {
                    format!(
                        "{}::{}",
                        mod_name
                            .unwrap()
                            .iter()
                            .enumerate()
                            .map(|(i, s)| {
                                if i > 0 {
                                    format!("::{}", s)
                                } else {
                                    s.to_string()
                                }
                            })
                            .collect::<String>(),
                        name
                    )
                } else {
                    name
                };
                println!("{:?}", self.scopes.last().unwrap());
                if let Some(ScopeVal::Function(size)) = self.get(name.clone()) {
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
                } else if let Some(ScopeVal::FunctionVariadic) = self.get(name.clone()) {
                    return;
                } else {
                    self.basic_err("Function not found".into(), ast.span)
                }
            }
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
    fn check_module(&mut self, name: String, items: Option<Vec<Expr>>, first: String) {
        if items.is_some() {
            self.start_scope();
            for item in items.unwrap().into_iter() {
                match item.inner.clone() {
                    ExprKind::Mod(m, x) => {
                        self.modules.push(m.to_owned());
                        self.check_module(m.clone(), x, format!("{}::{}", first.clone(), m));
                    }
                    ExprKind::Function(name, params, _, _) => {
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(name, ScopeVal::Function(params.len()));
                    }
                    ExprKind::Let(v_name, _, _) => self.basic_err(
                        "Variables are not allowed inside modules, use constants instead!"
                            .to_owned(),
                        item.span,
                    ),
                    _ => self.check(item),
                }
            }
            let map = self.scopes.clone();

            self.end_scope();
            for (key, value) in map.last().unwrap() {
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(format!("{}::{}", first, key), value.to_owned());
            }
            println!("{:?}", self.scopes)
        } else {
            let src = match std::fs::read_to_string(format!("{}.dy", name)) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("File not found: {}.dy", name);
                    std::process::exit(1);
                }
            };

            let token_iter = LogosToken::lexer(&src)
                .spanned()
                .map(|(tok, span)| match tok {
                    Ok(tok) => (tok, span.into()),
                    Err(()) => (LogosToken::Error, span.into()),
                });

            let token_stream = Stream::from_iter(token_iter)
                .spanned::<LogosToken, SimpleSpan>((src.len()..src.len()).into());

            match parser().parse(token_stream).into_result() {
                Ok(o) => {
                    let mut analyzer = Analyzer::new(o.clone(), &src);
                    if analyzer.analyze() {
                        let mut checker = TypeChecker::new(o.clone(), &src);
                        self.start_scope();
                        if checker.typecheck() {
                            for node in o {
                                match node.inner.clone() {
                                    ExprKind::Mod(m, x) => {
                                        self.modules.push(m.to_owned());
                                        self.check_module(m.clone(), x, format!("{}::{}", first.clone(), m));
                                    }
                                    ExprKind::Function(name, params, _, _) => {
                                        self.scopes
                                            .last_mut()
                                            .unwrap()
                                            .insert(name, ScopeVal::Function(params.len()));
                                    }
                                    ExprKind::Let(v_name, _, _) => self.basic_err(
                                        "Variables are not allowed inside modules, use constants instead!"
                                            .to_owned(),
                                        node.span,
                                    ),
                                    _ => self.check(node),
                                }
                            }
                        };
                        let map = self.scopes.clone();

                        self.end_scope();
                        for (key, value) in map.last().unwrap() {
                            self.scopes
                                .last_mut()
                                .unwrap()
                                .insert(format!("{}::{}", first, key), value.to_owned());
                        }
                        println!("{:?}", self.scopes)
                    }
                }

                Err(errs) => {
                    for err in errs {
                        let span: std::ops::Range<usize> = (*err.span()).into();
                        let reason = err.reason().to_string();
                        println!(
                            "{:?}",
                            miette!(
                                labels = vec![LabeledSpan::at(span, reason)],
                                "Parsing error"
                            )
                            .with_source_code(src.clone())
                        );
                    }

                    std::process::exit(1);
                }
            };
        }
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
