use miette::{miette, LabeledSpan, Report};
use std::{collections::HashMap, ops::Range};

use crate::parser::{Expr, ExprKind};
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLike {
    pub parameters: HashMap<String, TypeForm>,
    pub return_type: Box<Option<TypeForm>>,
}
#[derive(Debug, Clone, PartialEq)]
pub enum TypeForm {
    Int,
    Float,
    Bool,
    String,
    Function(FunctionLike),
    Void,
}

pub struct TypeChecker {
    pub ast: Vec<Expr>,
    scopes: Vec<HashMap<String, TypeForm>>,
    errors: Vec<Report>,
    source: String,
}
impl TypeChecker {
    pub fn new(ast: Vec<Expr>, source: &str) -> Self {
        Self {
            ast,
            errors: Vec::new(),
            source: source.to_owned(),
            scopes: vec![HashMap::new()],
        }
    }
    pub fn typecheck(&mut self) {
        for node in self.ast.clone() {
            self.check(node);
        }
        for error in &self.errors {
            println!("{:?}", error)
        }
    }
    fn check(&mut self, node: Expr) -> Option<TypeForm> {
        match node.clone().inner {
            ExprKind::Let(name, ty, value) => {
                let ty = self.get_type(&ty);
                match ty {
                    Some(ty) => {
                        if let Some(val) = *value {
                            let val_ty = self.check(val.clone());
                            if val_ty.is_none() || val_ty.unwrap() != ty {
                                self.basic_err(
                                    format!("Expected value to be type {:?}", ty),
                                    val.span,
                                );
                                return None;
                            }
                        }
                        self.scopes.last_mut().unwrap().insert(name, ty);
                    }
                    None => self.basic_err("This variable has an incorrect type".into(), node.span),
                }
                None
            }
            ExprKind::Bool(_) => Some(TypeForm::Bool),
            ExprKind::Float(_) => Some(TypeForm::Float),
            ExprKind::String(_) => Some(TypeForm::String),
            ExprKind::Int(_) => Some(TypeForm::Int),
            ExprKind::Function(name, params, ret_type, stmts) => {
                let mut params_ty: HashMap<String, TypeForm> = HashMap::new();

                for (name, ty) in params.clone() {
                    let ty = self.get_type(&ty);
                    if let Some(ty) = ty {
                        params_ty.insert(name, ty);
                    } else {
                        self.basic_err(
                            "This function doesn't have a valid parameter types.".into(),
                            node.span.clone(),
                        )
                    }
                }
                let mut return_type: Option<TypeForm> = None;
                if let Some(ret) = ret_type {
                    if let Some(ret_ty) = self.get_type(&ret) {
                        return_type = Some(ret_ty)
                    } else {
                        self.basic_err(
                            "Function has an invalid return type in the function signature.".into(),
                            node.span.clone(),
                        )
                    }
                }
                let func_sig = FunctionLike {
                    parameters: params_ty.clone(),
                    return_type: Box::new(return_type.clone()),
                };

                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name, TypeForm::Function(func_sig));
                self.start_scope();
                for (name, ty) in params_ty
                    .into_iter()
                    .map(|(name, ty)| (name, ty))
                    .collect::<Vec<(String, TypeForm)>>()
                {
                    self.scopes.last_mut().unwrap().insert(name, ty);
                }
                for (index, node) in stmts.clone().iter().enumerate() {
                    match node.clone().inner {
                        ExprKind::Return(real_ty) => {
                            let mut success = true;
                            if let Some(ty_) = *real_ty {
                                let ty = self.check(ty_.clone());
                                if ty != return_type {
                                    success = false;
                                    self.basic_err(
                                        format!(
                                            "Expected type {:?} found type {:?}",
                                            return_type.unwrap_or(TypeForm::Void),
                                            ty.unwrap_or(TypeForm::Void)
                                        ),
                                        ty_.span,
                                    );
                                }
                            }
                            if index != stmts.len() - 1 && success {
                                let span =
                                    stmts[index + 1].span.start..stmts[stmts.len() - 1].span.end;
                                println!("{:?}", miette!(
                                    severity = miette::Severity::Warning,
                                    labels = vec![
                                        LabeledSpan::at(span, "Unreachable code".to_owned())
                                    ],
                                    help = "Just delete that code lol, the compiler will automatically ignore it..",
                                    " Warning: Unreachable code"
                                ).with_source_code(self.source.clone()));
                            }
                            break;
                        }
                        _ => {
                            self.check(node.clone());
                        }
                    }
                }

                self.end_scope();
                None
            }
            ExprKind::FunctionCall(f, args) => {
                for arg in args {
                    self.check(arg);
                }
                if let ExprKind::Ident(ident) = &f.as_ref().inner {
                    match self.get(ident.to_owned()) {
                        Some(v) => {
                            if let TypeForm::Function(f) = v {
                                return *f.return_type;
                            }
                            unreachable!()
                        }
                        None => return None,
                    }
                } else {
                    unreachable!()
                }
            }
            ExprKind::Ident(name) => return self.get(name),
            ExprKind::Binary(lhs, _, rhs) => {
                let lhs_ty = self.check(*lhs.clone());
                let rhs_ty = self.check(*rhs.clone());
                if lhs_ty != rhs_ty {
                    self.errors.push(
                        miette!(
                            labels = vec![
                                LabeledSpan::at(
                                    lhs.span,
                                    format!(
                                        "This is with type {:?}",
                                        lhs_ty.unwrap_or(TypeForm::Void)
                                    )
                                ),
                                LabeledSpan::at(
                                    rhs.span,
                                    format!(
                                        "And this is with type {:?}",
                                        rhs_ty.unwrap_or(TypeForm::Void)
                                    )
                                ),
                            ],
                            help = "Make the left, and right have the same types.".to_owned(),
                            "Please do operations with the same types"
                        )
                        .with_source_code(self.source.clone()),
                    );
                    return None;
                }
                return lhs_ty;
            }
            _ => return None,
        }
    }
    fn get_type(&self, name: &str) -> Option<TypeForm> {
        Some(match name {
            "int" => TypeForm::Int,
            "float" => TypeForm::Float,
            "bool" => TypeForm::Bool,
            "string" => TypeForm::String,
            _ => return None,
        })
    }

    fn get(&self, ident: String) -> Option<TypeForm> {
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
            miette!(labels = vec![LabeledSpan::at(span, message)], "Type Error")
                .with_source_code(self.source.clone()),
        )
    }
}
