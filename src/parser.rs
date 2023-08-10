use std::{
    fmt::{self},
    ops::Range,
};

use chumsky::{input::ValueInput, prelude::*};
use logos::Logos;

pub type Span = SimpleSpan<usize>;
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \r\f\n\t]+")]
pub enum LogosToken<'a> {
    #[regex(r#"\d+"#, priority = 2)]
    Int(&'a str),
    #[regex(r#"((\d+(\.\d+)?)|((\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float(&'a str),
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Slash,
    #[token("->")]
    Arrow,
    #[token("^")]
    Pow,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("...")]
    ThreeDots,
    #[token("::")]
    FourDots,
    #[token("=")]
    Eq,
    #[token("!")]
    Bang,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("==")]
    Eqq,
    #[token("!=")]
    Neq,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("_")]
    Under,
    #[token("<")]
    LAngle,
    #[token(">")]
    RAngle,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LSquare,
    #[token("]")]
    RSquare,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semi,
    // Constructs
    #[regex(r#""([^"\\]|\\[\s\S])*""#)]
    String(&'a str),
    // #[regex(r#"//[^\n]*\n"#)]
    // LineComment,
    #[regex(r#"[A-Za-z_]([A-Za-z]|_|\d)*"#)]
    Ident(&'a str),
    #[token("let")]
    KwLet,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("return")]
    KwReturn,
    #[token("extern")]
    KwExtern,
    #[token("while")]
    KwWhile,
    #[token("mod")]
    KwMod,
    Error,
}
impl<'a> fmt::Display for LogosToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogosToken::And => write!(f, "&&"),
            LogosToken::Or => write!(f, "||"),
            LogosToken::True => write!(f, "true"),
            LogosToken::False => write!(f, "false"),
            LogosToken::FourDots => write!(f, "::"),
            LogosToken::Bang => write!(f, "!"),
            LogosToken::KwWhile => write!(f, "while"),
            LogosToken::String(_) => write!(f, "string"),
            LogosToken::Ident(_) => write!(f, "identifier"),
            LogosToken::Int(_) => write!(f, "integer"),
            LogosToken::Float(_) => write!(f, "float"),
            LogosToken::Eq => write!(f, "="),
            LogosToken::Eqq => write!(f, "=="),
            LogosToken::Arrow => write!(f, "->"),
            LogosToken::KwLet => write!(f, "let"),
            LogosToken::KwElse => write!(f, "else"),
            LogosToken::KwIf => write!(f, "if"),
            LogosToken::KwMod => write!(f, "mod"),
            LogosToken::ThreeDots => write!(f, "..."),
            LogosToken::Semi => write!(f, ";"),
            LogosToken::LAngle => write!(f, "<"),
            LogosToken::RAngle => write!(f, ">"),
            LogosToken::LParen => write!(f, "("),
            LogosToken::Comma => write!(f, ","),
            LogosToken::RParen => write!(f, ")"),
            LogosToken::Error => write!(f, "unknown character"),
            LogosToken::Plus => write!(f, "+"),
            LogosToken::KwExtern => write!(f, "extern"),
            LogosToken::Minus => write!(f, "-"),
            LogosToken::Times => write!(f, "*"),
            LogosToken::Slash => write!(f, "/"),
            LogosToken::Pow => write!(f, "^"),
            LogosToken::Colon => write!(f, ":"),
            LogosToken::Neq => write!(f, "!="),
            LogosToken::Leq => write!(f, "<="),
            LogosToken::Geq => write!(f, ">="),
            LogosToken::Under => write!(f, "_"),
            LogosToken::LSquare => write!(f, "["),
            LogosToken::RSquare => write!(f, "]"),
            LogosToken::LBrace => write!(f, "{{"),
            LogosToken::RBrace => write!(f, "}}"),
            LogosToken::KwReturn => write!(f, "return"),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Range<usize>,
    pub inner: ExprKind,
}
impl Expr {
    pub fn new(span: Range<usize>, inner: ExprKind) -> Self {
        Expr { inner, span }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ident(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Let(String, String, Box<Option<Expr>>),
    FunctionCall(Option<Vec<String>>, String, Vec<Expr>),
    Function(String, Vec<(String, String)>, Option<String>, Vec<Expr>),
    If(Box<Expr>, Vec<Expr>, Option<Vec<Expr>>), // IF <condition> <block> (else <block>)?
    ExternFunction(String, Vec<String>, Option<String>, bool),
    Return(Box<Option<Expr>>),
    While(Box<Expr>, Vec<Expr>),
    Mod(String, Vec<Expr>),
    Assignment(String, Box<Expr>),
}
#[derive(Debug, Clone)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    NotEq,
    Eq,
    Or,
    And,
}
impl BinaryOp {
    pub fn is_comp(self) -> bool {
        match self {
            Self::Eq
            | Self::NotEq
            | Self::Greater
            | Self::GreaterEq
            | Self::Less
            | Self::LessEq
            | Self::And
            | Self::Or => true,
            _ => false,
        }
    }
}

pub fn parser<'a, I>() -> impl Parser<'a, I, Vec<Expr>, extra::Err<Rich<'a, LogosToken<'a>>>>
where
    I: ValueInput<'a, Token = LogosToken<'a>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let inline = recursive(|e| {
            let ident = select! {
                LogosToken::Ident(name) => name.to_string()
            };

            let val = select! {
                LogosToken::Int(i) => ExprKind::Int(i.parse().unwrap()),
                LogosToken::Float(f) => ExprKind::Float(f.parse().unwrap()),
                LogosToken::String(s) =>{
                  let mut result = String::new();
                  let mut chars = s.chars().peekable();

                  while let Some(ch) = chars.next() {
                    if ch == '\\' {
                      match chars.next() {
                        Some('n') => result.push('\n'),
                        Some('t') => result.push('\t'),
                        Some('r') => result.push('\r'),
                        Some('x') => {
                            let mut hex = String::new();

                            for digit in chars.by_ref() {
                              if digit.is_ascii_hexdigit() {
                                hex.push(digit);
                              } else {
                                break;
                              }
                            }

                            if let Ok(value) = u32::from_str_radix(&hex, 16) {
                              result.push(char::from_u32(value).unwrap());
                            }
                        },
                        // Add more cases

                        _ => result.push(ch), // Invalid escape, just keep char
                      }
                    } else {
                      result.push(ch);
                    }
                  };
                    ExprKind::String(result)
                },
                LogosToken::Ident(s) => ExprKind::Ident(s.to_string()),
                LogosToken::True => ExprKind::Bool(true),
                LogosToken::False => ExprKind::Bool(false),
            }
            .map_with_span(|a, span: Span| Expr::new(span.into(), a))
            .or(e
                .clone()
                .delimited_by(just(LogosToken::LParen), just(LogosToken::RParen)));
            let op = just(LogosToken::Times)
                .to(BinaryOp::Mul)
                .or(just(LogosToken::Slash).to(BinaryOp::Div));
            let items = e
                .clone()
                .separated_by(just(LogosToken::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();
            let four_dots = ident
                .separated_by(just(LogosToken::FourDots))
                .collect::<Vec<_>>()
                .or_not()
                .then(ident)
                .map(|(module, name)| (module, name));
            let call = four_dots
                .clone()
                .then(items.delimited_by(just(LogosToken::LParen), just(LogosToken::RParen)))
                .map_with_span(|((module, name), args), span: Span| {
                    Expr::new(span.into(), ExprKind::FunctionCall(module, name, args))
                });
            let product = val.clone().foldl(
                op.then(val)
                    .map_with_span(|a, span: Span| (a, span))
                    .repeated(),
                |lhs, ((op, rhs), span)| {
                    Expr::new(
                        span.into(),
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                },
            );
            let op = just(LogosToken::Plus)
                .to(BinaryOp::Add)
                .or(just(LogosToken::Minus).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |lhs, (op, rhs)| {
                    Expr::new(
                        lhs.span.start..rhs.span.end,
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                });
            let op = choice((
                just(LogosToken::Eqq).to(BinaryOp::Eq),
                just(LogosToken::Neq).to(BinaryOp::NotEq),
                just(LogosToken::LAngle).to(BinaryOp::Less),
                just(LogosToken::RAngle).to(BinaryOp::Greater),
                just(LogosToken::Geq).to(BinaryOp::GreaterEq),
                just(LogosToken::Leq).to(BinaryOp::LessEq),
            ));
            let comp = sum
                .clone()
                .foldl(op.then(sum).repeated(), |lhs, (op, rhs)| {
                    Expr::new(
                        lhs.span.start..rhs.span.end,
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                });
            let op = choice((
                just(LogosToken::Or).to(BinaryOp::Or),
                just(LogosToken::And).to(BinaryOp::And),
            ));
            let expr_ = comp
                .clone()
                .foldl(op.then(comp).repeated(), |lhs, (op, rhs)| {
                    Expr::new(
                        lhs.span.start..rhs.span.end,
                        ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
                    )
                });
            call.or(expr_.clone())
                .clone()
                .or(expr_.delimited_by(just(LogosToken::LParen), just(LogosToken::RParen)))
        });
        let ident = select! {
            LogosToken::Ident(name) => name.to_string()
        };

        let mod_expr = just(LogosToken::KwMod)
            .ignore_then(ident)
            .then(
                expr.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(LogosToken::LBrace), just(LogosToken::RBrace)),
            )
            .map_with_span(|(name, body), span: Span| {
                Expr::new(span.into(), ExprKind::Mod(name, body))
            });

        let let_expr = just(LogosToken::KwLet)
            .ignore_then(ident)
            .then_ignore(just(LogosToken::Colon))
            .then(ident)
            .then(just(LogosToken::Eq).ignore_then(inline.clone()).or_not())
            .map_with_span(|a, span: Span| (a, span))
            .map(|(((ident, ty), rhs), span)| {
                Expr::new(
                    span.into(),
                    ExprKind::Let(ident.to_string(), ty.to_string(), Box::new(rhs)),
                )
            });
        let function = just(LogosToken::KwLet)
            .ignore_then(ident)
            .then(
                ident
                    .then_ignore(just(LogosToken::Colon))
                    .then(ident)
                    .separated_by(just(LogosToken::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(LogosToken::LParen), just(LogosToken::RParen))
                    .then(just(LogosToken::Arrow).ignore_then(ident).or_not()),
            )
            .then_ignore(just(LogosToken::LBrace))
            .then(expr.clone().repeated().collect::<Vec<_>>())
            .then_ignore(just(LogosToken::RBrace))
            .map_with_span(|((name, (params, return_type)), stmts), span: Span| {
                Expr::new(
                    span.into(),
                    ExprKind::Function(name.to_string(), params, return_type, stmts),
                )
            });
        let return_expr = just(LogosToken::KwReturn)
            .ignore_then(inline.clone().or_not())
            .map_with_span(|e, span: Span| Expr::new(span.into(), ExprKind::Return(Box::new(e))));
        let if_expr = just(LogosToken::KwIf)
            .ignore_then(inline.clone())
            .then(
                expr.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(LogosToken::LBrace), just(LogosToken::RBrace)),
            )
            .then(
                just(LogosToken::KwElse)
                    .ignore_then(
                        expr.clone()
                            .repeated()
                            .collect::<Vec<_>>()
                            .delimited_by(just(LogosToken::LBrace), just(LogosToken::RBrace)),
                    )
                    .or_not(),
            )
            .map_with_span(|((condition, body), else_), span: Span| {
                Expr::new(span.into(), ExprKind::If(Box::new(condition), body, else_))
            });
        let extern_fn = just(LogosToken::KwExtern)
            .ignore_then(just(LogosToken::String("\"C\"")))
            .ignore_then(just(LogosToken::KwLet))
            .ignore_then(ident)
            .then_ignore(just(LogosToken::LParen))
            .then(
                ident
                    .clone()
                    .separated_by(just(LogosToken::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .then(
                        just(LogosToken::ThreeDots)
                            .or_not()
                            .map(|v| return v.is_some()),
                    ),
            )
            .then_ignore(just(LogosToken::RParen))
            .then(just(LogosToken::Arrow).ignore_then(ident).or_not())
            .map_with_span(|((name, (param_types, is_var)), return_type), span: Span| {
                Expr::new(
                    span.into(),
                    ExprKind::ExternFunction(name.to_owned(), param_types, return_type, is_var),
                )
            });
        let while_loop = just(LogosToken::KwWhile)
            .ignore_then(inline.clone())
            .then(
                expr.repeated()
                    .collect::<Vec<_>>()
                    .delimited_by(just(LogosToken::LBrace), just(LogosToken::RBrace)),
            )
            .map_with_span(|(condition, body), span: Span| {
                Expr::new(span.into(), ExprKind::While(Box::new(condition), body))
            });
        let assignment = ident
            .then_ignore(just(LogosToken::Eq))
            .then(inline.clone())
            .map_with_span(|(name, expr), span: Span| {
                Expr::new(
                    span.into(),
                    ExprKind::Assignment(name.to_string(), Box::new(expr)),
                )
            });
        function
            .or(extern_fn)
            .or(mod_expr)
            .or(while_loop)
            .or(return_expr)
            .or(assignment)
            .or(let_expr)
            .or(if_expr)
            .or(inline)
            .then_ignore(just(LogosToken::Semi).or_not())
    })
    .repeated()
    .collect::<Vec<Expr>>()
}

#[test]
fn b() {
    let src = "hamza::call()";
    let token_iter = LogosToken::lexer(&src)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((src.len()..src.len()).into());

    let p = parser().parse(token_stream);
    println!("{:?}", p.output())
}
