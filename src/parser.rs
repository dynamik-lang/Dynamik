use std::fmt::{self, write};

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::{Lexer, Logos};

pub type Span = SimpleSpan<usize>;
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \r\f\n\t]+")]
pub enum LogosToken<'a> {
    #[regex(r#"\d+"#, priority = 2)]
    Int(&'a str),
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float(&'a str),
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
    #[regex(r#"[A-Za-z]([A-Za-z]|_|\d)*"#)]
    Ident(&'a str),
    #[token("let")]
    KwLet,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("return")]
    KwReturn,
    Error,
}
impl<'a> fmt::Display for LogosToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogosToken::And => write!(f, "&&"),
            LogosToken::Or => write!(f, "||"),
            LogosToken::Bang => write!(f, "!"),
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
            LogosToken::Semi => write!(f, ";"),
            LogosToken::LAngle => write!(f, "<"),
            LogosToken::RAngle => write!(f, ">"),
            LogosToken::LParen => write!(f, "("),
            LogosToken::Comma => write!(f, ","),
            LogosToken::RParen => write!(f, ")"),
            LogosToken::Error => write!(f, "unknown character"),
            LogosToken::Plus => write!(f, "+"),
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
#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Float(f64),
    String(String),
    Ident(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Let(String, String, Box<Option<Expr>>),
    FunctionCall(Box<Expr>, Vec<Expr>),
    Function(String, Vec<(String, String)>, Option<String>, Vec<Expr>),
    Return(Box<Option<Expr>>),
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
fn parser<'a, I>() -> impl Parser<'a, I, Vec<Expr>, extra::Err<Rich<'a, LogosToken<'a>>>>
where
    I: ValueInput<'a, Token = LogosToken<'a>, Span = SimpleSpan>,
{
   
    recursive(|expr| {
        let inline = recursive(|e| {
            let val = select! {
                LogosToken::Int(i) => Expr::Int(i.parse().unwrap()),
                LogosToken::Float(f) => Expr::Float(f.parse().unwrap()),
                LogosToken::String(s) => Expr::String(s.to_string()),
                LogosToken::Ident(s) => Expr::Ident(s.to_string())
            };
            let op = just(LogosToken::Times)
                .to(BinaryOp::Mul)
                .or(just(LogosToken::Slash).to(BinaryOp::Div));
            let items = e
                .clone()
                .separated_by(just(LogosToken::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();
            let call = val.clone().foldl(
                items
                    .delimited_by(just(LogosToken::LParen), just(LogosToken::RParen))
                    .repeated(),
                |f, args| Expr::FunctionCall(Box::new(f), args),
            );
            let product = call
                .clone()
                .foldl(op.then(val).repeated(), |lhs, (op, rhs)| {
                    Expr::Binary(Box::new(lhs), op, Box::new(rhs))
                });
            let op = just(LogosToken::Plus)
                .to(BinaryOp::Add)
                .or(just(LogosToken::Minus).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |lhs, (op, rhs)| {
                    Expr::Binary(Box::new(lhs), op, Box::new(rhs))
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
                    Expr::Binary(Box::new(lhs), op, Box::new(rhs))
                });
            let op = choice((
                just(LogosToken::Or).to(BinaryOp::Or),
                just(LogosToken::And).to(BinaryOp::And),
            ));
            let expr_ = comp
                .clone()
                .foldl(op.then(comp).repeated(), |lhs, (op, rhs)| {
                    Expr::Binary(Box::new(lhs), op, Box::new(rhs))
                });

            expr_
        });
        let ident = select! {
            LogosToken::Ident(name) => name.to_string()
        };
        let let_expr = just(LogosToken::KwLet)
            .ignore_then(ident)
            .then_ignore(just(LogosToken::Colon))
            .then(ident)
            .then(just(LogosToken::Eq).ignore_then(inline.clone()).or_not())
            .map(|((ident, ty), rhs)| Expr::Let(ident.to_string(), ty.to_string(), Box::new(rhs)));
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
                    .then(just(LogosToken::Arrow).ignore_then(ident).or_not())
            )
            .then_ignore(just(LogosToken::LBrace))
            .then(
                expr
                    .separated_by(just(LogosToken::Semi).or_not())
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(LogosToken::RBrace))
            .map(|((name, (params, return_type)), stmts)| {
                Expr::Function(name.to_string(), params, return_type , stmts)
            });
        let return_expr = just(LogosToken::KwReturn).ignore_then(inline.clone().or_not()).map(|e| {
            Expr::Return(Box::new(e))
        });
        function.or(return_expr).or(let_expr).or(inline)
    })
    .separated_by(just(LogosToken::Semi).or_not())
    .collect::<Vec<Expr>>()
}


#[test]
fn test() {
    const SRC: &str = r#"
let hamza(b: i64) -> f64 {
    let 
        a: i64 = 9
    print(
        a,
        "Hello World"
    )
    return 
        to_f64(a)
}
"#;
    let token_iter = LogosToken::lexer(SRC)
        .spanned()
        .map(|(tok, span)| match tok {
            // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
            // to work with
            Ok(tok) => return (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    // Turn the token iterator into a stream that chumsky can use for things like backtracking
    let token_stream = Stream::from_iter(token_iter)
        // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
        // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
        .spanned::<LogosToken, SimpleSpan>((SRC.len()..SRC.len()).into());
    match parser().parse(token_stream).into_result() {
        // If parsing was successful, attempt to evaluate the s-expression
        Ok(o) => {
            println!("{:#?}", o)
        }
        // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
        // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
        // with Rust's built-in `Display` trait, but it's a little crude
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, (), err.span().start)
                    // .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(SRC))
                    .unwrap();
            }
        }
    };
}
