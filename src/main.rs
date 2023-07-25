use dynamik::analyzer;
use dynamik::typechecker;
use dynamik::parser;

use dynamik::Compiler;

use std::ops::Range;

use crate::parser::*;
use chumsky::{input::Stream, prelude::*};
use logos::Logos;
use miette::{miette, LabeledSpan};

fn main() {
    const SRC: &str = r#"
extern "C" let var printf(string)
let v1: int = 2;
if v1 == 2 {
    printf("v1 is two");
} else {
    printf("v1 is not two");
}
"#;

    let token_iter = LogosToken::lexer(SRC)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((SRC.len()..SRC.len()).into());
    match parser().parse(token_stream).into_result() {
        Ok(o) => {
            let mut analyzer = analyzer::Analyzer::new(o.clone(), SRC);
            if analyzer.analyze() {
                let mut checker = typechecker::TypeChecker::new(o.clone(), SRC);

                if checker.typecheck() {
                    let context = inkwell::context::Context::create();
                    let mut compiler = Compiler::new(&context, inkwell::OptimizationLevel::None);

                    compiler.process(&o).unwrap();
                    compiler.jit_run();
                };
            }
        }

        Err(errs) => {
            for err in errs {
                let span: Range<usize> = (*err.span()).into();
                let reason = err.reason().to_string();
                println!(
                    "{:?}",
                    miette!(
                        labels = vec![LabeledSpan::at(span, reason)],
                        "Parsing error"
                    )
                    .with_source_code(SRC)
                );
            }
        }
    };
}
