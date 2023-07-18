use ariadne::{Report, ReportKind, Label, Color, Source};
use chumsky::{prelude::*, input::Stream};
use dynamik::parser::*;
use logos::Logos;

fn main() {
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
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((SRC.len()..SRC.len()).into());
    match parser().parse(token_stream).into_result() {
        Ok(o) => {
            println!("{o:#?}");
        }
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
