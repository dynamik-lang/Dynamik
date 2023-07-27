use std::process::Command;

use dynamik::analyzer;
use dynamik::cli;
use dynamik::parser;
use dynamik::typechecker;

use dynamik::Compiler;
use inkwell::OptimizationLevel;

use std::ops::Range;

use crate::parser::*;
use chumsky::{input::Stream, prelude::*};
use logos::Logos;
use miette::{miette, LabeledSpan};

fn main() {
    let matches = cli::get_matches();
    let opt_level = matches
        .get_one::<String>("opt level")
        .unwrap()
        .parse::<u32>()
        .expect("Expected a number");

    let opt_level = match opt_level {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,

        _ => {
            eprintln!("Invalid optimization level");
            std::process::exit(0);
        }
    };

    let compile;
    let file_path;

    match matches.subcommand() {
        Some(("compile", matches)) => {
            file_path = matches.get_one::<String>("file path").unwrap();
            compile = true;
        }

        Some(("run", matches)) => {
            file_path = matches.get_one::<String>("file path").unwrap();
            compile = false;
        }

        _ => unreachable!(),
    };

    let src = match std::fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("failed to read file: {e:?}");
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
            println!("{o:?}");
            let mut analyzer = analyzer::Analyzer::new(o.clone(), &src);
            if analyzer.analyze() {
                let mut checker = typechecker::TypeChecker::new(o.clone(), &src);

                if checker.typecheck() {
                    let context = inkwell::context::Context::create();
                    let mut compiler = Compiler::new(&context, opt_level);

                    compiler.process(&o).unwrap();
                    if compile {
                        let output_file = file_path.split_once('.').unwrap().0;
                        compiler.compile(&format!("{output_file}.o"), opt_level);
                        Command::new("gcc")
                            .arg(format!("{output_file}.o"))
                            .arg("-o")
                            .arg(output_file)
                            .spawn()
                            .expect("Failed to execute gcc");
                    } else {
                        compiler.jit_run();
                    }
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
                    .with_source_code(src.clone())
                );
            }
        }
    };
}
