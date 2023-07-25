pub mod analyzer;
pub mod llvm;
pub mod parser;
pub mod typechecker;
pub mod cli;

pub use llvm::Compiler;

// this is just an example of how to create tests for the compiler
// I will let my student create more testcases
// you're welcome

#[cfg(test)]
mod compiler_tests {
    use super::*;
    use crate::parser::*;
    use chumsky::{input::Stream, prelude::*};
    use logos::Logos;

    #[test]
    fn test() {
        const SRC: &str = r#"
let calc() -> int {
    let v1: int = 1;
    let v2: int = 4;
    let v3: int = 10;
    let v4: int = 50;

    return (v1 + v2) * v3 / v4;
}

let float_test() -> float {
    let f1: float = 5.0;
    let f2: float = 2.0;

    return f2 / f1;
}
"#;

        type CalcFn = unsafe extern "C" fn() -> i64;
        type FloatFn = unsafe extern "C" fn() -> f64;

        let token_iter = LogosToken::lexer(SRC)
            .spanned()
            .map(|(tok, span)| match tok {
                Ok(tok) => (tok, span.into()),
                Err(()) => (LogosToken::Error, span.into()),
            });

        let token_stream = Stream::from_iter(token_iter)
            .spanned::<LogosToken, SimpleSpan>((SRC.len()..SRC.len()).into());

        let ast = parser().parse(token_stream).unwrap();

        let context = inkwell::context::Context::create();
        let mut compiler = Compiler::new(&context, inkwell::OptimizationLevel::None);

        compiler.process(&ast).unwrap();

        let calc_fn = compiler.get_jit_function::<CalcFn>("calc").unwrap();
        let float_fn = compiler.get_jit_function::<FloatFn>("float_test").unwrap();

        assert!(unsafe { calc_fn.call() } == 1);
        assert!(unsafe { float_fn.call() } == 0.4);
    }
}
