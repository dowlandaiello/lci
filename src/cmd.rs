use super::MAXIMUM_EVALUATION_CYCLES;
use lclib::{
    parser::{self, Expr},
    reducer::{NaiveReducer, Reducer},
    stdlib::combinators,
};
use std::{
    fs::OpenOptions,
    io::{self, Read, Write},
};

macro_rules! read_eof {
    ($in:ident, $out:ident, $prompt:expr, $t:ty) => {{
        print!("{}", $prompt);

        $out.flush().expect("failed to flush input");

        let mut input_buff = String::new();
        let n_bytes = $in
            .read_line(&mut input_buff)
            .expect("failed to read input");

        if n_bytes == 0 {
            println!("");

            break;
        }

        input_buff.trim().parse::<$t>()
    }};
}

fn preprocess(e: Expr) -> Expr {
    e.replace_keyword('Y', combinators::y())
        .replace_keyword('T', combinators::t())
        .replace_keyword('F', combinators::f())
        .replace_keyword('?', combinators::cond())
}

pub fn exec_script(fname: &str) {
    let mut f = OpenOptions::new().read(true).open(fname).unwrap();

    let mut buff = String::new();
    f.read_to_string(&mut buff)
        .expect("failed to read expression file");

    let tok_stream = parser::lex(&buff).expect("failed to lex");
    let mut expr: Expr = preprocess(
        tok_stream
            .as_slice()
            .try_into()
            .expect("failed to construct AST"),
    );

    for _ in 0..MAXIMUM_EVALUATION_CYCLES {
        let new_res = NaiveReducer::step(expr.clone());

        if new_res == expr {
            break;
        }

        expr = new_res;
    }

    println!("{}", expr);
}

pub fn repl() {
    let input = io::stdin();
    let mut output = io::stdout();

    loop {
        let input_buff = read_eof!(input, output, "> ", String).expect("failed to read input");

        let tok_stream = parser::lex(&input_buff).expect("failed to lex");
        let mut expr: Expr = preprocess(
            tok_stream
                .as_slice()
                .try_into()
                .expect("failed to construct AST"),
        );

        loop {
            let input_buff =
                read_eof!(input, output, "^D|1|n > ", usize).expect("failed to read input");

            let res = (0..input_buff).fold(expr.clone(), |acc, _| {
                let res = NaiveReducer::step(acc);
                println!("{}", res);

                NaiveReducer::step(res)
            });

            expr = res;
        }
    }
}
