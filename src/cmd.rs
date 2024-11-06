pub fn repl() {
    let input = io::stdin();
    let mut output = io::stdout();

    loop {
        let input_buff = read_eof!(input, output, "> ", String).expect("failed to read input");

        let tok_stream = parser::lex(&input_buff).expect("failed to lex");
        let mut expr: Expr = tok_stream
            .as_slice()
            .try_into()
            .expect("failed to construct AST");

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
