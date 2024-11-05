use lclib::{
    parser::Expr,
    plan::{NaivePlanner, Plan, Strategy},
    reducer::{NaiveReducer, Reducer},
};
use std::io::{self, Write};

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

fn main() {
    let input = io::stdin();
    let mut output = io::stdout();

    loop {
        let input_buff = read_eof!(input, output, "> ", String).expect("failed to read input");

        let tok_stream = parser::lex(&input_buff).expect("failed to lex");
        let expr: Expr = tok_stream
            .as_slice()
            .try_into()
            .expect("failed to construct AST");

        let mut plan: Plan = NaivePlanner::translate(expr.clone());

        println!("{:?} {:?}", expr, plan);

        loop {
            let input_buff =
                read_eof!(input, output, "^D|1|n > ", usize).expect("failed to read input");

            let res = (0..input_buff).fold(plan.clone(), |acc, _| {
                let res = NaiveReducer::step(acc);
                println!("{}", res);

                NaivePlanner::translate(res)
            });

            if res.is_irreducible() {
                break;
            }

            plan = res;
        }
    }
}
