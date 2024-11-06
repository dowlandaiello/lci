use std::env;

mod cmd;

const MAXIMUM_EVALUATION_CYCLES: usize = 1_000_000;

fn main() {
    if let Some(cmd) = env::args().nth(1) {
        if cmd.as_str() == "run" {
            cmd::exec_script(&env::args().nth(2).expect("missing script name"));
        }

        return;
    }

    cmd::repl()
}
