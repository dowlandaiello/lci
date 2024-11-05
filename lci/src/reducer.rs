use super::parser::Expr;
use std::collections::BTreeSet;

pub trait Reducer {
    fn step(p: Expr) -> Expr;
}

pub struct NaiveReducer;

impl Reducer for NaiveReducer {
    fn step(p: Expr) -> Expr {
        match p.clone() {
            e @ Expr::Id(_) | e @ Expr::Abstraction { .. } => e,
            Expr::Application { lhs, rhs } => match *lhs {
                Expr::Id(_) => Expr::Application { lhs, rhs },
                // Need to continue reducing the left side to get an actual abstraction
                e @ Expr::Application { .. } => {
                    println!("here: {} {} {}", e, p, Self::step(e.clone()));
                    Expr::Application {
                        lhs: Box::new(Self::step(e)),
                        rhs,
                    }
                }
                Expr::Abstraction { bind_id, body } => {
                    body.replace_free(BTreeSet::default(), bind_id, *rhs.clone())
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pure_terminal_substitution() {
        let expr: Expr = "(a)".try_into().unwrap();

        assert_eq!(NaiveReducer::step(expr), Expr::Id('a'));
    }

    #[test]
    fn test_pure_substitution() {
        let expr: Expr = "(\\a.a)(a)".try_into().unwrap();

        assert_eq!(NaiveReducer::step(expr), Expr::Id('a'));
    }
}
