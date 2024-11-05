use super::{parser::Expr, plan::Plan};
use std::collections::BTreeSet;

pub trait Reducer {
    fn step(p: Plan) -> Expr;
}

pub struct NaiveReducer;

impl Reducer for NaiveReducer {
    fn step(p: Plan) -> Expr {
        match p {
            Plan::Irreducible(e) => e.into(),
            Plan::Substitution {
                step,
                binding,
                argument,
            } => <Plan as Into<Expr>>::into(*step).replace_free(
                BTreeSet::default(),
                binding,
                <Plan as Into<Expr>>::into(*argument),
            ),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{
        super::plan::{NaivePlanner, Strategy},
        *,
    };

    #[test]
    fn test_pure_terminal_substitution() {
        let expr: Expr = "(a)".try_into().unwrap();
        let plan = NaivePlanner::translate(expr);

        assert_eq!(NaiveReducer::step(plan), Expr::Id('a'));
    }

    #[test]
    fn test_pure_substitution() {
        let expr: Expr = "(\\a.a)(a)".try_into().unwrap();
        let plan = NaivePlanner::translate(expr);

        assert_eq!(NaiveReducer::step(plan), Expr::Id('a'));
    }
}
