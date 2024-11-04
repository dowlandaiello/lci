use super::parser::Expr;
use std::collections::BTreeMap;

const STARTING_ID: char = 'a';

pub trait Reducer {
    fn next(&mut self) -> Expr;
}

pub struct PureReducer {
    curr_step: Expr,
}

impl PureReducer {
    pub fn new(e: Expr) -> Self {
        Self { curr_step: e }
    }
}

impl Reducer for PureReducer {
    fn next(&mut self) -> Expr {
        self.curr_step = match &self.curr_step {
            expr @ Expr::Id(_) => expr.clone(),
            expr @ Expr::Abstraction { .. } => expr.clone(),
            expr @ Expr::Application { lhs, rhs } => match *lhs.clone() {
                Expr::Abstraction { bind_id, body } => body.replace_free(bind_id, *rhs.clone()),
                _ => expr.clone(),
            },
        };

        self.curr_step.clone()
    }
}

/// Stack-based beta reducer.
pub struct StackReducer {
    curr_step: Expr,
    scopes: BTreeMap<char, Expr>,
}

impl StackReducer {
    pub fn new(e: Expr) -> Self {
        Self {
            curr_step: e,
            scopes: Default::default(),
        }
    }
}

impl Reducer for StackReducer {
    fn next(&mut self) -> Expr {
        self.curr_step = match &self.curr_step {
            expr @ Expr::Id(c) => self.scopes.get(&c).unwrap_or(&expr).clone(),
            Expr::Abstraction { mut bind_id, body } => {
                let mut inner_body = *body.clone();

                // Alpha renaming must occur
                if self.scopes.contains_key(&bind_id) {
                    let new_bind_id = self
                        .scopes
                        .last_entry()
                        .map(|ent| *ent.key())
                        .unwrap_or(STARTING_ID);

                    inner_body = inner_body.rename_free(bind_id, new_bind_id);
                    bind_id = new_bind_id;
                }

                Expr::Abstraction {
                    bind_id,
                    body: Box::new(inner_body),
                }
            }
            expr @ Expr::Application { lhs, rhs } => match *lhs.clone() {
                Expr::Abstraction { bind_id, body } => {
                    self.scopes.insert(bind_id, *rhs.clone());

                    *body.clone()
                }
                _ => expr.clone(),
            },
        };

        self.curr_step.clone()
    }
}
