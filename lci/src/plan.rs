use super::parser::Expr;

pub trait Strategy {
    fn translate(e: Expr) -> Plan;
}

/// A terminal expression
#[derive(Debug, PartialEq, Clone)]
pub enum TerminalExpr {
    Id(char),
    Abstraction { bind_id: char, body: Box<Plan> },
}

impl From<TerminalExpr> for Expr {
    fn from(e: TerminalExpr) -> Self {
        match e {
            TerminalExpr::Id(c) => Self::Id(c),
            TerminalExpr::Abstraction { bind_id, body } => Self::Abstraction {
                bind_id,
                body: Box::new(<Plan as Into<Expr>>::into(*body)),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrreducibleExpr {
    Terminal(TerminalExpr),
    Application { lhs: char, rhs: Box<Plan> },
}

impl From<IrreducibleExpr> for Expr {
    fn from(e: IrreducibleExpr) -> Self {
        match e {
            IrreducibleExpr::Terminal(e) => e.into(),
            IrreducibleExpr::Application { lhs, rhs } => Expr::Application {
                lhs: Box::new(Expr::Id(lhs)),
                rhs: Box::new(<Plan as Into<Expr>>::into(*rhs)),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Plan {
    Irreducible(IrreducibleExpr),
    Substitution {
        step: Box<Plan>,
        binding: char,
        argument: Box<Plan>,
    },
}

impl Plan {
    pub fn is_irreducible(&self) -> bool {
        match self {
            Self::Irreducible(_) => true,
            _ => false,
        }
    }
}

impl From<Plan> for Expr {
    fn from(p: Plan) -> Self {
        match p {
            Plan::Irreducible(IrreducibleExpr::Terminal(e)) => {
                <TerminalExpr as Into<Self>>::into(e)
            }
            Plan::Irreducible(IrreducibleExpr::Application { lhs, rhs }) => Self::Application {
                lhs: Box::new(Expr::Id(lhs)),
                rhs: Box::new(<Plan as Into<Expr>>::into(*rhs.clone())),
            },
            Plan::Substitution {
                step,
                binding: bind_id,
                argument,
            } => Self::Application {
                lhs: Box::new(Self::Abstraction {
                    bind_id,
                    body: Box::new(<Plan as Into<Self>>::into(*step.clone())),
                }),
                rhs: Box::new(<Plan as Into<Self>>::into(*argument.clone())),
            },
        }
    }
}

pub struct NaivePlanner;

impl Strategy for NaivePlanner {
    fn translate(e: Expr) -> Plan {
        match e {
            Expr::Id(c) => Plan::Irreducible(IrreducibleExpr::Terminal(TerminalExpr::Id(c))),
            Expr::Abstraction {
                bind_id: c,
                body: b,
            } => Plan::Irreducible(IrreducibleExpr::Terminal(TerminalExpr::Abstraction {
                bind_id: c,
                body: Box::new(Self::translate(*b.clone())),
            })),
            Expr::Application { lhs, rhs } => match *lhs.clone() {
                Expr::Abstraction { bind_id, body } => Plan::Substitution {
                    step: Box::new(Self::translate(*body.clone())),
                    binding: bind_id,
                    argument: Box::new(Self::translate(*rhs.clone())),
                },
                Expr::Id(c) => Plan::Irreducible(IrreducibleExpr::Application {
                    lhs: c,
                    rhs: Box::new(Self::translate(*rhs.clone())),
                }),
                e @ Expr::Application { .. } => Self::translate(e),
            },
        }
    }
}
