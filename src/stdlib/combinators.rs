use super::super::parser::Expr;

pub fn y() -> Expr {
    "(\\f.(\\x.(f)((x)(x)))(\\x.(f)((x)(x))))"
        .try_into()
        .unwrap()
}

pub fn id() -> Expr {
    "(\\x.x)".try_into().unwrap()
}

pub fn constant() -> Expr {
    "(\\x.(\\y.x))".try_into().unwrap()
}

pub fn substitution() -> Expr {
    "(\\x.(\\y.(\\z.((x)(z))((y)(z)))))".try_into().unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_y() {
        y();
    }

    #[test]
    fn test_id() {
        id();
    }

    #[test]
    fn test_constant() {
        constant();
    }

    #[test]
    fn test_substitution() {
        substitution();
    }
}
