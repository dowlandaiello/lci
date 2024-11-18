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

pub fn t() -> Expr {
    "(\\a.(\\b.a))".try_into().unwrap()
}

pub fn f() -> Expr {
    "(\\a.(\\b.b))".try_into().unwrap()
}

pub fn cond() -> Expr {
    "(\\p.(\\a.(\\b.(p) (a) (b))))".try_into().unwrap()
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

    #[test]
    fn test_t() {
        t();
    }

    #[test]
    fn test_f() {
        f();
    }

    #[test]
    fn tets_cond() {
        cond();
    }
}
