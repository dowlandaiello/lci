use std::{backtrace::Backtrace, char, error::Error as StdError, fmt};

const WELLFORMED_PARENTHESIZED_TERM: [Token; 3] =
    [Token::LeftParen, Token::Id('*'), Token::RightParen];

const WELLFORMED_ABSTRACTION_HEADER: [Token; 4] =
    [Token::LeftParen, Token::Lambda, Token::Id('*'), Token::Dot];

pub type TokenBuff = Vec<Span<Token>>;

pub type TokenStream = [Span<Token>];

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnrecognizedSymbol(char),
    UnrecognizedToken(Token),
    UnbalancedParen,
    EmptyExpression,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::UnrecognizedSymbol(c) => write!(f, "unrecognized symbol: {}", c),
            Self::UnrecognizedToken(tok) => write!(f, "unrecognized token: {:?}", tok),
            Self::UnbalancedParen => write!(f, "unbalanced parenthesis"),
            Self::EmptyExpression => write!(f, "empty expression"),
        }
    }
}

impl StdError for Error {}

#[derive(Debug)]
pub struct Span<T: fmt::Debug + Clone> {
    pos: usize,
    content: T,
    backtrace: Backtrace,
}

impl<T: fmt::Debug + Clone> Clone for Span<T> {
    fn clone(&self) -> Self {
        Self::new(self.pos, self.content.clone())
    }
}

impl<T: fmt::Debug + Clone> Span<T> {
    pub fn new(pos: usize, content: T) -> Self {
        Self {
            pos,
            content,
            backtrace: Backtrace::capture(),
        }
    }
}

impl<T: fmt::Debug + PartialEq + Clone> Span<T> {
    pub fn contents_eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl<T: fmt::Debug + PartialEq + Clone> PartialEq for Span<T> {
    fn eq(&self, other: &Self) -> bool {
        self.content == other.content
    }
}

impl fmt::Display for Span<Error> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "err @ {}: {}{}", self.pos, self.content, self.backtrace)
    }
}

impl StdError for Span<Error> {}

impl Span<Token> {
    fn as_err_unrecognized_token(&self) -> Span<Error> {
        Span::new(self.pos, Error::UnrecognizedToken(self.content.clone()))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Id(char),
    Abstraction { bind_id: char, body: Box<Expr> },
    Application { lhs: Box<Expr>, rhs: Box<Expr> },
}

impl Expr {
    fn insert_cdr(self, e: Expr) -> Self {
        Self::Application {
            lhs: Box::new(self),
            rhs: Box::new(e),
        }
    }

    /* Renames all free variables in this scope to the specified value
    pub fn rename_free(&self, from: char, to: char) -> Self {
        match self {
            expr @ Self::Id(c) => {
                if *c == from {
                    Expr::Id(to)
                } else {
                    expr.clone()
                }
            }
            Self::Application { lhs, rhs } => Self::Application {
                lhs: Box::new(lhs.rename_free(from, to)),
                rhs: Box::new(rhs.rename_free(from, to)),
            },
            Self::Abstraction { bind_id, body } => {
                if *bind_id == from {
                    Self::Abstraction {
                        bind_id: *bind_id,
                        body: body.clone(),
                    }
                } else {
                    Self::Abstraction {
                        bind_id: *bind_id,
                        body: Box::new(body.rename_free(from, to)),
                    }
                }
            }
        }
    }*/

    /// Replaces all free variables in this scope with the specified value
    pub fn replace_free(&self, from: char, to: Expr) -> Self {
        match self.clone() {
            expr @ Self::Id(c) => {
                if c == from {
                    to
                } else {
                    expr.clone()
                }
            }
            Self::Application { lhs, rhs } => Self::Application {
                lhs: Box::new(lhs.replace_free(from, to.clone())),
                rhs: Box::new(rhs.replace_free(from, to)),
            },
            Self::Abstraction { bind_id, body } => {
                // Must do alpha renaming, because the free variables are shadowed
                // they belong to this new inner scope
                Self::Abstraction {
                    bind_id,
                    body: Box::new(body.replace_free(from, to)),
                }
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id(x) => write!(f, "({})", x),
            Self::Abstraction { bind_id, body } => write!(f, "(\\{}.{})", bind_id, body),
            Self::Application { lhs, rhs } => write!(f, "({}{})", lhs, rhs),
        }
    }
}

/// Pops a parenthesized expression from the token stream, leaving the rest of the token stream
/// intact.
pub fn pop_paren_expr(tok_stream: &TokenStream) -> Result<(TokenBuff, TokenBuff), Span<Error>> {
    let mut buff: TokenBuff = tok_stream.into();
    let mut expr = TokenBuff::new();
    let mut left_parens = Vec::new();

    while (!left_parens.is_empty() || expr.is_empty()) && !buff.is_empty() {
        let token = buff.remove(0);

        match token {
            Span {
                pos: _,
                content: Token::LeftParen,
                backtrace: _,
            } => {
                left_parens.push(());
            }
            Span {
                pos: _,
                content: Token::RightParen,
                backtrace: _,
            } => {
                left_parens.pop();
            }
            Span {
                pos: _,
                content: _,
                backtrace: _,
            } => {}
        }

        expr.push(token);
    }

    if !left_parens.is_empty() {
        return Err(Span::new(buff[buff.len() - 1].pos, Error::UnbalancedParen));
    }

    Ok((expr, buff))
}

/// Groups curried function application arguments by parenthesis.
pub fn to_curried(tok_stream: &TokenStream) -> Result<Vec<TokenBuff>, Span<Error>> {
    let mut buff: TokenBuff = tok_stream.into();
    let mut terms = Vec::new();

    while !buff.is_empty() {
        let (popped, remaining) = pop_paren_expr(&buff)?;

        terms.push(popped);
        buff = remaining;
    }

    Ok(terms)
}

impl TryFrom<&str> for Expr {
    type Error = Span<Error>;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        lex(s).and_then(|tokens| tokens.as_slice().try_into())
    }
}

impl TryFrom<&TokenStream> for Expr {
    type Error = Span<Error>;

    fn try_from(tok_stream: &TokenStream) -> Result<Self, Self::Error> {
        let mut applicands = to_curried(tok_stream)?;

        // Single term
        if applicands.len() == 1 {
            let mut term = applicands
                .pop()
                .ok_or(Span::new(0, Error::EmptyExpression))?;

            // Free term
            if term.len() == 1 {
                let tok = term.remove(0);

                match tok {
                    Span {
                        pos: _,
                        content: Token::Id(c),
                        backtrace: _,
                    } => {
                        return Ok(Expr::Id(c));
                    }
                    Span {
                        pos,
                        content,
                        backtrace: _,
                    } => {
                        return Err(Span::new(pos, Error::UnrecognizedToken(content)));
                    }
                }
            }

            // Parenthesized smoething
            if term.len() > 3
                && term[0].content == Token::LeftParen
                && term[term.len() - 1].content == Token::RightParen
                && term[1].content != Token::Lambda
            {
                let content = &term[1..term.len() - 1];

                return content.try_into();
            }

            // Parenthesized term
            if term.len() == 3 {
                if let Some((unexpected_token, _)) = term
                    .iter()
                    .take(WELLFORMED_PARENTHESIZED_TERM.len())
                    .zip(WELLFORMED_PARENTHESIZED_TERM.iter())
                    .find(|(a, b)| !a.content.is_like(b))
                {
                    return Err(unexpected_token.as_err_unrecognized_token());
                }

                let parenthesized_term = &term[1];

                match parenthesized_term {
                    Span {
                        pos: _,
                        content: Token::Id(c),
                        backtrace: _,
                    } => {
                        return Ok(Expr::Id(*c));
                    }
                    unexpected_tok => {
                        return Err(unexpected_tok.as_err_unrecognized_token());
                    }
                }
            }

            // Lambda abstraction must begin with (\x. and end with a )
            if term[term.len() - 1].content != Token::RightParen {
                return Err(term[term.len() - 1].as_err_unrecognized_token());
            }

            if let Some((unexpected_token, _)) = term
                .iter()
                .take(WELLFORMED_ABSTRACTION_HEADER.len())
                .zip(WELLFORMED_ABSTRACTION_HEADER.iter())
                .find(|(a, b)| !a.content.is_like(b))
            {
                return Err(unexpected_token.as_err_unrecognized_token());
            }

            let body_tokens = &term[WELLFORMED_ABSTRACTION_HEADER.len()..term.len() - 1];
            let body: Expr = body_tokens.try_into()?;

            if let Span {
                pos: _,
                content: Token::Id(c),
                backtrace: _,
            } = term[2]
            {
                return Ok(Expr::Abstraction {
                    bind_id: c,
                    body: Box::new(body),
                });
            }
        }

        // Continuously fold lambda terms into application from left to right
        let init = <&TokenStream as TryInto<Expr>>::try_into(applicands.remove(0).as_slice())?;

        applicands.into_iter().try_fold(init, |expr, arg| {
            Ok(expr.insert_cdr(<&TokenStream as TryInto<Expr>>::try_into(arg.as_slice())?))
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Id(char),
    Dot,
    Lambda,
    LeftParen,
    RightParen,
}

impl Token {
    fn is_like(&self, other: &Token) -> bool {
        self == other || matches!((self, other), (Self::Id(_), Self::Id(_)))
    }
}

impl TryFrom<char> for Token {
    type Error = Error;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '\\' => Ok(Self::Lambda),
            '.' => Ok(Self::Dot),
            '(' => Ok(Self::LeftParen),
            ')' => Ok(Self::RightParen),
            x => {
                if x.is_ascii_lowercase() {
                    Ok(Self::Id(c))
                } else {
                    Err(Error::UnrecognizedSymbol(c))
                }
            }
        }
    }
}

pub fn lex(input: &str) -> Result<TokenBuff, Span<Error>> {
    input
        .chars()
        .enumerate()
        .filter(|(_, c)| !c.is_whitespace())
        .map(|(i, c)| {
            c.try_into()
                .map_err(|e| Span::new(i, e))
                .map(|token| Span::new(i, token))
        })
        .collect::<Result<_, _>>()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_gibberish() {
        let s = ",.!";

        let tokens = lex(s);

        assert!(tokens.is_err());
        assert_eq!(tokens.unwrap_err().content, Error::UnrecognizedSymbol(','));
    }

    #[test]
    fn test_whitespace_empty() {
        let s = "";

        assert_eq!(lex(s).unwrap().len(), 0);
    }

    #[test]
    fn test_lex_identifier() {
        let s = "a";

        let tokens = lex(s).unwrap();

        assert!(tokens[0].contents_eq(&Span::new(0, Token::Id('a'))));
    }

    #[test]
    fn test_lex_abstraction() {
        let s = "\\a.a";

        let tokens = lex(s).unwrap();

        assert!(tokens
            .into_iter()
            .zip(
                vec![
                    Span::new(0, Token::Lambda,),
                    Span::new(1, Token::Id('a'),),
                    Span::new(2, Token::Dot,),
                    Span::new(3, Token::Id('a'),)
                ]
                .into_iter()
            )
            .all(|(a, b)| a.contents_eq(&b)));
    }

    #[test]
    fn test_curried_vars() {
        <&str as TryInto<Expr>>::try_into("(a)(b)(c)").unwrap();
    }

    #[test]
    fn test_parse_succ() {
        <&str as TryInto<Expr>>::try_into("(\\n.(\\f.(\\x.(f)((n)(f)(x)))))").unwrap();
    }

    #[test]
    fn test_parse_succ_e2e() {
        println!(
            "{}",
            <&str as TryInto<Expr>>::try_into("(\\n.(\\f.(\\x.(f)((n)(f)(x)))))")
                .unwrap()
                .to_string()
                .as_str()
        );
        assert_eq!(
            <&str as TryInto<Expr>>::try_into(
                <&str as TryInto<Expr>>::try_into("(\\n.(\\f.(\\x.(f)((n)(f)(x)))))")
                    .unwrap()
                    .to_string()
                    .as_str()
            )
            .unwrap(),
            <&str as TryInto<Expr>>::try_into("(\\n.(\\f.(\\x.(f)((n)(f)(x)))))").unwrap()
        );
    }

    #[test]
    fn test_lex_application() {
        let s = "(\\a.a)a";

        let tokens = lex(s).unwrap();

        assert!(tokens
            .into_iter()
            .zip(
                vec![
                    Span::new(0, Token::LeftParen,),
                    Span::new(1, Token::Lambda),
                    Span::new(2, Token::Id('a'),),
                    Span::new(3, Token::Dot,),
                    Span::new(4, Token::Id('a')),
                    Span::new(5, Token::RightParen,),
                    Span::new(6, Token::Id('a'))
                ]
                .into_iter()
            )
            .all(|(a, b)| a.contents_eq(&b)));
    }

    #[test]
    fn test_pop_paren_expr_easy() {
        let tokens = lex("(\\a.(\\b.a))(a)(b)").unwrap();

        assert!(tokens
            .into_iter()
            .zip(
                vec![
                    Span::new(0, Token::LeftParen),
                    Span::new(1, Token::Lambda),
                    Span::new(2, Token::Id('a')),
                    Span::new(3, Token::Dot),
                    Span::new(4, Token::LeftParen),
                    Span::new(5, Token::Lambda),
                    Span::new(6, Token::Id('b')),
                    Span::new(7, Token::Dot),
                    Span::new(8, Token::Id('a')),
                    Span::new(9, Token::RightParen),
                    Span::new(10, Token::RightParen),
                    Span::new(11, Token::LeftParen),
                    Span::new(12, Token::Id('a')),
                    Span::new(13, Token::RightParen),
                    Span::new(14, Token::LeftParen),
                    Span::new(15, Token::Id('b')),
                    Span::new(16, Token::RightParen)
                ]
                .into_iter()
            )
            .all(|(a, b)| a.contents_eq(&b)));
    }

    #[test]
    fn test_to_curried_easy() {
        let s = to_curried(lex("(\\a.(\\b.a))(a)(b)").unwrap().as_slice()).unwrap();

        assert_eq!(s.len(), 3);
    }

    #[test]
    fn test_curried_free_term() {
        assert_eq!(
            pop_paren_expr(lex("a").unwrap().as_slice()).unwrap(),
            (vec![Span::new(0, Token::Id('a'))], Vec::new(),)
        );
    }

    #[test]
    fn test_curried_free_term_and_application() {
        assert_eq!(
            pop_paren_expr(lex("(\\a.a)a").unwrap().as_slice()).unwrap(),
            (
                vec![
                    Span::new(0, Token::LeftParen,),
                    Span::new(1, Token::Lambda,),
                    Span::new(2, Token::Id('a'),),
                    Span::new(3, Token::Dot,),
                    Span::new(4, Token::Id('a')),
                    Span::new(5, Token::RightParen,)
                ],
                vec![Span::new(6, Token::Id('a'))],
            )
        );
    }

    #[test]
    fn test_parse_free_term() {
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(lex("a").unwrap().as_slice()).unwrap(),
            Expr::Id('a')
        );
    }

    #[test]
    fn test_parse_parenthesized_term() {
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(lex("(a)").unwrap().as_slice()).unwrap(),
            Expr::Id('a')
        );
    }

    #[test]
    fn test_parse_parenthesized_applications() {
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(lex("(a)(a)").unwrap().as_slice()).unwrap(),
            Expr::Application {
                lhs: Box::new(Expr::Id('a')),
                rhs: Box::new(Expr::Id('a'))
            },
        );
    }

    #[test]
    fn test_parse_complex_expression() {
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(lex("(\\a.a)(a)").unwrap().as_slice())
                .unwrap(),
            Expr::Application {
                lhs: Box::new(Expr::Abstraction {
                    bind_id: 'a',
                    body: Box::new(Expr::Id('a'))
                }),
                rhs: Box::new(Expr::Id('a'))
            },
        );
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(lex("(\\a.a)(a)(a)").unwrap().as_slice())
                .unwrap(),
            Expr::Application {
                lhs: Box::new(Expr::Application {
                    lhs: Box::new(Expr::Abstraction {
                        bind_id: 'a',
                        body: Box::new(Expr::Id('a'))
                    }),
                    rhs: Box::new(Expr::Id('a')),
                }),
                rhs: Box::new(Expr::Id('a'))
            },
        );
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(
                lex("(\\a.(\\b.a))(a)(a)").unwrap().as_slice()
            )
            .unwrap(),
            Expr::Application {
                lhs: Box::new(Expr::Application {
                    lhs: Box::new(Expr::Abstraction {
                        bind_id: 'a',
                        body: Box::new(Expr::Abstraction {
                            bind_id: 'b',
                            body: Box::new(Expr::Id('a'))
                        })
                    }),
                    rhs: Box::new(Expr::Id('a'))
                }),
                rhs: Box::new(Expr::Id('a'))
            },
        );
    }

    /*#[test]
    fn test_contains_free() {
        assert!(<&TokenStream as TryInto<Expr>>::try_into(
            lex("(\\a.(\\a.b)a)").unwrap().as_slice()
        )
        .unwrap()
        .contains_free('b'));
    }

    #[test]
    fn test_alpha_rename() {
        assert!(<&TokenStream as TryInto<Expr>>::try_into(
            lex("(\\a.(\\a.b)a)").unwrap().as_slice()
        )
        .unwrap()
        .rename_free('b', 'c')
        .contains_free('c'));
    }*/
}
