use std::{
    fmt,
    io::{self, Write},
};

const WELLFORMED_ABSTRACTION_HEADER: [Token; 4] =
    [Token::LeftParen, Token::Lambda, Token::Id('*'), Token::Dot];

type TokenBuff = Vec<Span<Token>>;

type TokenStream = [Span<Token>];

#[derive(Debug, PartialEq, Clone)]
enum Error {
    UnrecognizedSymbol(char),
    UnrecognizedToken(Token),
    UnbalancedParen,
    EmptyExpression,
}

#[derive(Debug, PartialEq, Clone)]
struct Span<T: fmt::Debug + PartialEq + Clone> {
    pos: usize,
    content: T,
}

impl Span<Token> {
    fn as_err_unrecognized_token(&self) -> Span<Error> {
        Span {
            pos: self.pos,
            content: Error::UnrecognizedToken(self.content.clone()),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Expr {
    Id(char),
    Abstraction { bind_id: char, body: Box<Expr> },
    Application { lhs: Box<Expr>, rhs: Box<Expr> },
}

/// Pops a parenthesized expression from the token stream, leaving the rest of the token stream
/// intact.
fn pop_paren_expr(tok_stream: &TokenStream) -> Result<(TokenBuff, TokenBuff), Span<Error>> {
    let mut buff: TokenBuff = tok_stream.into();
    let mut expr = TokenBuff::new();
    let mut left_parens = Vec::new();

    while (!left_parens.is_empty() || expr.is_empty()) && !buff.is_empty() {
        let token = buff.remove(0);

        match token {
            Span {
                pos: _,
                content: Token::LeftParen,
            } => {
                left_parens.push(());
            }
            Span {
                pos: _,
                content: Token::RightParen,
            } => {
                left_parens.pop();
            }
            Span { pos: _, content: _ } => {}
        }

        expr.push(token);
    }

    if !left_parens.is_empty() {
        return Err(Span {
            pos: buff[buff.len() - 1].pos,
            content: Error::UnbalancedParen,
        });
    }

    Ok((expr, buff))
}

/// Groups curried function application arguments by parenthesis.
fn to_curried(tok_stream: &TokenStream) -> Result<Vec<TokenBuff>, Span<Error>> {
    let mut buff: TokenBuff = tok_stream.into();
    let mut terms = Vec::new();

    while !buff.is_empty() {
        let (popped, remaining) = pop_paren_expr(&buff)?;

        terms.push(popped);
        buff = remaining;
    }

    Ok(terms)
}

impl TryFrom<&TokenStream> for Expr {
    type Error = Span<Error>;

    fn try_from(tok_stream: &TokenStream) -> Result<Self, Self::Error> {
        let mut applicands = to_curried(tok_stream)?;

        // Single term
        if applicands.len() == 1 {
            let mut term = applicands.pop().ok_or(Span {
                pos: 0,
                content: Error::EmptyExpression,
            })?;

            // Free term
            if term.len() == 1 {
                let tok = term.remove(0);

                match tok {
                    Span {
                        pos: _,
                        content: Token::Id(c),
                    } => {
                        return Ok(Expr::Id(c));
                    }
                    Span { pos, content } => {
                        return Err(Span {
                            pos,
                            content: Error::UnrecognizedToken(content),
                        });
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
                .filter(|(a, b)| !a.content.is_like(b))
                .next()
            {
                return Err(unexpected_token.as_err_unrecognized_token());
            }

            let body_tokens = &term[WELLFORMED_ABSTRACTION_HEADER.len()..term.len() - 1];
            let body: Expr = body_tokens.try_into()?;

            if let Span {
                pos: _,
                content: Token::Id(c),
            } = term[2]
            {
                return Ok(Expr::Abstraction {
                    bind_id: c,
                    body: Box::new(body),
                });
            }
        }

        // Continuously fold lambda terms into application from right to left
        fn fold_application(
            argument: Expr,
            mut applicands: Vec<TokenBuff>,
        ) -> Result<Expr, Span<Error>> {
            let function = applicands.remove(applicands.len() - 1);

            let curr = Expr::Application {
                lhs: Box::new(function.as_slice().try_into()?),
                rhs: Box::new(argument),
            };

            if applicands.len() == 1 {
                return Ok(curr);
            }

            fold_application(curr, applicands)
        }

        let arg = applicands.remove(applicands.len() - 1);

        fold_application(arg.as_slice().try_into()?, applicands)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Id(char),
    Dot,
    Lambda,
    LeftParen,
    RightParen,
}

impl Token {
    fn is_like(&self, other: &Token) -> bool {
        return self == other
            || match (self, other) {
                (Self::Id(_), Self::Id(_)) => true,
                _ => false,
            };
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
                if x >= 'a' && x <= 'z' {
                    Ok(Self::Id(c))
                } else {
                    Err(Error::UnrecognizedSymbol(c))
                }
            }
        }
    }
}

fn lex(input: &str) -> Result<TokenBuff, Span<Error>> {
    input
        .chars()
        .enumerate()
        .filter(|(_, c)| !c.is_whitespace())
        .map(|(i, c)| {
            c.try_into()
                .map_err(|e| Span { pos: i, content: e })
                .map(|token| Span {
                    pos: i,
                    content: token,
                })
        })
        .collect::<Result<_, _>>()
}

fn main() {
    let input = io::stdin();
    let mut output = io::stdout();

    loop {
        print!("> ");

        output.flush().expect("failed to flush input");

        let mut input_buff = String::new();
        let n_bytes = input
            .read_line(&mut input_buff)
            .expect("failed to read input");

        if n_bytes == 0 {
            break;
        }

        let tok_stream = lex(&input_buff).expect("failed to lex");
        let expr: Expr = tok_stream
            .as_slice()
            .try_into()
            .expect("failed to construct AST");

        println!("{:?}", expr);
    }
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

        assert_eq!(
            tokens[0],
            Span {
                pos: 0,
                content: Token::Id('a')
            }
        );
    }

    #[test]
    fn test_lex_abstraction() {
        let s = "\\a.a";

        let tokens = lex(s).unwrap();

        assert_eq!(
            tokens,
            vec![
                Span {
                    pos: 0,
                    content: Token::Lambda,
                },
                Span {
                    pos: 1,
                    content: Token::Id('a'),
                },
                Span {
                    pos: 2,
                    content: Token::Dot,
                },
                Span {
                    pos: 3,
                    content: Token::Id('a'),
                }
            ]
        );
    }

    #[test]
    fn test_lex_application() {
        let s = "(\\a.a)a";

        let tokens = lex(s).unwrap();

        assert_eq!(
            tokens,
            vec![
                Span {
                    pos: 0,
                    content: Token::LeftParen,
                },
                Span {
                    pos: 1,
                    content: Token::Lambda
                },
                Span {
                    pos: 2,
                    content: Token::Id('a'),
                },
                Span {
                    pos: 3,
                    content: Token::Dot,
                },
                Span {
                    pos: 4,
                    content: Token::Id('a')
                },
                Span {
                    pos: 5,
                    content: Token::RightParen,
                },
                Span {
                    pos: 6,
                    content: Token::Id('a')
                }
            ]
        );
    }

    #[test]
    fn test_pop_paren_expr() {
        for i in 0..1_000 {
            let s = "()".repeat(i);
            let mut tokens = lex(&s).unwrap();

            for j in 0..i {
                let (expr, rest) = pop_paren_expr(tokens.as_slice()).unwrap();

                assert_eq!(
                    expr,
                    vec![
                        Span {
                            pos: 2 * j,
                            content: Token::LeftParen
                        },
                        Span {
                            pos: 2 * j + 1,
                            content: Token::RightParen
                        }
                    ]
                );

                tokens = rest;
            }
        }
    }

    #[test]
    fn test_to_curried() {
        for i in 0..1_000 {
            let s = "()".repeat(i);
            let tokens = lex(&s).unwrap();

            let parts = to_curried(tokens.as_slice()).unwrap();

            assert_eq!(parts.len(), i);

            for (i, part) in parts.iter().enumerate() {
                assert_eq!(
                    part.as_slice(),
                    [
                        Span {
                            pos: 2 * i,
                            content: Token::LeftParen
                        },
                        Span {
                            pos: 2 * i + 1,
                            content: Token::RightParen
                        }
                    ]
                );
            }
        }
    }

    #[test]
    fn test_curried_free_term() {
        assert_eq!(
            pop_paren_expr(lex("a").unwrap().as_slice()).unwrap(),
            (
                vec![Span {
                    pos: 0,
                    content: Token::Id('a')
                }],
                Vec::new(),
            )
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            <&TokenStream as TryInto<Expr>>::try_into(lex("(\\a.a)(a)").unwrap().as_slice())
                .unwrap(),
            Expr::Application {
                lhs: Box::new(Expr::Abstraction {
                    bind_id: 'a',
                    body: Box::new(Expr::Id('a'))
                }),
                rhs: Box::new(Expr::Id('a')),
            }
        );
    }
}
