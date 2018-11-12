use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub struct LexedLine<'a> {
    tokens: Vec<Token>,
    raw: &'a str,
}

impl<'a> LexedLine<'a> {
    pub fn get_tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn get_raw(&self) -> &'a str {
        &self.raw
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenBracket,
    CloseBracket,
    Ident(String),
    Number(u16),
    Comma,
    Plus,
}

impl Token {
    fn extract_ident(chars: &mut Peekable<Chars>) -> Result<Token, ()> {
        let mut result = String::new();
        let mut curr_char = 0;
        while let Some(c) = chars.peek() {
            match c {
                'a'...'z' | 'A'...'Z' | '_' => {
                    result.push(*c);
                    chars.next();
                    curr_char += 1;
                }
                '0'...'9' if curr_char > 0 => {
                    result.push(*c);
                    chars.next();
                    curr_char += 1;
                }
                _ => break,
            };
        }
        match result.len() > 0 {
            true => Ok(Token::Ident(result)),
            false => Err(()),
        }
    }

    fn extract_number(chars: &mut Peekable<Chars>) -> Result<Token, ()> {
        let starts_with_zero = chars.peek() == Some(&'0');
        let mut val = 0;
        let mut num_chars = 0;
        let mut num_literal = String::new();
        let mut is_hex = false;
        let mut is_bin = false;
        while let Some(c) = chars.peek() {
            num_literal.push(*c);
            match c {
                'x' | 'X' if num_chars == 1 && starts_with_zero => {
                    is_hex = true;
                }
                'b' | 'B' if num_chars == 1 && starts_with_zero => {
                    is_bin = true;
                }
                'a'...'f' | 'A'...'F' | '0'...'9' => {
                    if is_hex {
                        let parsed = from_hex_digit(*c)?;
                        val = val * 16 + parsed;
                    } else if is_bin {
                        let parsed = from_bin_digit(*c)?;
                        val = val * 2 + parsed;
                    } else {
                        let parsed = from_decimal_digit(*c)?;
                        val = val * 10 + parsed;
                    }
                }
                _ => break,
            };
            num_chars += 1;
            chars.next();
        }

        if (is_hex || is_bin) && num_chars <= 2 {
            return Err(());
        }
        Ok(Token::Number(val))
    }
}

fn from_hex_digit(c: char) -> Result<u16, ()> {
    match c {
        '0'...'9' => Ok((c as u16) - ('0' as u16)),
        'a'...'f' => Ok(10 + (c as u16) - ('a' as u16)),
        'A'...'F' => Ok(10 + (c as u16) - ('A' as u16)),
        _ => Err(()),
    }
}

fn from_decimal_digit(c: char) -> Result<u16, ()> {
    match c {
        '0'...'9' => Ok((c as u16) - ('0' as u16)),
        _ => Err(()),
    }
}

fn from_bin_digit(c: char) -> Result<u16, ()> {
    match c {
        '0'...'1' => Ok((c as u16) - ('0' as u16)),
        _ => Err(()),
    }
}

pub fn lex<'a>(input: &'a str) -> Result<Vec<LexedLine<'a>>, ()> {
    let mut lines = vec![];
    for line in input.lines() {
        lines.push(lex_line(line)?);
    }
    Ok(lines)
}

fn lex_line<'a>(line: &'a str) -> Result<LexedLine<'a>, ()> {
    let mut chars = line.chars().peekable();
    let mut tokens = vec![];
    while let Some(ref c) = chars.peek() {
        match c {
            ' ' => {
                chars.next();
            }
            '[' => {
                tokens.push(Token::OpenBracket);
                chars.next();
            }
            ']' => {
                tokens.push(Token::CloseBracket);
                chars.next();
            }
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            }
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '0'...'9' => tokens.push(Token::extract_number(&mut chars)?),
            'a'...'z' | 'A'...'Z' | '_' => {
                tokens.push(Token::extract_ident(&mut chars)?)
            }
            ';' => {
                // begins a comment which extends to end of line
                break;
            }
            _ => return Err(()),
        }
    }
    Ok(LexedLine { tokens, raw: line })
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! peek {
        ($e:expr) => {
            $e.chars().peekable()
        };
    }

    #[test]
    fn extracts_idents() {
        let mut peekable = peek!("abc123_12a abc");
        let expected = Ok(Token::Ident("abc123_12a".to_string()));
        assert_eq!(Token::extract_ident(&mut peekable), expected);

        let mut peekable = peek!("123");
        let expected = Err(());
        assert_eq!(Token::extract_ident(&mut peekable), expected);
    }

    #[test]
    fn extracts_nums() {
        let mut peekable = peek!("65535 12");
        let expected = Ok(Token::Number(65535));
        assert_eq!(Token::extract_number(&mut peekable), expected);

        let mut peekable = peek!("0x10 10");
        let expected = Ok(Token::Number(16));
        assert_eq!(Token::extract_number(&mut peekable), expected);

        let mut peekable = peek!("0b101 22");
        let expected = Ok(Token::Number(5));
        assert_eq!(Token::extract_number(&mut peekable), expected);
    }

    #[test]
    #[should_panic]
    fn fails_to_extract_big_nums() {
        let mut peekable = peek!("65536");
        Token::extract_number(&mut peekable);
    }

    #[test]
    fn basic_line_lex() {
        let line = "ADD A, 1";
        let expected = Ok(LexedLine {
            tokens: vec![
                Token::Ident("ADD".to_string()),
                Token::Ident("A".to_string()),
                Token::Comma,
                Token::Number(1),
            ],
            raw: "ADD A, 1",
        });
        assert_eq!(lex_line(line), expected);
    }

    #[test]
    fn complicated_line_lex() {
        let line = "SET    [A + 0x12  ] , [POP]     ; this is a line";
        let expected = Ok(LexedLine {
            tokens: vec![
                Token::Ident("SET".to_string()),
                Token::OpenBracket,
                Token::Ident("A".to_string()),
                Token::Plus,
                Token::Number(18),
                Token::CloseBracket,
                Token::Comma,
                Token::OpenBracket,
                Token::Ident("POP".to_string()),
                Token::CloseBracket,
            ],
            raw: "SET    [A + 0x12  ] , [POP]     ; this is a line",
        });
        assert_eq!(lex_line(line), expected);
    }

    #[test]
    fn lex_multiple_lines() {
        let input = "ADD A, 1
SET    [A + 0x12  ] , [POP]     ; this is a line";
        let expected = Ok(vec![
            LexedLine {
                tokens: vec![
                    Token::Ident("ADD".to_string()),
                    Token::Ident("A".to_string()),
                    Token::Comma,
                    Token::Number(1),
                ],
                raw: "ADD A, 1",
            },
            LexedLine {
                tokens: vec![
                    Token::Ident("SET".to_string()),
                    Token::OpenBracket,
                    Token::Ident("A".to_string()),
                    Token::Plus,
                    Token::Number(18),
                    Token::CloseBracket,
                    Token::Comma,
                    Token::OpenBracket,
                    Token::Ident("POP".to_string()),
                    Token::CloseBracket,
                ],
                raw: "SET    [A + 0x12  ] , [POP]     ; this is a line",
            },
        ]);
        assert_eq!(lex(input), expected);
    }
}
