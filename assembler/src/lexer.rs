use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
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
    fn extract_ident(chars: &mut Peekable<Chars>) -> Token {
        let mut result = String::new();
        result.push(chars.next().unwrap());
        while let Some(c) = chars.peek() {
            match c {
                'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => {
                    result.push(*c);
                    chars.next();
                }
                _ => break,
            };
        }
        return Token::Ident(result);
    }

    fn extract_number(chars: &mut Peekable<Chars>) -> Result<Token, ()> {
        let starts_with_zero = chars.peek() == Some(&'0');
        let mut val = 0;
        let mut num_chars = 0;
        let mut num_literal = String::new();
        let mut looks_hexy = false;
        let mut looks_biny = false;
        while let Some(c) = chars.peek() {
            num_literal.push(*c);
            match c {
                'x' | 'X' if num_chars == 1 && starts_with_zero => {
                    looks_hexy = true;
                }
                'b' | 'B' if num_chars == 1 && starts_with_zero => {
                    looks_biny = true;
                }
                'a'...'f' | 'A'...'F' | '0'...'9' => {
                    if looks_hexy {
                        let parsed = from_hex_digit(*c)?;
                        val = val * 16 + parsed;
                    } else if looks_biny {
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

        if (looks_hexy || looks_biny) && num_chars <= 2 {
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
                tokens.push(Token::extract_ident(&mut chars))
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
