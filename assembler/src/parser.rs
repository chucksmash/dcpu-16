use std::iter::Peekable;
use std::slice::Iter;

use super::lexer::{LexedLine, Token};

macro_rules! get {
    ($e:expr) => {
        $e.ok_or(ParseError::UnexpectedEndOfLine)
    };
}

macro_rules! punct_token {
    ($e:expr, $cmp:expr) => {{
        let tmp = get!($e)?;
        if tmp != $cmp {
            return Err(ParseError::UnexpectedToken);
        }
    }};
}

// predicates
fn is_reg(s: &str) -> bool {
    match Register::from_str(s) {
        Ok(_) => true,
        Err(_) => false,
    }
}

fn is_pop(s: &str) -> bool {
    s == "POP" || s == "pop"
}

fn is_push(s: &str) -> bool {
    s == "PUSH" || s == "push"
}

fn is_peek(s: &str) -> bool {
    s == "PEEK" || s == "peek"
}

fn is_pick(s: &str) -> bool {
    s == "PICK" || s == "pick"
}

fn is_sp(s: &str) -> bool {
    s == "SP" || s == "sp"
}

fn is_ex(s: &str) -> bool {
    s == "EX" || s == "ex"
}

#[derive(Debug)]
pub enum ParseError {
    IllegalOpCode,
    IllegalBValue,
    IllegalAValue,
    IllegalInstruction,
    IllegalRegister,
    UnexpectedEndOfLine,
    UnexpectedToken,
}

#[derive(Debug)]
pub enum BasicOpCode {
    Set,
    Add,
    Sub,
    Mul,
    Mli,
    Div,
    Dvi,
    Mod,
    Mdi,
    And,
    Bor,
    Xor,
    Shr,
    Asr,
    Shl,
    Ifb,
    Ifc,
    Ife,
    Ifn,
    Ifg,
    Ifa,
    Ifl,
    Ifu,
    Adx,
    Sbx,
    Sti,
    Std,
}

impl BasicOpCode {
    fn from_str(name: &str) -> Result<BasicOpCode, ParseError> {
        match name {
            "set" | "SET" => Ok(BasicOpCode::Set),
            "add" | "ADD" => Ok(BasicOpCode::Add),
            "sub" | "SUB" => Ok(BasicOpCode::Sub),
            "mul" | "MUL" => Ok(BasicOpCode::Mul),
            "mli" | "MLI" => Ok(BasicOpCode::Mli),
            "div" | "DIV" => Ok(BasicOpCode::Div),
            "dvi" | "DVI" => Ok(BasicOpCode::Dvi),
            "mod" | "MOD" => Ok(BasicOpCode::Mod),
            "mdi" | "MDI" => Ok(BasicOpCode::Mdi),
            "and" | "AND" => Ok(BasicOpCode::And),
            "bor" | "BOR" => Ok(BasicOpCode::Bor),
            "xor" | "XOR" => Ok(BasicOpCode::Xor),
            "shr" | "SHR" => Ok(BasicOpCode::Shr),
            "asr" | "ASR" => Ok(BasicOpCode::Asr),
            "shl" | "SHL" => Ok(BasicOpCode::Shl),
            "ifb" | "IFB" => Ok(BasicOpCode::Ifb),
            "ifc" | "IFC" => Ok(BasicOpCode::Ifc),
            "ife" | "IFE" => Ok(BasicOpCode::Ife),
            "ifn" | "IFN" => Ok(BasicOpCode::Ifn),
            "ifg" | "IFG" => Ok(BasicOpCode::Ifg),
            "ifa" | "IFA" => Ok(BasicOpCode::Ifa),
            "ifl" | "IFL" => Ok(BasicOpCode::Ifl),
            "ifu" | "IFU" => Ok(BasicOpCode::Ifu),
            "adx" | "ADX" => Ok(BasicOpCode::Adx),
            "sbx" | "SBX" => Ok(BasicOpCode::Sbx),
            "sti" | "STI" => Ok(BasicOpCode::Sti),
            "std" | "STD" => Ok(BasicOpCode::Std),
            _ => Err(ParseError::IllegalOpCode),
        }
    }

    fn parse(tokens: &mut PeekableTokens) -> Result<BasicOpCode, ParseError> {
        match get!(tokens.next())? {
            Token::Ident(s) => BasicOpCode::from_str(s),
            _ => Err(ParseError::IllegalOpCode),
        }
    }
}

#[derive(Debug)]
pub enum SpecialOpCode {
    Jsr,
    Int,
    Iag,
    Ias,
    Rfi,
    Iaq,
    Hwn,
    Hwq,
    Hwi,
}

impl SpecialOpCode {
    fn from_str(name: &str) -> Result<SpecialOpCode, ParseError> {
        match name {
            "jsr" | "JSR" => Ok(SpecialOpCode::Jsr),
            "int" | "INT" => Ok(SpecialOpCode::Int),
            "iag" | "IAG" => Ok(SpecialOpCode::Iag),
            "ias" | "IAS" => Ok(SpecialOpCode::Ias),
            "rfi" | "RFI" => Ok(SpecialOpCode::Rfi),
            "iaq" | "IAQ" => Ok(SpecialOpCode::Iaq),
            "hwn" | "HWN" => Ok(SpecialOpCode::Hwn),
            "hwq" | "HWQ" => Ok(SpecialOpCode::Hwq),
            "hwi" | "HWI" => Ok(SpecialOpCode::Hwi),
            _ => Err(ParseError::IllegalOpCode),
        }
    }

    fn parse(tokens: &mut PeekableTokens) -> Result<SpecialOpCode, ParseError> {
        match get!(tokens.next())? {
            Token::Ident(s) => SpecialOpCode::from_str(s),
            _ => Err(ParseError::IllegalOpCode),
        }
    }
}

#[derive(Debug)]
pub enum OpCode {
    Basic(BasicOpCode),
    Special(SpecialOpCode),
}

impl OpCode {
    fn from_str(name: &str) -> Result<OpCode, ParseError> {
        let basic = BasicOpCode::from_str(name);
        let special = SpecialOpCode::from_str(name);
        match (basic, special) {
            (Ok(b), Err(_)) => Ok(OpCode::Basic(b)),
            (Err(_), Ok(s)) => Ok(OpCode::Special(s)),
            (Err(_), Err(_)) => Err(ParseError::IllegalOpCode),
            (Ok(_), Ok(_)) => panic!("Impossible condition!"),
        }
    }

    fn parse(tokens: &mut PeekableTokens) -> Result<OpCode, ParseError> {
        match tokens.next() {
            Some(Token::Ident(name)) => OpCode::from_str(name),
            _ => Err(ParseError::IllegalOpCode),
        }
    }
}

#[derive(Debug)]
pub enum ValueA {
    Reg(Register),
    Addr(Address),
    Pop,
    Peek,
    Pick(Number),
    Sp,
    Ex,
    Num(Number),
}

impl ValueA {
    fn parse(tokens: &mut PeekableTokens) -> Result<ValueA, ParseError> {
        match get!(tokens.peek())? {
            Token::OpenBracket => Ok(ValueA::Addr(Address::parse(tokens)?)),
            Token::Ident(s) => {
                if is_reg(s) {
                    Ok(ValueA::Reg(Register::parse(tokens)?))
                } else if is_pop(s) {
                    tokens.next();
                    Ok(ValueA::Pop)
                } else if is_peek(s) {
                    tokens.next();
                    Ok(ValueA::Peek)
                } else if is_pick(s) {
                    tokens.next();
                    Ok(ValueA::Pick(Number::parse(tokens)?))
                } else if is_sp(s) {
                    tokens.next();
                    Ok(ValueA::Sp)
                } else if is_ex(s) {
                    tokens.next();
                    Ok(ValueA::Ex)
                } else {
                    Err(ParseError::IllegalAValue)
                }
            }
            Token::Number(_) => Ok(ValueA::Num(Number::parse(tokens)?)),
            _ => Err(ParseError::IllegalAValue),
        }
    }
}

#[derive(Debug)]
pub enum ValueB {
    Reg(Register),
    Addr(Address),
    Push,
    Peek,
    Pick(Number),
    Sp,
    Ex,
}

impl ValueB {
    fn parse(tokens: &mut PeekableTokens) -> Result<ValueB, ParseError> {
        match get!(tokens.peek())? {
            Token::OpenBracket => Ok(ValueB::Addr(Address::parse(tokens)?)),
            Token::Ident(s) => {
                if is_reg(s) {
                    Ok(ValueB::Reg(Register::parse(tokens)?))
                } else if is_push(s) {
                    tokens.next();
                    Ok(ValueB::Push)
                } else if is_peek(s) {
                    tokens.next();
                    Ok(ValueB::Peek)
                } else if is_pick(s) {
                    tokens.next();
                    Ok(ValueB::Pick(Number::parse(tokens)?))
                } else if is_sp(s) {
                    tokens.next();
                    Ok(ValueB::Sp)
                } else if is_ex(s) {
                    tokens.next();
                    Ok(ValueB::Ex)
                } else {
                    Err(ParseError::IllegalBValue)
                }
            }
            _ => Err(ParseError::IllegalBValue),
        }
    }
}

#[derive(Debug)]
pub enum Register {
    A,
    B,
    C,
    X,
    Y,
    Z,
    I,
    J,
}

impl Register {
    fn from_str(name: &str) -> Result<Register, ParseError> {
        match name {
            "A" | "a" => Ok(Register::A),
            "B" | "b" => Ok(Register::B),
            "C" | "c" => Ok(Register::C),
            "X" | "x" => Ok(Register::X),
            "Y" | "y" => Ok(Register::Y),
            "Z" | "z" => Ok(Register::Z),
            "I" | "i" => Ok(Register::I),
            "J" | "j" => Ok(Register::J),
            _ => Err(ParseError::IllegalRegister),
        }
    }

    fn parse(tokens: &mut PeekableTokens) -> Result<Register, ParseError> {
        if let Token::Ident(name) = get!(tokens.next())? {
            Register::from_str(name)
        } else {
            Err(ParseError::IllegalRegister)
        }
    }
}

#[derive(Debug)]
pub enum Address {
    Reg(Register),
    RegPlusNum(Register, Number),
    Num(Number),
}

impl Address {
    fn parse(tokens: &mut PeekableTokens) -> Result<Address, ParseError> {
        punct_token!(tokens.next(), &Token::OpenBracket);
        let tmp_tokens = tokens.clone();
        let addr_vec = tmp_tokens
            .take_while(|t| **t != Token::CloseBracket)
            .collect::<Vec<_>>();

        match *addr_vec.as_slice() {
            [&Token::Ident(_)] => {
                let reg = Register::parse(tokens)?;
                punct_token!(tokens.next(), &Token::CloseBracket);
                Ok(Address::Reg(reg))
            }
            [&Token::Number(_)] => {
                let num = Number::parse(tokens)?;
                punct_token!(tokens.next(), &Token::CloseBracket);
                Ok(Address::Num(num))
            }
            [&Token::Ident(_), &Token::Plus, &Token::Number(_)] => {
                let reg = Register::parse(tokens)?;
                punct_token!(tokens.next(), &Token::Plus);
                let num = Number::parse(tokens)?;
                punct_token!(tokens.next(), &Token::CloseBracket);
                Ok(Address::RegPlusNum(reg, num))
            }
            _ => Err(ParseError::UnexpectedToken),
        }
    }
}

#[derive(Debug)]
pub struct Number(u16);

impl Number {
    fn parse(tokens: &mut PeekableTokens) -> Result<Number, ParseError> {
        match get!(tokens.next())? {
            Token::Number(val) => Ok(Number(*val)),
            _ => Err(ParseError::UnexpectedToken),
        }
    }
}

#[derive(Debug)]
pub enum ParsedLine {
    Basic(BasicOpCode, ValueB, ValueA),
    Special(SpecialOpCode, ValueA),
}

type ParseResult = Result<ParsedLine, ParseError>;
type PeekableTokens<'a> = Peekable<Iter<'a, Token>>;
type LexedLines<'a, 'b> = Iter<'a, LexedLine<'b>>;

pub struct Parsed {
    lines: Vec<ParsedLine>,
}

impl Parsed {
    pub fn get_lines(&self) -> &[ParsedLine] {
        &self.lines
    }
}

pub fn parse(lines: &mut LexedLines) -> Result<Parsed, ParseError> {
    let mut results = vec![];
    for (idx, line) in lines.enumerate() {
        let parsed = parse_line(&mut line.get_tokens().iter().peekable());
        match parsed {
            Ok(ok_line) => results.push(ok_line),
            Err(err) => {
                eprintln!(
                    "ERROR: {:?} on line {:04}:
{}
{:?}
",
                    err,
                    idx + 1,
                    line.get_raw(),
                    line.get_tokens()
                );
                return Err(err);
            }
        }
    }
    Ok(Parsed { lines: results })
}

fn parse_line(line: &mut PeekableTokens) -> ParseResult {
    if let Some(Token::Ident(name)) = line.peek() {
        match OpCode::from_str(&name)? {
            OpCode::Basic(_) => parse_bop_line(line),
            OpCode::Special(_) => parse_sop_line(line),
        }
    } else {
        Err(ParseError::IllegalOpCode)
    }
}

fn parse_bop_line(line: &mut PeekableTokens) -> ParseResult {
    let op_code = BasicOpCode::parse(line)?;
    let b_value = ValueB::parse(line)?;
    punct_token!(line.next(), &Token::Comma);
    let a_value = ValueA::parse(line)?;
    Ok(ParsedLine::Basic(op_code, b_value, a_value))
}

fn parse_sop_line(line: &mut PeekableTokens) -> ParseResult {
    let op_code = SpecialOpCode::parse(line)?;
    let a_value = ValueA::parse(line)?;
    Ok(ParsedLine::Special(op_code, a_value))
}
