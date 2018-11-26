use std::collections::HashMap;
use std::fmt;

use super::parser::Address;
use super::parser::BasicOpCode;
use super::parser::LabelRef;
use super::parser::Number;
use super::parser::Parsed;
use super::parser::ParsedLine;
use super::parser::Register;
use super::parser::SpecialOpCode;
use super::parser::ValueA;
use super::parser::ValueB;

#[derive(Debug)]
pub enum Word {
    Raw(u16),
    LabelRef(String),
}

#[derive(Default)]
pub struct LabelResolvePass {
    words: Vec<Word>,
    line_lens: Vec<usize>,
    labels: HashMap<String, u16>,
    curr_offset: u16,
}

impl LabelResolvePass {
    fn push_basic(
        &mut self,
        op: u16,
        b_word: Option<Word>,
        a_word: Option<Word>,
    ) {
        let mut offset = 1;
        self.words.push(Word::Raw(op));
        // The spec says the A bits of an op are always handled before
        // the B bits which imples that the A word is appended to the
        // vec of words prior to the B word.
        if let Some(w) = a_word {
            self.words.push(w);
            offset += 1;
        }
        if let Some(w) = b_word {
            self.words.push(w);
            offset += 1;
        }
        self.line_lens.push(offset);
        self.curr_offset += offset as u16;
    }

    fn push_special(&mut self, op: u16, a_word: Option<Word>) {
        let mut offset = 1;
        self.words.push(Word::Raw(op));
        if let Some(w) = a_word {
            self.words.push(w);
            offset += 1;
        }
        self.line_lens.push(offset);
        self.curr_offset += offset as u16;
    }

    fn push_label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.curr_offset);
    }

    fn finalize(self) -> Listing {
        let mut listing: Listing = Default::default();
        listing.line_lens = self.line_lens;
        for word in self.words {
            match word {
                Word::Raw(_) => {
                    listing.words.push(word);
                }
                Word::LabelRef(name) => {
                    // TODO: Don't unwrap here
                    let offset = self.labels.get(&name).unwrap();
                    listing.words.push(Word::Raw(*offset));
                }
            }
        }
        listing
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Word::Raw(ref n) => write!(f, "0x{:04x}", n),
            Word::LabelRef(ref s) => write!(f, "Ref({})", s),
        }
    }
}

#[derive(Default)]
pub struct Listing {
    words: Vec<Word>,
    line_lens: Vec<usize>,
}

impl fmt::Display for Listing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut words = self.words.iter();
        let mut result = String::new();
        for offset in self.line_lens.iter() {
            result += &format!(
                "{}\n",
                words
                    .by_ref()
                    .take(*offset)
                    .map(|word| format!("{}", word))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
        }
        write!(f, "{}", result)
    }
}

pub fn instr_to_bits(line: &ParsedLine) -> u16 {
    match line {
        ParsedLine::Basic(code, _, _) => {
            use self::BasicOpCode::*;
            match code {
                Set => 0x01,
                Add => 0x02,
                Sub => 0x03,
                Mul => 0x04,
                Mli => 0x05,
                Div => 0x06,
                Dvi => 0x07,
                Mod => 0x08,
                Mdi => 0x09,
                And => 0x0a,
                Bor => 0x0b,
                Xor => 0x0c,
                Shr => 0x0d,
                Asr => 0x0e,
                Shl => 0x0f,
                Ifb => 0x10,
                Ifc => 0x11,
                Ife => 0x12,
                Ifn => 0x13,
                Ifg => 0x14,
                Ifa => 0x15,
                Ifl => 0x16,
                Ifu => 0x17,
                Adx => 0x1a,
                Sbx => 0x1b,
                Sti => 0x1e,
                Std => 0x1f,
            }
        }
        ParsedLine::Special(code, _) => {
            use self::SpecialOpCode::*;
            match code {
                Jsr => 0x01,
                Int => 0x08,
                Iag => 0x09,
                Ias => 0x0a,
                Rfi => 0x0b,
                Iaq => 0x0c,
                Hwn => 0x10,
                Hwq => 0x11,
                Hwi => 0x12,
            }
        }
        ParsedLine::Label(_) => panic!("Learn to program, bub"),
    }
}

pub fn reg_to_bits(reg: &Register) -> u16 {
    use self::Register::*;

    match reg {
        A => 0x00,
        B => 0x01,
        C => 0x02,
        X => 0x03,
        Y => 0x04,
        Z => 0x05,
        I => 0x06,
        J => 0x07,
    }
}

pub fn addr_to_bits(addr: &Address) -> (u16, Option<Word>) {
    use self::Address::*;

    match addr {
        Reg(ref r) => (reg_to_bits(r) + 0x08, None),
        RegPlusNum(r, Number(n)) => {
            (reg_to_bits(r) + 0x10, Some(Word::Raw(*n)))
        }
        Num(Number(n)) => (0x1e, Some(Word::Raw(*n))),
        Label(LabelRef(s)) => (0x1e, Some(Word::LabelRef(s.to_string()))),
    }
}

pub fn val_a_to_bits(val: &ValueA) -> (u16, Option<Word>) {
    use self::ValueA::*;
    let (unshifted, next_word) = match val {
        Reg(ref r) => (reg_to_bits(r), None),
        Addr(ref a) => addr_to_bits(a),
        Pop => (0x18, None),
        Peek => (0x19, None),
        Pick(Number(ref n)) => (0x1a, Some(Word::Raw(*n))),
        Sp => (0x1b, None),
        Pc => (0x1c, None),
        Ex => (0x1d, None),
        Num(Number(ref n)) => match n {
            0...30 => ((n + 0x21), None),
            65535 => (0x20, None),
            _ => (0x1f, Some(Word::Raw(*n))),
        },
    };
    (unshifted, next_word)
}

pub fn val_b_to_bits(val: &ValueB) -> (u16, Option<Word>) {
    use self::ValueB::*;
    let (unshifted, next_word) = match val {
        Reg(ref r) => (reg_to_bits(r), None),
        Addr(ref a) => addr_to_bits(a),
        Push => (0x18, None),
        Peek => (0x19, None),
        Pick(Number(n)) => (0x1a, Some(Word::Raw(*n))),
        Sp => (0x1b, None),
        Pc => (0x1c, None),
        Ex => (0x1d, None),
    };
    (unshifted, next_word)
}

pub fn generate_code(parsed: Parsed) -> Listing {
    let mut resolve_pass: LabelResolvePass = Default::default();

    for parsed_line in parsed.get_lines() {
        match parsed_line {
            ParsedLine::Basic(_, b, a) => {
                let code_bits = instr_to_bits(&parsed_line);
                let (b_bits, b_word) = val_b_to_bits(&b);
                let (a_bits, a_word) = val_a_to_bits(&a);
                let op = code_bits | (b_bits << 5) | (a_bits << 10);
                resolve_pass.push_basic(op, b_word, a_word);
            }
            ParsedLine::Special(_, a) => {
                let code_bits = instr_to_bits(&parsed_line);
                let (a_bits, a_word) = val_a_to_bits(&a);
                let op = (code_bits << 5) | (a_bits << 10);
                resolve_pass.push_special(op, a_word);
            }
            ParsedLine::Label(name) => {
                resolve_pass.push_label(name);
            }
        }
    }
    resolve_pass.finalize()
}
