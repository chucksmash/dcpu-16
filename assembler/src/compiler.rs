use std::fmt;

use super::parser::Address;
use super::parser::BasicOpCode;
use super::parser::Number;
use super::parser::Parsed;
use super::parser::ParsedLine;
use super::parser::Register;
use super::parser::SpecialOpCode;
use super::parser::ValueA;
use super::parser::ValueB;

pub struct Asm {
    out: Vec<u16>,
}

impl Asm {
    pub fn new(op_word: u16, b_word: Option<u16>, a_word: Option<u16>) -> Asm {
        let mut asm = Asm { out: vec![op_word] };
        if let Some(b) = b_word {
            asm.out.push(b);
        };
        if let Some(a) = a_word {
            asm.out.push(a);
        };
        asm
    }
}

impl fmt::Display for Asm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let result = self
            .out
            .iter()
            .map(|word| format!("{:x}", word))
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "{}", result)
    }
}

pub struct Listing {
    lines: Vec<Asm>,
}

impl fmt::Display for Listing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in self.lines.iter() {
            writeln!(f, "{}", line)?
        }
        Ok(())
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
                Jsr => 0x01 << 5,
                Int => 0x08 << 5,
                Iag => 0x09 << 5,
                Ias => 0x0a << 5,
                Rfi => 0x0b << 5,
                Iaq => 0x0c << 5,
                Hwn => 0x10 << 5,
                Hwq => 0x11 << 5,
                Hwi => 0x12 << 5,
            }
        }
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

pub fn addr_to_bits(addr: &Address) -> (u16, Option<u16>) {
    use self::Address::*;

    match addr {
        Reg(ref r) => (reg_to_bits(r) + 0x08, None),
        RegPlusNum(r, Number(n)) => (reg_to_bits(r) + 0x10, Some(*n)),
        Num(Number(n)) => (0x1e, Some(*n)),
    }
}

pub fn val_a_to_bits(val: &ValueA) -> (u16, Option<u16>) {
    use self::ValueA::*;
    const SHIFT: usize = 10;
    let (unshifted, next_word) = match val {
        Reg(ref r) => (reg_to_bits(r), None),
        Addr(ref a) => addr_to_bits(a),
        Pop => (0x18, None),
        Peek => (0x19, None),
        Pick(Number(ref n)) => (0x1a, Some(*n)),
        Sp => (0x1b, None),
        // TODO: Pc?!?
        Ex => (0x1d, None),
        Num(Number(ref n)) => match n {
            0...30 => ((n + 0x21), None),
            65535 => (0x20, None),
            _ => (0x1f, Some(*n)),
        },
    };
    (unshifted << SHIFT, next_word)
}

pub fn val_b_to_bits(val: &ValueB) -> (u16, Option<u16>) {
    use self::ValueB::*;
    const SHIFT: usize = 10;
    let (unshifted, next_word) = match val {
        Reg(ref r) => (reg_to_bits(r), None),
        Addr(ref a) => addr_to_bits(a),
        Push => (0x18, None),
        Peek => (0x19, None),
        Pick(Number(n)) => (0x1a, Some(*n)),
        Sp => (0x1b, None),
        // TODO: Pc?!?
        Ex => (0x1d, None),
    };
    (unshifted << SHIFT, next_word)
}

pub fn assemble(parsed: Parsed) -> Listing {
    let mut listing = Listing { lines: vec![] };
    for parsed_line in parsed.get_lines() {
        let asm = match parsed_line {
            ParsedLine::Basic(code, b, a) => {
                let code_bits = instr_to_bits(&parsed_line);
                let (b_bits, b_word) = val_b_to_bits(&b);
                let (a_bits, a_word) = val_a_to_bits(&a);
                let op_bits = code_bits | b_bits | a_bits;
                Asm::new(op_bits, b_word, a_word)
            }
            ParsedLine::Special(code, a) => {
                let code_bits = instr_to_bits(&parsed_line);
                let (b_bits, b_word) = (0x0, None);
                let (a_bits, a_word) = val_a_to_bits(&a);
                let op_bits = code_bits | b_bits | a_bits;
                Asm::new(op_bits, b_word, a_word)
            }
        };
        listing.lines.push(asm);
    }
    listing
}
