extern crate assembler;

use assembler::lexer;
use assembler::parser;

fn main() {
    let input = "ADD    [0x21], 0b101
SET B   ,     123
jsr [11]
   set c ,0b11
";
    let lexed_lines = lexer::lex(input).unwrap();
    let parsed_lines = parser::parse(&mut lexed_lines.iter()).unwrap();
    for line in parsed_lines.iter() {
        println!("{:?}", line);
    }
}
