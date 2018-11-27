extern crate assembler;

use assembler::codegen;
use assembler::lexer;
use assembler::parser;

fn main() {
    let input = "JSR 12

; welcome message
chuck: SET X, 0
made: this:
SET Y, 3
SET A, 0
SET [480], 900


happen:
SET X, 0    ; change colors
SET Y, 0xf
SET A, 1
SET A, [happen]

SET I, 0    ; counter for device_draw_loop
";
    let lexed_lines = lexer::lex(input).unwrap();
    let parsed = parser::parse(&mut lexed_lines.iter()).unwrap();
    for line in parsed.get_lines().iter() {
        println!("{:?}", line);
    }
    let code = codegen::generate_code(parsed);
    println!("{}", code);
}
