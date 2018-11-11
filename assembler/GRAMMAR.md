# Productions

## a-values (AV):

AV := REGISTER
AV := ADDR
AV := "POP"
AV := "PEEK"
AV := "PICK" NUMBER
AV := "SP"
AV := "EX"
AV := NUMBER

## b-values (BV):

BV := REGISTER
BV := ADDR
BV := "PUSH"
BV := "PEEK"
BV := "PICK" NUMBER
BV := "SP"
BV := "EX"

## addresses (ADDR):

ADDR := "[" REGISTER "]"
ADDR := "[" REGISTER "+" NUMBER "]"
ADDR := "[" NUMBER "]"

## registers (REGISTER):

REGISTER := "A"
REGISTER := "B"
REGISTER := "C"
REGISTER := "X"
REGISTER := "Y"
REGISTER := "Z"
REGISTER := "I"
REGISTER := "J"

## numbers (NUMBER):

NUMBER := INT
NUMBER := HEX
NUMBER := BIN

### decimal (INT):

INT := INT_DIGIT+

INT_DIGIT := [0-9]

### hexadecimal (HEX):

HEX := "0x" HEX_DIGIT+
HEX := "0X" HEX_DIGIT+

HEX_DIGIT := [0-1a-fA-F]

### binary (BIN):

BIN := "0b" BIN_DIGIT+
BIN := "0B" BIN_DIGIT+

BIN_DIGIT := "0" | "1"

## basic opcodes (BOP):

BOP := "SET" BV "," AV
BOP := "ADD" BV "," AV
BOP := "SUB" BV "," AV
BOP := "MUL" BV "," AV
BOP := "MLI" BV "," AV
BOP := "DIV" BV "," AV
BOP := "DVI" BV "," AV
BOP := "MOD" BV "," AV
BOP := "MDI" BV "," AV
BOP := "AND" BV "," AV
BOP := "BOR" BV "," AV
BOP := "XOR" BV "," AV
BOP := "SHR" BV "," AV
BOP := "ASR" BV "," AV
BOP := "SHL" BV "," AV
BOP := "IFB" BV "," AV
BOP := "IFC" BV "," AV
BOP := "IFE" BV "," AV
BOP := "IFN" BV "," AV
BOP := "IFG" BV "," AV
BOP := "IFA" BV "," AV
BOP := "IFL" BV "," AV
BOP := "IFU" BV "," AV
BOP := "ADX" BV "," AV
BOP := "SBX" BV "," AV
BOP := "STI" BV "," AV
BOP := "STD" BV "," AV

## special opcodes (SOP):

SOP := "JSR" AV
SOP := "INT" AV
SOP := "IAG" AV
SOP := "IAS" AV
SOP := "RFI" AV
SOP := "IAQ" AV
SOP := "HWN" AV
SOP := "HWQ" AV
SOP := "HWI" AV

## lines (LINE)

LINE := BOP
LINE := SOP
