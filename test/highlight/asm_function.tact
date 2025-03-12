asm(-> 1_0_1) fun pushInt(): Int {
// <- keyword
// ^ punctuation.bracket
//  ^ operator
//     ^ number
//      ^ number
//          ^ punctuation.bracket
//            ^ keyword
//                ^ function
//                       ^ punctuation.bracket
//                        ^ punctuation.bracket
//                         ^ punctuation.delimiter
//                           ^ type.builtin
//                               ^ punctuation.bracket
    42 PUSHINT
}

asm fun sequences() {
    <{ 42 PUSHINT }> PUSHCONT
    // <- punctuation.bracket
    // ^^ number
    //    ^^^^^^^ function.call
    //            ^^ punctuation.bracket

    <{ 42 PUSHINT }>CONT PUSHCONT
    //            ^^^^^^ punctuation.bracket
}

asm(x -> 000) fun inAndOut(x: Int): Int
// <- keyword
// ^ punctuation.bracket
//  ^ variable
//    ^ operator
//       ^ number
//          ^ punctuation.bracket
{
NOP
}

asm fun noWhitespace() {b{00} PUSHSLICE}

asm fun otherSamples() {
    // comments
    // <- comment
    /* comments */
    // <- comment

    // string
    "  YAYAYA  " DEBUGSTR
    // <- string
    // ^^^^^^^^^ string
    //           ^^^^^^^^ function.call

    // hex bitstring
    x{DEADBEEF_} PUSHSLICE
    // <- function.macro
    // ^^^^^^^^^ function.macro
    //           ^ function.call
    x{babecafe}  SLICE
    // <- function.macro
    //           ^ function.call
    x{}          SLICE
    // <- function.macro
    //           ^ function.call

    // binary bitstring
    b{1} SLICE
    // <- function.macro
    // ^ function.macro
    //   ^ function.call
    b{}  PUSHSLICE
    // <- function.macro
    //   ^ function.call

    // BoC in hex
    B{DEADBEEF_} B>boc PUSHSLICE
    // <- function.macro
    // ^^^^^^^^^^^^^^^ function.macro
    //                 ^ function.call
    B{babecafe}  B>boc SLICE
    // <- function.macro
    // ^^^^^^^^^^^^^^^ function.macro
    //                 ^ function.call
    B{}          B>boc SLICE
    // <- function.macro
    // ^^^^^^^^^^^^^^^ function.macro
    //                 ^ function.call
    <b b> PUSHREF
    // <- function.macro
    // ^^ function.macro
    //    ^ function.call

    // Control register
    c0 PUSHCTR
    // <- string.special.symbol
    // ^ function.call
    c15 PUSH
    // <- string.special.symbol
    //  ^ function.call

    // Stack register
    s0 PUSH
    // <- string.special.symbol
    // ^ function.call
    s15 POP
    // <- string.special.symbol
    //  ^ function.call
    16 s() PUSH
    // <- string.special.symbol
    // ^^^ string.special.symbol
    //     ^ function.call

    // Number
    0    PUSHINT
    // <- number
    //   ^ function.call
    500  INT
    // <- number
    //   ^ function.call
    -42  INT
    // <- number
    //   ^ function.call
    0b10 INT
    // <- number
    //   ^ function.call
    0x0f INT
    // <- number
    //   ^ function.call
    0xFF INT
    // <- number
    //   ^ function.call

    // Calls
    $global_contractBasechainAddress INLINECALLDICT
    // <- variable
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //                               ^ function.call
    $Deploy$_load_without_opcode     CALLDICT
    // <- variable
    // ^^^^^^^^^^^^^^^^^^^^^^^^^ variable
    //                               ^ function.call
    %lshift                          CALL
    // <- variable
    // ^^^^ variable
    //                               ^ function.call
    __tact_context                   JMP
    // <- variable
    // ^^^^^^^^^^^ variable
    //                               ^ function.call
}

