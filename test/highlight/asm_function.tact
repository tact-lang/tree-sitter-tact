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

// TODO: highlighting queries! And indents too... And use that in my hx
// In a separate commit from now, yeah.

asm fun sequences() {
    <{ 42 PUSHINT }> PUSHCONT

    <{ 42 PUSHINT }>CONT PUSHCONT
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
    /* comments */

    // string
    "  YAYAYA  " DEBUGSTR

    // hex bitstring
    x{DEADBEEF_} PUSHSLICE
    x{babecafe}  SLICE
    x{}          SLICE

    // binary bitstring
    b{}  PUSHSLICE
    b{1} SLICE

    // BoC in hex
    B{DEADBEEF_} B>boc PUSHSLICE
    B{babecafe}  B>boc SLICE
    B{}          B>boc SLICE
    <b b> PUSHREF

    // Control register
    c0 PUSHCTR
    c15 PUSH

    // Stack register
    s0 PUSH
    s15 POP
    16 s() PUSH

    // Number
    0 PUSHINT
    500 INT
    -42 INT
}

