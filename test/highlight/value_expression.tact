struct Basic {
    prop: Int;
}

extends fun c(self: Int, ) {
//            ^ variable.parameter
//                     ^ punctuation.delimiter

    /* call_expression */
    95.toString();
    // ^ function.call

    /* call_expression and */
    a  .asCell()!!.toString();
    // <- variable
    // ^ punctuation.delimiter
    //  ^ function.call
    //        ^ punctuation.bracket
    //         ^ punctuation.bracket
    //          ^ operator
    //            ^ punctuation.delimiter
    //             ^ function.call
    //                     ^ punctuation.bracket
    //                      ^ punctuation.bracket
    //                       ^ punctuation.delimiter

    /* field_expression */
    context().sender;
    // <- function.call
    //        ^ property

    /* static_call_expression */
    now();
    // <- function.call
    now ();
    // <- function.call

    /* parenthesized_expression */
    ( 10 );
    // ^ number

    /* map */
    map<> {};
    // <- type.builtin
    // ^^ punctuation.bracket
    //    ^^ punctuation.bracket
    //      ^ punctuation.delimiter
    map<Int, Int,> {};
    // ^ punctuation.bracket
    //  ^^^ type.builtin
    //     ^ punctuation.delimiter
    //       ^^^ type.builtin
    //          ^ punctuation.delimiter
    //           ^ punctuation.bracket
    map<Address, Int as uint8> {
        //       ^^^ type.builtin
        //           ^^ keyword
        //              ^^^^^ type.builtin
        //                   ^ punctuation.bracket
        address("yo"): 15,
        // <- function.call
        //     ^ punctuation.bracket
        //      ^^^^ string
        //          ^ punctuation.bracket
        //           ^ punctuation.delimiter
        //             ^^ number
        //               ^ punctuation.delimiter
        address("yo2"): 16,
        // <- function.call
        //     ^ punctuation.bracket
        //      ^^^^^ string
        //           ^ punctuation.bracket
        //            ^ punctuation.delimiter
        //              ^^ number
        //                ^ punctuation.delimiter
        };
    //  ^ punctuation.bracket
    //   ^ punctuation.delimiter

    /* set */
    set<> {};
    // <- type.builtin
    // ^^ punctuation.bracket
    //    ^^ punctuation.bracket
    //      ^ punctuation.delimiter
    set<Int,> {
        // <- type.builtin
        // ^ punctuation.delimiter
        //  ^ punctuation.bracket
        //    ^ punctuation.bracket
        10 ,
        // <- number
        // ^ punctuation.delimiter
        20 ,
        // <- number
        // ^ punctuation.delimiter
        100,
        // <- number
        // ^ punctuation.delimiter
        ascii("tact"),
        // <- function.call
        //   ^ punctuation.bracket
        //    ^^^^^^ string
        //          ^ punctuation.bracket
        //           ^ punctuation.delimiter
        };
    //  ^ punctuation.bracket
    //   ^ punctuation.delimiter

    /* instance_expression */
    Basic{prop: ton("1"), };
    // <- constructor
    //   ^ punctuation.bracket
    //    ^ property
    //        ^ punctuation.delimiter
    //          ^ function.call
    //             ^ punctuation.bracket
    //              ^ string
    //                 ^ punctuation.bracket
    //                  ^ punctuation.delimiter
    //                    ^ punctuation.bracket
    //                     ^ punctuation.delimiter

    /* punned instance_expression */
    Basic{prop, };
    // <- constructor
    //   ^ punctuation.bracket
    //    ^ property
    //        ^ punctuation.delimiter
    //          ^ punctuation.bracket
    //           ^ punctuation.delimiter

    /* integer */
    0b101;
    // <- number
    //   ^ punctuation.delimiter
    0xf0f;
    // <- number
    0o707;
    // <- number
    909;
    // <- number

    /* boolean */
    true;
    // <- constant.builtin
    //  ^ punctuation.delimiter
    false;
    // <- constant.builtin

    /* identifier */
    name;
    // <- variable
    //  ^ punctuation.delimiter

    /* null */
    null;
    // <- constant.builtin
    //  ^ punctuation.delimiter

    /* initOf */
    initOf Contract(123, 123, );
    // <- keyword
    //     ^ constructor
    //                      ^ punctuation.delimiter
    //                        ^ punctuation.bracket
    //                         ^ punctuation.delimiter

    /* codeOf */
    codeOf Contract;
    // <- keyword
    //     ^ constructor

    /* string */
    "// \\ \" \n\r \t\v \b\f \u{0} \u{FFFFFF} \u0000 \xFF";
    // <- string
    //  ^ string.special

    /* self, not as a builtin, but as a parameter */
    self.toString();
    // <- variable.parameter
    //   ^ function.call
}
