struct Basic {
    prop: Int;
}

extends fun c(self: Int, ) {
//            ^ variable.parameter
//                     ^ punctuation.delimiter

    /* call_expression */
    95.toString();
    // ^ function

    /* call_expression and */
    a  .asCell()!!.toString();
    // <- variable
    // ^ punctuation.delimiter
    //  ^ function
    //        ^ punctuation.bracket
    //         ^ punctuation.bracket
    //          ^ operator
    //            ^ punctuation.delimiter
    //             ^ function
    //                     ^ punctuation.bracket
    //                      ^ punctuation.bracket
    //                       ^ punctuation.delimiter

    /* field_expression */
    context().sender;
    // <- function
    //        ^ property

    /* static_call_expression */
    now();
    // <- function
    now ();
    // <- function

    /* parenthesized_expression */
    ( 10 );
    // ^ number

    /* instance_expression */
    Basic{prop: ton("1"), };
    // <- constructor
    //   ^ punctuation.bracket
    //    ^ property
    //        ^ punctuation.delimiter
    //          ^ function
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

    /* string */
    "// \\ \" \n\r \t\v \b\f \u{0} \u{FFFFFF} \u0000 \xFF";
    // <- string
    //  ^ string.special

    /* self, not as a builtin, but as a parameter */
    self.toString();
    // <- variable.parameter
    //   ^ function
}
