extends fun c(self: SomeMessage): Int {
// <- keyword
//      ^ keyword
//          ^ function
//           ^ punctuation.bracket
//            ^ variable.parameter
//                ^ punctuation.delimiter
//                  ^ type
//                             ^ punctuation.bracket
//                              ^ punctuation.delimiter
//                                ^ type.builtin
//                                    ^ punctuation.bracket
    let a: Int = 0;
    // <- keyword
    //  ^ variable
    //   ^ punctuation.delimiter
    //     ^ type.builtin
    //         ^ operator
    //           ^ number
    //            ^ punctuation.delimiter

    {  }
    // <- punctuation.bracket
    // ^ punctuation.bracket
    
    {
        let b: Int = 20;
        // <- keyword
        //  ^ variable
        //   ^ punctuation.delimiter
        //     ^ type.builtin
        //         ^ operator
        //           ^ number
        //             ^ punctuation.delimiter
    }

    self.description = "Tact is awesome!";
    // <- variable.parameter
    //  ^ punctuation.delimiter
    //   ^ property
    //               ^ operator
    //                 ^ string
    //                      ^ string
    //                                   ^ punctuation.delimiter

    self.description.description = "It really is!";
    // <- variable.parameter
    //   ^ property
    //               ^ property

    a  = 15;
    // <- variable
    // ^ operator
    //   ^ number
    //     ^ punctuation.delimiter

    a += 32;
    // <- variable
    // ^ operator

    if (true) {}
    // <- keyword
    // ^ punctuation.bracket
    //  ^ constant.builtin
    //      ^ punctuation.bracket
    //        ^ punctuation.bracket
    //         ^ punctuation.bracket
    
    if (false) {}
    //  ^ constant.builtin
    else {}
    // <- keyword
    //   ^ punctuation.bracket
    //    ^ punctuation.bracket

    if (false) {}
    else if (true) {}
    //   ^ keyword
    //      ^ punctuation.bracket
    //       ^ constant.builtin
    //           ^ punctuation.bracket
    //             ^ punctuation.bracket
    //              ^ punctuation.bracket

    while (false) {}
    // <- keyword
    //    ^ punctuation.bracket
    //     ^ constant.builtin
    //          ^ punctuation.bracket
    //            ^ punctuation.bracket
    //             ^ punctuation.bracket

    repeat (5) {}
    // <- keyword
    //     ^ punctuation.bracket
    //      ^ number
    //       ^ punctuation.bracket
    //         ^ punctuation.bracket
    //          ^ punctuation.bracket

    do {}
    // <- keyword
    // ^ punctuation.bracket
    //  ^ punctuation.bracket
    until (false);
    // <- keyword
    //    ^ punctuation.bracket
    //     ^ constant.builtin
    //          ^ punctuation.bracket
    //           ^ punctuation.delimiter

    0  ;
    // <- number
    // ^ punctuation.delimiter

    return 0;
    // <- keyword
    //     ^ number
    //      ^ punctuation.delimiter
}
// <- punctuation.bracket
