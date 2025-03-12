contract Empty {}
// <- keyword
//       ^ variable
//             ^ punctuation.bracket
//              ^ punctuation.bracket

contract Filled {
    init() {}
    // <- function
    //  ^ punctuation.bracket
    //   ^ punctuation.bracket
    //     ^ punctuation.bracket
    //      ^ punctuation.bracket

    receive() {}
    // <- function

    external() {}
    // <- function

    bounced(msg: Slice) {}
    // <- function
    //      ^ variable.parameter
    //         ^ punctuation.delimiter
    //           ^ type.builtin
}

contract NoLazy(
    //         ^ punctuation.bracket
    foo: Int as int16,
    // <- variable.parameter
    // ^ punctuation.delimiter
    //   ^^^ type.builtin
    //       ^^ keyword
    //          ^^^^^ type.builtin
    //               ^ punctuation.delimiter
)  {
// <- punctuation.bracket
// ^ punctuation.bracket
    get fun getFoo(): Int {
        return self.foo;
    }
}
