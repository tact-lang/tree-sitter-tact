message Empty {}
// <- keyword
//      ^ type
//            ^ punctuation.bracket
//             ^ punctuation.bracket

message(0x42) WithValue {}
// <- keyword
//     ^ punctuation.bracket
//       ^ number
//          ^ punctuation.bracket
//            ^ type
//                      ^ punctuation.bracket
//                       ^ punctuation.bracket

message((crc32("Tact") + 42) & 0xFFFF_FFFF) WithComptimeValue {}
// <- keyword
//     ^ punctuation.bracket
//      ^ punctuation.bracket
//       ^ function
//            ^ punctuation.bracket
//             ^ string
//                   ^ punctuation.bracket
//                     ^ operator
//                       ^ number
//                         ^ punctuation.bracket
//                           ^ operator
//                             ^ number
//                                        ^ punctuation.bracket
