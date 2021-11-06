// establishes ordering for constants but be handled explicitily
// use crate::ast::*;
// use std::cmp::Ordering;

// // Checks equality amon the constants
// impl PartialEq for Constant {
//     fn eq(&self, other: &Self) -> bool {
//         use Constant::*;
//         match (self, other) {
//             (Int(i), Int(j)) => i == j,
//             (Char(i), Char(j)) => i == j,
//             (String(i), String(j)) => i.eq(j),
//             _ => false,
//         }
//     }
// }

// // Checks equality amon the constants
// impl PartialOrd for Constant {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         use Constant::*;
//         Some(match (self, other) {
//             (Int(i), Int(j)) => i.cmp(j),
//             (Char(i), Char(j)) => i.cmp(j),
//             (String(i), String(j)) => i.cmp(j),
//             _ => return None,
//         })
//     }
// }