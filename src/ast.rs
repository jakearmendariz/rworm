// use std::collections::Hash, PartialEq, PartialOrd, EqMap;

/*
* ast.rs
* contains the ast features necessary in this program
*/
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/**
 * Weird Idea
 * Maybe instead of expressions be first class nodes, it should be ast and return statements
 * and expression is just another variant
 * 
 * this way I could write
 * int y = some_function();
 * int x = if y == value {
 *      int x = compute on y
 *      return x
 * } else {
 *      while y < value {
 *          y += 1;
 *      }
 *      return y;
 * }
 * 
 * 
 * Another idea is simplifying boolean expressions where isntead of 
 * x == 1 | x == 2 | x == 3
 * I could do
 * x == (1 | 2 | 3)
 * x in (1..3)
 */

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub enum AstNode {
    // option(type) var_name = expression
    Assignment {
        var_type: Option<VarType>,
        identifier: Identifier,
        expr: Expr,
    },
    // if bool then do ast
    If(Vec<(Expr, Vec<Box<AstNode>>)>),
    While(Expr, Vec<Box<AstNode>>),
    // these built in functions consume an entire line
    BuiltIn(BuiltIn),
    // return from a function
    ReturnStm(Expr),
    // Makes life easy when designing ast to have a skip value
    Skip(),
}

/**
* Goal is to have a structure that can symbolize
* the following variants
* x
* x[0]
* x[0][0]
* x.y
* x.y[0]
* x.y[0].z
* ... etc
*/
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum IdentifierHelper {
    ArrayIndex(Expr),
    StructIndex(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Identifier {
    pub var_name: String,
    pub tail: Vec<IdentifierHelper>,
    pub position: usize,
}

pub const APPEND: &str = "append";
pub const PREPEND: &str = "prepend";
pub const REMOVE: &str = "remove";
pub const TO_STR: &str = "to_str";
pub const USER_INPUT: &str = "user_input";
pub const PARSE_INT: &str = "parse_int";
pub const LEN: &str = "len";

use std::collections::HashSet;
lazy_static::lazy_static! {
    pub static ref RESERVED_FUNCTIONS: HashSet<&'static str> = {
        let mut reserved: HashSet<&'static str> = HashSet::with_capacity(10);
        reserved.insert(APPEND);
        reserved.insert(PREPEND);
        reserved.insert(REMOVE);
        reserved.insert(TO_STR);
        reserved.insert(USER_INPUT);
        reserved.insert(PARSE_INT);
        reserved.insert(LEN);
        reserved
    };
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub struct Function {
    pub name: String,
    pub params: Vec<(VarType, String)>,
    pub return_type: VarType,
    pub statements: Vec<Box<AstNode>>,
    pub position: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct FnCall {
    pub name: String,
    pub params: Vec<Expr>,
    pub position: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub enum BuiltIn {
    Print(Expr),
    Assert(Expr),
    StaticPrint(Expr),
}

/// Expression is core of ast, all values are created from expressions
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Expr {
    Identifier(Identifier),
    Literal(Literal, usize),
    FnCall {
        name: String,
        params: Vec<Expr>,
        position: usize,
    },
    // current:
    //  [|i| i; 10] => [0,1,2,3,4,5,6,7,8,9]
    //  [int] => [] # this stays
    // desired: # any of the following
    //  [0..10]
    //  [i for i in range(0..10)]
    //  [0,1,2,3,4,5,6,7,8,9]
    //  [i for value in array]
    ListComprehension {
        piped_var: Option<String>,
        value_expr: Box<Expr>,
        in_expr: Box<Expr>,
    },
    BinaryExpr(Box<Expr>, OpType, Box<Expr>),
    UnaryExpr(UnaryOp, Box<Expr>),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow,
    Modulus,
    /// Boolean Operation
    Eq,
    Neq,
    Geq,
    Leq,
    Lt,
    Gt,
    And,
    Or,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum UnaryOp {
    Not,
}

/// All expressions evaluate to a specific type, which is represented as a Literal
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialOrd, Eq, Ord)]
pub enum VarType {
    Int,
    Char,
    Bool,
    String,
    Generic, // Not used in code yet, but needed for analysis
    Array(Box<VarType>),
    Map(Box<VarType>, Box<VarType>),
    Struct(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, Ord)]
pub enum Literal {
    Int(i32),
    Bool(bool),
    String(String),
    Char(char),
    Array(VarType, Vec<Literal>), // Arrays are fixed size in worm, but its easiest to implement with vec
    Map(VarType, VarType, BTreeMap<Literal, Literal>),
    Struct {
        name: String,
        pairs: BTreeMap<String, Literal>,
    },
}

use std::cmp::Ordering;

impl PartialEq for VarType {
    fn eq(&self, other: &VarType) -> bool {
        use VarType::*;
        match (self, other) {
            (Int, Int) | (String, String) | (Char, Char) => true,
            (Int, Char) => true, // allow int => char conversion
            (Map(k1, v1), Map(k2, v2)) => k1.eq(&k2) && v1.eq(v2),
            (Array(arr1), Array(arr2)) => arr1.eq(arr2),
            _ => false,
        }
    }
}

// Checks equality amon the Literals
impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        use Literal::*;
        match (self, other) {
            (Int(i), Int(j)) => i == j,
            (Char(i), Char(j)) => i == j,
            (String(i), String(j)) => i.eq(j),
            (Bool(a), Bool(b)) => a == b,
            _ => false,
        }
    }
}

// Checks equality amon the Literals
impl PartialOrd for Literal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Literal::*;
        Some(match (self, other) {
            (Int(i), Int(j)) => i.cmp(j),
            (Char(i), Char(j)) => i.cmp(j),
            (String(i), String(j)) => i.cmp(j),
            (Bool(a), Bool(b)) => a.cmp(b),
            _ => return None,
        })
    }
}
