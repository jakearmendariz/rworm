// use std::collections::Hash, PartialEq, PartialOrd, EqMap;

/*
* ast.rs
* contains the ast features necessary in this program
*/
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub enum AstNode {
    // option(type) var_name = expression
    Assignment(Option<VarType>, Identifier, Expr),
    //type, name, optional piped variable for index, value expression, size expression
    // int[] arr = [|opt| expr1; expr2]
    ArrayDef(VarType, String, Option<String>, Expr, Expr),
    // if bool then do ast
    If(Vec<(BoolAst, Vec<Box<AstNode>>)>),
    While(BoolAst, Vec<Box<AstNode>>),
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
    StructIndex(String)
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Identifier {
    pub var_name: String,
    pub tail: Vec<IdentifierHelper>,
}

pub const APPEND: &str = "append";
pub const TO_STR: &str = "to_str";
pub const USER_INPUT: &str = "user_input";
pub const PARSE_INT: &str = "parse_int";
pub const LEN: &str = "len";

use std::collections::HashSet;
lazy_static::lazy_static! {
    pub static ref RESERVED_FUNCTIONS: HashSet<&'static str> = {
        let mut reserved: HashSet<&'static str> = HashSet::with_capacity(10);
        reserved.insert(APPEND);
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
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct FnCall {
    pub name: String,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub enum BoolAst {
    Not(Box<BoolAst>),
    And(Box<BoolAst>, Box<BoolAst>),
    Or(Box<BoolAst>, Box<BoolAst>),
    Exp(BoolExp),
    Const(bool),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub enum BuiltIn {
    Print(Expr),
    Assert(BoolAst),
    StaticPrint(Expr),
}

// boolean expressions for conditional statements
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub struct BoolExp(pub Expr, pub BoolOp, pub Expr);

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq)]
pub enum BoolOp {
    Eq,
    Neq,
    Geq,
    Leq,
    Lt,
    Gt,
}

// expression represents 
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Expr {
    ExpVal(Object),
    ExpOp(Box<Expr>, OpType, Box<Expr>),
}

// objects abstract away the constant to allow for variables and function calls
// helpful in expressions
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum Object {
    Identifier(Identifier),
    Constant(Constant),
    FnCall(FnCall),
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct Operation {
    op: OpType,
    left: Box<Expr>,
    right: Box<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow,
    Modulus,
}

// vartype and constants are the core of the language
// all expressions evaluate to a specific type, which is represented as a constant
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialOrd, Eq, Ord)]
pub enum VarType {
    Int,
    Char,
    String,
    Array(Box<VarType>),
    Map(Box<VarType>, Box<VarType>),
    Struct(String),
}

impl PartialEq for VarType {
    fn eq(&self, other:&VarType) -> bool {
        use VarType::*;
        match (self, other) {
            (Int, Int) | (String, String) | (Char, Char) => 
                true,
            (Int, Char) => 
                true, // allow int => char conversion
            (Map(k1, v1), Map(k2, v2)) => 
                k1.eq(&k2) && v1.eq(v2),
            (Array(arr1), Array(arr2)) => 
                arr1.eq(arr2),
            _ => false,
        }
    }
}

// TODO convert Float to be represented as two integers
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, Ord)]
pub enum Constant {
    Int(i32),
    String(String),
    Char(char),
    Array(VarType, Vec<Constant>), // Arrays are fixed size in worm, but its easiest to implement with vec
    Index(String, Box<Expr>), // string for variable name, expr will the the key or index (array or Hash, PartialEq, PartialOrd, Eqmap)
    Map(VarType, VarType, WormMap), // custom type for Hash, PartialEq, PartialOrd, Eqmap
    Struct(WormStruct),
    StructVal(String, String)
}

use std::cmp::Ordering;

// Checks equality amon the constants
impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        use Constant::*;
        match (self, other) {
            (Int(i), Int(j)) => i == j,
            (Char(i), Char(j)) => i == j,
            (String(i), String(j)) => i.eq(j),
            _ => false,
        }
    }
}

// Checks equality amon the constants
impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Constant::*;
        Some(match (self, other) {
            (Int(i), Int(j)) => i.cmp(j),
            (Char(i), Char(j)) => i.cmp(j),
            (String(i), String(j)) => i.cmp(j),
            _ => return None,
        })
    }
}

/*
* Hash, PartialEq, PartialOrd, Eqmap implementation is literally linear
* I wanted to be able to nest Hash, PartialEq, PartialOrd, Eqmaps in this version I can
* Its trash and would need to be replaced (if I had more time)
* NOTE if I convert float to two integers instead, default Hash, PartialEq, PartialOrd, Eqmap implementation works
*/
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialOrd, Eq, Ord, PartialEq, Hash)]
pub struct WormMap {
    pairs: BTreeMap<Constant, Constant>,
}

impl WormMap {
    pub fn get(self, key: Constant) -> Option<Constant> {
        Some(self.pairs.get(&key)?.clone())
    }

    pub fn insert(&mut self, key: Constant, value: Constant) {
        self.pairs.insert(key, value);
    }
}


#[derive(Debug, Clone, Serialize, Deserialize, Default, Hash, PartialEq, PartialOrd, Eq, Ord)]
pub struct WormStruct {
    pub name: String,
    pub pairs: BTreeMap<String, Constant>,
}

impl WormStruct {
    pub fn get(self, key: String) -> Option<Constant> {
        Some(self.pairs.get(&key)?.clone())
    }

    pub fn insert(&mut self, key: String, value: Constant) {
        self.pairs.insert(key, value);
    }
}