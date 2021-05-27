use crate::HashMap;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    pub var_map: HashMap<String, Constant>,
    pub func_map: HashMap<String, Function>,
    pub var_stack: Vec<(String, u32)>,
    pub stack_lvl: u32,
}

impl State {
    // increase stack level
    pub fn increment_stack_level(&mut self) {
        self.stack_lvl += 1;
    }

    /* pops all variables off the stack that are on a lower level of the stack */
    pub fn pop_stack(&mut self) {
        self.stack_lvl -= 1;
        if self.var_stack.len() == 0 {
            return;
        }
        let mut last_pos = self.var_stack.len() - 1;
        // loop, deleting the variables that are out of scope of the current level
        loop {
            if self.var_stack[last_pos].1 > self.stack_lvl {
                self.var_map.remove(&self.var_stack[last_pos].0);
                self.var_stack.remove(last_pos);
                if last_pos == 0 {
                    break;
                }
                last_pos -= 1;
            } else {
                break;
            }
        }
    }

    // save variable to stack
    pub fn save_variable(&mut self, var_name: String, value: Constant) {
        match self.var_map.get(&var_name) {
            Some(_) => (), // variable was already inserted
            None => self.var_stack.push((var_name.clone(), self.stack_lvl)),
        }
        self.var_map.insert(var_name, value);
    }

    pub fn _print_stack(&mut self) {
        println!(
            "lvl: {} & variable stack: {:?}",
            self.stack_lvl, self.var_stack
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AstNode {
    // option(type) var_name = expression
    Assignment(Option<VarType>, String, Expr),
    //type, name, optional piped variable for index, value expression, size expression
    // int[] arr = [|opt| expr1; expr2]
    ArrayDef(VarType, String, Option<String>, Expr, Expr),
    // int[] arr = function(that returns an array)
    ArrayFromExp(VarType, String, Expr),
    // arr[exp1] = exp2;
    ArrayIndexAssignment(String, Expr, Expr),
    // if bool then do ast
    If(Vec<(BoolAst, Vec<Box<AstNode>>)>),
    While(BoolAst, Vec<Box<AstNode>>),
    BuiltIn(BuiltIn),
    ReturnStm(Expr),
    Skip(),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub params: Vec<(VarType, String)>,
    pub return_type: VarType,
    pub statements: Vec<Box<AstNode>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BoolAst {
    Not(Box<BoolAst>),
    And(Box<BoolAst>, Box<BoolAst>),
    Or(Box<BoolAst>, Box<BoolAst>),
    Exp(BoolExp),
    Const(bool),
}

impl std::fmt::Display for BoolAst {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BoolAst::*;
        match &self {
            Not(a) => write!(f, "{}", a),
            And(a, b) => write!(f, "{} & {} ", a, b),
            Or(a, b) => write!(f, "{} | {}", a, b),
            Exp(a) => write!(f, "{}", a),
            Const(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BuiltIn {
    Print(Expr),
    Assert(BoolAst),
    StaticPrint(Expr),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoolExp(pub Expr, pub BoolOp, pub Expr);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BoolOp {
    Eq,
    Neq,
    Geq,
    Leq,
    Lt,
    Gt,
}

impl BoolOp {
    pub fn as_str(self) -> &'static str {
        match &self {
            BoolOp::Eq => "==",
            BoolOp::Neq => "!=",
            BoolOp::Leq => "<=",
            BoolOp::Geq => ">=",
            BoolOp::Lt => "<",
            BoolOp::Gt => ">",
        }
    }
}

impl std::fmt::Display for BoolExp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} {} {}", self.0, self.1.clone().as_str(), self.2)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    ExpVal(Object),
    ExpOp(Box<Expr>, OpType, Box<Expr>),
}

impl Expr {
    pub fn expr_to_str(self) -> String {
        match &*&self {
            Expr::ExpVal(object) => {
                format!("{}", object)
            }
            Expr::ExpOp(exp1, op, exp2) => {
                let p1 = exp1.clone().expr_to_str();
                let p2 = exp2.clone().expr_to_str();
                use OpType::*;
                let op = match op {
                    Add => "+",
                    Mult => "*",
                    Sub => "-",
                    Div => "/",
                    Pow => "^",
                    Modulus => "%",
                };
                format!("{}{}{}", p1, op, p2)
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.clone().expr_to_str())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VarType {
    Int,
    Float,
    Char,
    String,
    Array(Box<VarType>),
    Map,
}

impl std::fmt::Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            VarType::Int => write!(f, "int"),
            VarType::Char => write!(f, "char"),
            VarType::Float => write!(f, "float"),
            VarType::String => write!(f, "string"),
            VarType::Array(vtype) => write!(f, "{}[]", vtype),
            VarType::Map => write!(f, "map"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuncCall {
    pub name: String,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constant {
    Int(i32),
    Float(f64),
    String(String),
    Char(char),
    Array(VarType, Vec<Constant>),
    ArrayIndex(String, Box<Expr>), // string for variable name, once retrieved the usize will get the constant value
    Map(WormMap),
}

/*
* hashmap implementation is literally linear
* I wanted to be able to nest hashmaps in this version I can
*/
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct WormMap {
    pairs: Vec<(Constant, Constant)>,
}

impl WormMap {
    pub fn get(self, key: Constant) -> Option<Constant> {
        for (stored_key, stored_val) in &self.pairs {
            if *stored_key == key {
                return Some(stored_val.clone());
            }
        }
        return None;
    }

    pub fn insert(&mut self, key: Constant, value: Constant) {
        self.remove(key.clone());
        self.pairs.push((key, value));
    }

    pub fn remove(&mut self, key: Constant) {
        let mut index = 0;
        for (stored_key, _stored_val) in &self.pairs {
            if *stored_key == key {
                self.pairs.remove(index);
                return;
            }
            index += 1;
        }
        return;
    }
}

// Checks equality amon the constants
impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        use Constant::*;
        match (self, other) {
            (Int(i), Int(j)) => i == j,
            (Float(i), Float(j)) => i == j,
            (Char(i), Char(j)) => i == j,
            (String(i), String(j)) => i.eq(j),
            _ => false,
        }
    }
}

// impl Eq for Constant {}

// Checks equality amon the constants
impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Constant::*;
        Some(match (self, other) {
            (Int(i), Int(j)) => i.cmp(j),
            (Float(i), Float(j)) => (*i as i64).cmp(&(*j as i64)),
            (Char(i), Char(j)) => i.cmp(j),
            (String(i), String(j)) => i.cmp(j),
            _ => return None,
        })
    }
}

// #[derive(Debug, Clone, Serialize, Deserialize)]
// pub enum Constant {

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(fl) => write!(f, "{}", fl),
            Constant::Char(c) => write!(f, "{}", c),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Array(t, n) => write!(f, "{}[{}]", t, n.len()),
            Constant::ArrayIndex(a, i) => write!(f, "{}[{}]", a, *i),
            Constant::Map(_) => write!(f, "map{{}}"),
        }
    }
}

/*
* Objects can be constants values, or variable names that turn into constants, or function calls
*/
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Object {
    Variable(String),
    Constant(Constant),
    FuncCall(FuncCall),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            Object::Variable(x) => write!(f, "{}", x),
            Object::Constant(c) => write!(f, "{}", c),
            Object::FuncCall(func_call) => write!(f, "{}()", func_call.name),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operation {
    op: OpType,
    left: Box<Expr>,
    right: Box<Expr>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow,
    Modulus,
}
