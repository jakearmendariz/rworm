use serde::{Deserialize, Serialize};

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
    IndexAssignment(String, Expr, Expr),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    ExpVal(Object),
    ExpOp(Box<Expr>, OpType, Box<Expr>),
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
    Array(VarType, Vec<Constant>), // Arrays are fixed size in worm, but its easiest to implement with vec
    Index(String, Box<Expr>), // string for variable name, expr will the the key or index (array or hashmap)
    Map(WormMap), // custom type for hashmap
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

/*
* Objects can be constants values, or variable names that turn into constants, or function calls
*/
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Object {
    Variable(String),
    Constant(Constant),
    FuncCall(FuncCall),
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
