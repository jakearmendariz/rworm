use crate::HashMap;

#[derive(Debug, Clone)]
pub struct State {
    pub var_map:HashMap<String, Constant>,
    pub func_map:HashMap<String, Function>,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Assignment(Option<VarType>, String, Expr),
    If(BoolAst, Vec<Box<AstNode>>),
    While(BoolAst, Vec<Box<AstNode>>),
    BuiltIn(BuiltIn),
    FuncDef(Function),
    ReturnStm(Expr),
    Skip(),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name:String,
    pub params:Vec<(VarType, String)>,
    pub return_type:VarType,
    pub statements:Vec<Box<AstNode>>,
}

#[derive(Debug, Clone)]
pub enum BoolAst {
    Not(Box<BoolAst>),
    And(Box<BoolAst>, Box<BoolAst>),
    Or(Box<BoolAst>, Box<BoolAst>),
    Exp(BoolExp),
    Const(bool)
}

#[derive(Debug, Clone)]
pub enum BuiltIn {
    Delete(String),
    Print(Expr)
}

#[derive(Debug, Clone)]
pub struct BoolExp(pub Expr, pub BoolOp, pub Expr);

#[derive(Debug, Clone)]
pub enum BoolOp {
    Eq,
    Neq,
    Geq,
    Leq,
    Lt,
    Gt
}

#[derive(Debug, Clone)]
pub enum Expr {
    ExpVal(Object),
    ExpOp(Box<Expr>, OpType, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum VarType {
    Int,
    Float,
    String,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name:String,
    pub params: Vec<Expr>
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
    Float(f64),
    String(String),
}

/*
* Objects can be constants values, or variable names that turn into constants, or function calls
*/
#[derive(Debug, Clone)]
pub enum Object {
  Variable(String),
  Constant(Constant),
  FuncCall(FuncCall),
}

#[derive(Debug, Clone)]
pub struct Operation {
    op:OpType,
    left:Box<Expr>,
    right:Box<Expr>
}

#[derive(Debug, Clone)]
pub enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow
}