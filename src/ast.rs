
#[derive(Debug)]
pub enum AstNode {
    Print(String),
    Assignment(Option<VarType>, String, Expr)
}

#[derive(Debug)]
pub enum Expr {
    ExpVal(Value),
    ExpOp(Box<Expr>, OpType, Box<Expr>)
}

#[derive(Debug)]
pub enum VarType {
    Int,
    Float,
    String
}

#[derive(Debug)]
pub enum Value {
  Variable(String),
  Number(f64)  
}

#[derive(Debug)]
pub struct Operation {
    op:OpType,
    left:Box<Expr>,
    right:Box<Expr>
}

#[derive(Debug)]
pub enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow
}