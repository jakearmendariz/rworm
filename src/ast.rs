
#[derive(Debug, Clone)]
pub enum AstNode {
    Print(String),
    Assignment(Option<VarType>, String, Expr),
    If(BoolExp, Vec<Box<AstNode>>),
    While(BoolExp, Vec<Box<AstNode>>)
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
    ExpVal(Value),
    ExpOp(Box<Expr>, OpType, Box<Expr>)
}

#[derive(Debug, Clone)]
pub enum VarType {
    Int,
    Float,
    String
}

#[derive(Debug, Clone)]
pub enum Value {
  Variable(String),
  Number(f64)  
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