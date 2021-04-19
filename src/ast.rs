
#[derive(Debug, Clone)]
pub enum AstNode {
    Print(String),
    Assignment(Option<VarType>, String, Expr),
    If(BoolAst, Vec<Box<AstNode>>),
    While(BoolAst, Vec<Box<AstNode>>),
    BuiltIn(BuiltIn),
    FuncDef(Function),
    Skip(),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name:String,
    pub params:Vec<(VarType, String)>,
    pub return_type:VarType,
    pub statements:Vec<Box<AstNode>>,
    pub return_stm:Expr
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
    Sum()
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
    ExpOp(Box<Expr>, OpType, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum VarType {
    Int,
    Float,
    String
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub name:String,
    pub params: Vec<Expr>
}

#[derive(Debug, Clone)]
pub enum Value {
  Variable(String),
  Number(f64),
  FuncCall(FuncCall)
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