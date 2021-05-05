use crate::HashMap;

#[derive(Debug, Clone)]
pub struct State {
    pub var_map:HashMap<String, Constant>,
    pub func_map:HashMap<String, Function>,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Assignment(Option<VarType>, String, Expr),
    ArrayDef(VarType, String, Expr, Expr), //type, name, value expression, size expression
    If(BoolAst, Vec<Box<AstNode>>),
    While(BoolAst, Vec<Box<AstNode>>),
    BuiltIn(BuiltIn),
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
    Array(VarType, Vec<Constant>),
    ArrayIndex(String, Box<Expr>) // string for variable name, once retrieved the usize will get the constant value
}

/*
* Objects can be constants values, or variable names that turn into constants, or function calls
*/
#[derive(Debug, Clone)]
pub enum Object {
    Variable(String),
    Constant(Constant),
    FuncCall(FuncCall),
    ArrayObj(VarType, Vec<Expr>)
}

/*
An array needs to be used in a couple of different places

First:
    It needs to be able to defined in one of two ways 
        [0; 5] brackets around a [value, length]
        [i; 5] or [i*2; 5]

    Usually assignment is a var_type = expression
    I need to add a seperate type where assignment of an array = array_definition
    
    So instead of assignment, I will deam this array_definition and it will take

    type[] arr = [equation with variable i being the index of array; size];

Second:
    I want to be able to update the values of an array
        arr[expr] = new_value; 

Third:
    I want to be able to access the values of an array in an expression
        int a = arr[0];
*/

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