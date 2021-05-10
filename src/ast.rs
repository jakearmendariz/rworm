use crate::HashMap;

#[derive(Debug, Clone)]
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
    pub fn save_variable(&mut self, var_name:String, value:Constant) {
        match self.var_map.get(&var_name) {
            Some(_) => self.var_stack.push((var_name.clone(), self.stack_lvl)),
            None => (), // variable was already inserted
        }
        self.var_map.insert(var_name, value);
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(VarType, String)>,
    pub return_type: VarType,
    pub statements: Vec<Box<AstNode>>,
}

#[derive(Debug, Clone)]
pub enum BoolAst {
    Not(Box<BoolAst>),
    And(Box<BoolAst>, Box<BoolAst>),
    Or(Box<BoolAst>, Box<BoolAst>),
    Exp(BoolExp),
    Const(bool),
}

#[derive(Debug, Clone)]
pub enum BuiltIn {
    Print(Expr),
    Assert(BoolAst),
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
    Gt,
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
    pub name: String,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i32),
    Float(f64),
    String(String),
    Array(VarType, Vec<Constant>),
    ArrayIndex(String, Box<Expr>), // string for variable name, once retrieved the usize will get the constant value
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
    op: OpType,
    left: Box<Expr>,
    right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum OpType {
    Add,
    Mult,
    Sub,
    Div,
    Pow,
}
