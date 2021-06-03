use crate::ast::*;
/*
* display traits for the project, each of the worm objects need to be displayed in error messages
*/

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

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(fl) => write!(f, "{}", fl),
            Constant::Char(c) => write!(f, "{}", c),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Array(t, n) => write!(f, "{}[{}]", t, n.len()),
            Constant::Index(a, i) => write!(f, "{}[{}]", a, *i),
            Constant::Map(_) => write!(f, "map{{}}"),
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            Object::Variable(x) => write!(f, "{}", x),
            Object::Constant(c) => write!(f, "{}", c),
            Object::FnCall(func_call) => {
                let mut result = format!("{}(", func_call.name);
                let mut first = true;
                for expr in &func_call.params {
                    if !first {
                        result.push_str(", ");
                    }
                    first = false;
                    result.push_str(&format!("{}", expr)[..]);
                }   
                write!(f, "{})", result)
            },
        }
    }
}