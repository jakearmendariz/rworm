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
            Expr::Identifier(identifier) => {
                format!("{}", identifier)
            }
            Expr::FnCall{name, params} => {
                let mut result = format!("{}(", name);
                let mut first = true;
                for expr in params {
                    if !first {
                        result.push_str(", ");
                    }
                    first = false;
                    result.push_str(&format!("{}", expr)[..]);
                }
                format!("{})", result)
            }
            Expr::Constant(constant) => {
                format!("{}", constant)
            }
            Expr::BinaryExpr(exp1, op, exp2) => {
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
            VarType::String => write!(f, "string"),
            VarType::Array(vtype) => write!(f, "{}[]", vtype),
            VarType::Map(key_type, val_type) => write!(f, "map<{},{}>", key_type, val_type),
            VarType::Struct(_) => write!(f, "worm struct"),
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Char(c) => write!(f, "{}", c),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Array(t, n) => write!(f, "{}[{}]", t, n.len()),
            Constant::Map(key_type, val_type, _) => write!(f, "map<{},{}>{{}}", key_type, val_type),
            Constant::Struct(s) => write!(f, "struct {}", s.name),
        }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut string = self.var_name.to_string();
        for sub in &self.tail {
            match sub {
                IdentifierHelper::ArrayIndex(expr) => {
                    string = format!("{}[{}]", string, expr);
                }
                IdentifierHelper::StructIndex(expr) => {
                    string = format!("{}.{}", string, expr);
                }
            }
        }
        write!(f, "{}", string)
    }
}
