use crate::ast::*;
/*
* display traits for the project, each of the worm objects need to be displayed in error messages
*/

impl Expr {
    pub fn expr_to_str(self) -> String {
        match &*&self {
            Expr::Identifier(identifier) => {
                format!("{}", identifier)
            }
            Expr::FnCall {
                name,
                params,
                position: _,
            } => {
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
            Expr::Constant(constant, _) => {
                format!("{}", constant)
            }
            Expr::ListComprehension {
                piped_var,
                value_expr,
                in_expr,
            } => {
                format!("|{:?}| {} in {}", piped_var, value_expr, in_expr)
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
                    Eq => "==",
                    Neq => "!=",
                    Leq => "<=",
                    Geq => ">=",
                    Lt => "<",
                    Gt => ">",
                    And => "and",
                    Or => "or",
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
            VarType::Bool => write!(f, "bool"),
            VarType::String => write!(f, "string"),
            VarType::Generic => write!(f, "generic"),
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
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Char(c) => write!(f, "{}", c),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Array(t, n) => write!(f, "{}[{}]", t, n.len()),
            Constant::Map(key_type, val_type, _) => write!(f, "map<{},{}>{{}}", key_type, val_type),
            Constant::Struct { name, pairs: _ } => write!(f, "struct {}", name),
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
