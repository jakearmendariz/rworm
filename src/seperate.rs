/*
* mut.rs
* Program analysis, but, without cloning, this part is still under development,
* but I think this addition could make the program run much more quickly and smoothly
*/

use crate::ast::*;
use crate::state::*;
use colored::*;

#[derive(Debug, Clone)]
pub enum StaticError {
    ValueDne(String),
    TypeViolation(VarType, VarType),
    NeedReturnStm(String),
    CannotFindFunction(String),
    General(String),
    Index(String, String), // array name, type of error
    Count(u16),
}

impl std::fmt::Display for StaticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            StaticError::ValueDne(x) => write!(f, "variable \'{}\' does not exist", x),
            StaticError::TypeViolation(a, b) => {
                write!(f, "expected type \'{}\' recieved type \'{}\'", a, b)
            }
            StaticError::NeedReturnStm(name) => {
                write!(f, "need a return statment in function \'{}\'", name)
            }
            StaticError::CannotFindFunction(name) => {
                write!(f, "function \'{}\' does not exist", name)
            }
            StaticError::General(x) => write!(f, "{}", x),
            StaticError::Index(name, reason) => {
                write!(f, "array index error in array \'{}\' for {}", name, reason)
            }
            StaticError::Count(x) => write!(f, "static analysis caught {} errors", x),
        }
    }
}

impl ExecutionState {
    pub fn check_program(&mut self, state:&State) -> Result<(), StaticError> {
        let mut error = false;
        let mut count = 0;
        // let functions = self.func_map.clone();
        for i  in 0..state.fn_list.len() {
            match self.inspect_function(i, state) {
                Ok(()) => (),
                Err(StaticError::Count(i)) => {
                    count += i;
                    error = true;
                },
                Err(_) => {
                    count += 1;
                    error = true;
                }
            }
        }
        if error {
            Err(StaticError::Count(count))
        } else {
            Ok(())
        }
    }


    // same as check_function, except with a index instead of passing function
    pub fn inspect_function(
        &mut self,
        index:usize,
        state:&State) -> Result<(), StaticError> {
        self.increment_stack_level();
        // save parameters of the function into state
        let fn_name = state.fn_list.get(index).unwrap();
        let function = state.func_map.get(&fn_name.to_string()).unwrap();
        let params = &function.params;
        for (param_type, param_name) in params {
            self.save_variable(param_name.to_string(), default_const(param_type.clone()));
        }
        let mut i = 1;
        let mut errors = 0;
        let mut return_stm = false;
        let expected_rt_type = function.return_type.clone();
        let mut return_type = VarType::Int;
        for ast in &function.statements {
            // execute ast will run a single statement or loop, if there is a return value, exit out of function
            match self.eval_ast(state, (**ast).clone()) {
                Ok(res) => match res {
                    Some(val) => {
                        return_type = val;
                        return_stm = true;
                    }
                    None => (),
                },
                Err(e) => {
                    println!(
                        "{} in function \'{}\' on line {}, error: {}",
                        "STATIC ERROR:".red().bold(),
                        fn_name,
                        i,
                        e
                    );
                    errors += 1;
                }
            }
            i += 1;
        }
        self.pop_stack();
        if !return_stm {
            println!(
                "{} {}",
                "STATIC ERROR:".red().bold(),
                StaticError::NeedReturnStm(fn_name.to_string())
            );
            errors += 1;
        } else {
            if !type_match(return_type.clone(), expected_rt_type.clone()) {
                println!(
                    "{} {}",
                    "STATIC ERROR:".red().bold(),
                    StaticError::TypeViolation(return_type, expected_rt_type)
                );
                errors += 1;
            }
        }
        if errors > 0 {
            Err(StaticError::Count(errors))
        } else {
            Ok(())
        }
    }
    
    fn eval_ast(&mut self, state:&State, ast: AstNode) -> Result<Option<VarType>, StaticError> {
        match ast {
            AstNode::Function(_) => {
                // let return_type = func.return_type.clone();
                // self.inspect_function(func.name.clone(), func)?;
                // return Ok(Some(return_type));
                panic!("I didn't know functions were here, is this an inner function maybe");
            }
            AstNode::Assignment(vtype, name, exp) => {
                let variable_type: VarType = match vtype {
                    Some(var_type) => var_type,
                    None => {
                        // if no variable type, turn it into an expression and parse value (error if dne)
                        self.type_of_expr(state, Expr::ExpVal(Object::Variable(name.clone())))?
                    }
                };
                // type check, variable type must match the result of expression
                let value_type = self.type_of_expr(state, exp)?;
                if type_match(variable_type.clone(), value_type.clone()) {
                    // println!("saving variable:{}", name.clone());
                    self.save_variable(name, default_const(variable_type));
                } else {
                    return Err(StaticError::TypeViolation(variable_type, value_type));
                }
            }
            AstNode::ArrayDef(var_type, name, piped, value_exp, length_exp) => {
                match self.type_of_expr(state, length_exp)? {
                    VarType::Int => (),
                    _ => {
                        return Err(StaticError::General(format!(
                            "length of array:\'{}\' must be int",
                            name
                        )));
                    }
                };
                // elements of the array
                let (variable, pipe) = match piped {
                    Some(piped) => (piped, true),
                    None => (String::from(""), false),
                };
                self.increment_stack_level();
                for i in 0..2 {
                    // not currently type checking need to add that later on
                    if pipe {
                        self.save_variable(variable.clone(), Constant::Int(i as i32));
                    }
                    self.type_of_expr(state, value_exp.clone())?;
                }
                self.pop_stack();
                self.save_variable(name, Constant::Array(var_type, Vec::new()));
            }
            AstNode::IndexAssignment(name, index_exp, value_exp) => {
                // println!("var_map:{:?}", state.var_map);
                let (var_type, _) = match self.get_value(name.clone())? {
                    Constant::Array(var_type, elements) => (var_type, elements),
                    Constant::Map(_) => {
                        return Ok(None); // allow all types inside of the hashmap
                    }
                    _ => {
                        return Err(StaticError::General(format!(
                            "length of array:\'{}\' must be int",
                            name
                        )))
                    }
                };
                match self.type_of_expr(state, index_exp)? {
                    VarType::Int => (),
                    _ => {
                        return Err(StaticError::General(format!(
                            "length of array:\'{}\' must be int",
                            name
                        )))
                    }
                };
                let value_type = self.type_of_expr(state, value_exp)?;
                if !type_match(var_type, value_type) {
                    return Err(StaticError::General(format!(
                        "length of array:\'{}\' must be int",
                        name
                    )));
                };
            }
            AstNode::If(if_pairs) => {
                self.increment_stack_level();
                for (conditional, mut stms) in if_pairs {
                    self.check_bool_ast(state, &conditional)?;
                    while stms.len() > 0 {
                        match self.eval_ast(state, *stms.remove(0))? {
                            Some(eval) => return Ok(Some(eval)), // TODO change this to type check every statement
                            None => (),
                        }
                    }
                }
                self.pop_stack();
            }
            AstNode::While(conditional, stms) => {
                self.check_bool_ast(state, &conditional)?;
                self.increment_stack_level();
                for stm in stms.iter() {
                    match self.eval_ast(state, *stm.clone())? {
                        Some(eval) => return Ok(Some(eval)), //if there was a return statement, return the value
                        None => (),
                    }
                }
                self.pop_stack();
            }
            AstNode::BuiltIn(builtin) => {
                match builtin {
                    BuiltIn::Print(exp) => {
                        self.type_of_expr(state, exp)?;
                    }
                    BuiltIn::StaticPrint(exp) => {
                        println!("static_print: {}", exp.clone());
                        self.type_of_expr(state, exp)?;
                    }
                    BuiltIn::Assert(boolast) => {
                        self.check_bool_ast(state, &boolast)?;
                    }
                }
                ()
            }
            AstNode::ReturnStm(expr) => {
                return Ok(Some(self.type_of_expr(state, expr)?));
            }
            AstNode::Skip() => (),
        }
        Ok(None)
    }

    /*
    * evalulates booleans based on their conjunction
    */
    fn check_bool_ast(&mut self, state:&State ,bool_ast: &BoolAst) -> Result<(), StaticError> {
        match &*bool_ast {
            BoolAst::Not(body) => self.check_bool_ast(state, &*body)?,
            BoolAst::And(a, b) => {
                self.check_bool_ast(state, &*a)?;
                self.check_bool_ast(state, &*b)?;
            }
            BoolAst::Or(a, b) => {
                self.check_bool_ast(state, &*a)?;
                self.check_bool_ast(state, &*b)?
            }
            BoolAst::Exp(exp) => self.check_bool(state, &*exp)?,
            BoolAst::Const(_) => (),
        };
        Ok(())
    }


    /*
    * evaluates expressions and constants to true false values
    */
    fn check_bool(&mut self, state:&State, bool_exp: &BoolExp) -> Result<(), StaticError> {
        let BoolExp(lhs, _, rhs) = &*bool_exp;
        // if
        let right = self.type_of_expr(state, rhs.clone())?;
        let left = self.type_of_expr(state, lhs.clone())?;
        if !type_match(left.clone(), right.clone()) {
            return Err(StaticError::TypeViolation(left, right));
        };
        Ok(())
    }
    /*
    * type_of_expr returns the type provided by an inline expression
    */
    fn type_of_expr(&mut self, state:&State, exp: Expr) -> Result<VarType, StaticError> {
        match exp {
            Expr::ExpVal(num) => {
                match num {
                    Object::Variable(name) => {
                        // get variable as a constant value
                        match self.var_map.get(&name) {
                            Some(value) => Ok(value.clone().get_type(self)?),
                            None => return Err(StaticError::ValueDne(name)),
                        }
                    }
                    Object::Constant(Constant::Array(var_type, _elements)) => {
                        Ok(VarType::Array(Box::new(var_type)))
                    }
                    Object::Constant(Constant::Index(name, index_exp)) => {
                        // get array from state map
                        match self.var_map.get(&name.clone()) {
                            Some(value) => match value.clone() {
                                Constant::Array(var_type, _) => {
                                    match self.type_of_expr(state, *index_exp)? {
                                        VarType::Int => (),
                                        _ => {
                                            return Err(StaticError::Index(
                                                name,
                                                String::from("non int accessing array"),
                                            ))
                                        }
                                    };
                                    Ok(var_type)
                                }
                                Constant::Int(_) => Ok(VarType::Int), // I think this should error out
                                Constant::String(_) => {
                                    match self.type_of_expr(state, *index_exp)? {
                                        VarType::Int => (),
                                        _ => {
                                            return Err(StaticError::Index(
                                                name,
                                                String::from("non int accessing array"),
                                            ))
                                        }
                                    };
                                    Ok(VarType::Char)
                                }
                                Constant::Map(_hashmap) => {
                                    Ok(VarType::Map) // TODO need to add an any type so our analysis accepts these values
                                }
                                _ => Err(StaticError::Index(
                                    name,
                                    String::from("non array value"),
                                )),
                            },
                            None => Err(StaticError::ValueDne(name)),
                        }
                    }
                    Object::Constant(Constant::Float(_)) => Ok(VarType::Float),
                    Object::Constant(Constant::Int(_)) => Ok(VarType::Int),
                    Object::Constant(Constant::Char(_)) => Ok(VarType::Char),
                    Object::Constant(Constant::String(_)) => Ok(VarType::String),
                    Object::Constant(Constant::Map(_)) => Ok(VarType::Map),
                    Object::FnCall(func_call) => {
                        // retrive function from memory, make sure its value matches
                        if func_call.name == "len" {
                            // builtin
                            if func_call.params.len() != 1 {
                                Err(StaticError::General(
                                    "Error len requires exactly 1 arg".to_string(),
                                ))
                            } else {
                                Ok(VarType::Int)
                            }
                        } else if func_call.name == "parse_int" {
                            if func_call.params.len() != 1 {
                                Err(StaticError::General(
                                    "Error parse_int requires exactly 1 arg".to_string(),
                                ))
                            } else {
                                Ok(VarType::Int)
                            }
                        } else if func_call.name == "user_input" {
                            Ok(VarType::String)
                        } else if func_call.name == "to_str" {
                            if func_call.params.len() != 1 {
                                Err(StaticError::General(
                                    "Error to_str requires exactly 1 arg".to_string(),
                                ))
                            } else {
                                Ok(VarType::String)
                            }
                        } else {
                            let function = match state.func_map.get(&func_call.name.clone()) {
                                Some(func) => func,
                                None => return Err(StaticError::CannotFindFunction(func_call.name)),
                            };
                            let Function {
                                name: _,
                                params,
                                return_type,
                                statements: _,
                            } = function.clone();
                            // iterate through the parameters provided and the function def,
                            for (expr, (var_type, _)) in func_call.params.iter().zip(params.iter()) {
                                let param_const = self.type_of_expr(state, expr.clone())?;
                                if !type_match(var_type.clone(), param_const.clone()) {
                                    return {
                                        Err(StaticError::TypeViolation(var_type.clone(), param_const))
                                    };
                                }
                            }
                            // function input types match expected values, return a empty constant of matching type
                            Ok(return_type)
                        }
                    }
                }
            }
            Expr::ExpOp(lhs, _, rhs) => {
                let left = self.type_of_expr(state, *lhs)?;
                let right = self.type_of_expr(state, *rhs)?;
                use VarType::*;
                match (left.clone(), right.clone()) {
                    (Char, Char) => Ok(VarType::String),
                    (Char, String) => Ok(VarType::String),
                    (String, Char) => Ok(VarType::String),
                    (_, _) => {
                        if type_match(left.clone(), right.clone()) {
                            Ok(left)
                        } else {
                            Err(StaticError::TypeViolation(left.clone(), right.clone()))
                        }
                    }
                }
            }
        }
    }
    /*
    * returns a value or that a value does not exist
    */
    fn get_value(&mut self, name: String) -> Result<Constant, StaticError> {
        // println!("get_value `{}`", name.clone());
        // println!("var_map `{:?}`", state.var_map);
        match self.var_map.get(&name.clone()) {
            Some(value) => Ok(value.clone()),
            None => {
                // println!("get_value=>DNE(`{}`)", name.clone());
                Err(StaticError::ValueDne(name))
            }
        }
    }


}

fn type_match(a: VarType, b: VarType) -> bool {
    use VarType::*;
    match (a, b) {
        (Int, Int) | (Float, Float) | (String, String) | (Char, Char) | (Map, Map) => true,
        (Int, Char) => true, // allow int => char conversion
        (Map, _) => true,
        (_, Map) => true,
        (Array(arr1), Array(arr2)) => type_match(*arr1, *arr2),
        _ => false,
    }
}

impl Constant {
    fn get_type(self, exestate: &mut ExecutionState) -> Result<VarType, StaticError> {
        Ok(match self {
            Constant::String(_) => VarType::String,
            Constant::Float(_) => VarType::Float,
            Constant::Int(_) => VarType::Int,
            Constant::Char(_) => VarType::Char,
            Constant::Array(vtype, _) => VarType::Array(Box::new(vtype)),
            Constant::Map(_) => VarType::Map,
            Constant::Index(name, _) => {
                // if it is an array, then retrieve the array from memory, then get its type
                match exestate.get_value(name.clone())? {
                    Constant::Array(var_type, _) => var_type.clone(),
                    Constant::String(_) => VarType::Char,
                    _ => {
                        return Err(StaticError::Index(
                            name,
                            format!("cannot index non array value"),
                        ))
                    }
                }
            }
        })
    }
}

// default constants given a vartype
fn default_const(var_type: VarType) -> Constant {
    match var_type {
        VarType::Int => Constant::Int(0),
        VarType::Float => Constant::Float(0.0),
        VarType::Char => Constant::Char(' '),
        VarType::String => Constant::String(String::from("")),
        VarType::Array(var_type) => Constant::Array(*var_type, Vec::new()),
        VarType::Map => Constant::Map(WormMap::default()),
    }
}
