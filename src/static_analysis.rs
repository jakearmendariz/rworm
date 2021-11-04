/*
* static_analysis.rs
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
    MapKeyTypeViolation(VarType, VarType),
    TypeMismatchInReturn(VarType, VarType),
    CannotFindFunction(String),
    General(String),
    Index(String, String), // array name, type of error
}

impl std::fmt::Display for StaticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            StaticError::ValueDne(x) => write!(f, "variable \'{}\' does not exist", x),
            StaticError::TypeViolation(a, b) =>
                write!(f, "expected type \'{}\' recieved type \'{}\'", a, b),
            StaticError::NeedReturnStm(name) =>
                write!(f, "need a return statment in function \'{}\'", name),
            StaticError::CannotFindFunction(name) =>
                write!(f, "function \'{}\' does not exist", name),
            StaticError::General(x) => write!(f, "{}", x),
            StaticError::Index(name, reason) => 
                write!(f, "array index error in array \'{}\' for {}", name, reason),
            StaticError::TypeMismatchInReturn(expected, recieved) => 
                write!(f, "Type mismatch on return, expected {}, recieved {}", expected, recieved),
            StaticError::MapKeyTypeViolation(expected, recieved) => 
                write!(f, "Type for key in map, expected {}, recieved {}", expected, recieved)
        }
    }
}
#[derive(Debug, Clone)]
pub struct StaticAnalyzer {
    pub execution_state:FakeExecutionState,
    pub errors:Vec<(String, StaticError)>
}

pub fn log_errors(errors: Vec<(String, StaticError)>) {
    let count = errors.len();
    for (fn_name, error) in errors {
        println!("{} {} in function {}", "Static Error:".red().bold(), error, fn_name);
    }
    println!("{} aborting due to {} error(s)", "error:".red().bold(), count);
}

impl StaticAnalyzer {
    pub fn check_program(&mut self, state:&State) -> Result<(), Vec<(String, StaticError)>> {
        let mut errors = Vec::new();
        for name  in state.func_map.keys() {
            match self.inspect_function(name.to_string(), state) {
                Ok(()) => (),
                Err(fn_errors) => {
                    for error in fn_errors {
                        errors.push((name.to_string(), error));
                    }
                },
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    // same as check_function, except with a index instead of passing function
    pub fn inspect_function(&mut self, fn_name:String, state:&State) -> Result<(), Vec<StaticError>> {
        self.execution_state.increment_stack_level();
        // save parameters of the function into state
        let function = state.func_map.get(&fn_name.to_string()).unwrap();
        let params = &function.params;
        for (param_type, param_name) in params {
            self.execution_state.save_variable(param_name.to_string(), param_type.clone());
        }
        let mut errors = Vec::new();
        self.eval_statements(function, state, &mut errors);
        self.execution_state.pop_stack();
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn eval_statements(&mut self, function: &Function, state:&State, errors:&mut Vec<StaticError>) {
        let mut return_flag = false;
        let expected_return = &function.return_type;
        for ast in &function.statements {
            // execute ast will run a single statement or loop, if there is a return value, exit out of function
            match self.eval_ast(state, (**ast).clone()) {
                Ok(res) => match res {
                    Some(val) => {
                        // if type_match(expected_return.clone(), val.clone()) {//expected_return != &val {
                        //     errors.push(StaticError::TypeMismatchInReturn(expected_return.clone(), val))
                        // }
                        return_flag = true;
                    }
                    None => (),
                },
                Err(e) => errors.push(e)
            }
        }
        if !return_flag {
            errors.push(StaticError::NeedReturnStm(function.name.clone()));
        }
    }

    fn get_type_of_identifier(&mut self, state: &State, identifier: Identifier) ->Result<VarType, StaticError> {
        let mut curr_type = self.type_of_expr(state, Expr::ExpVal(Object::Variable(identifier.var_name)))?;
        for indentifier_helper in identifier.tail {
            match indentifier_helper {
                IdentifierHelper::ArrayIndex(expr) => {
                    let expr_type = self.type_of_expr(state, expr);
                    match curr_type {
                        VarType::Array(var_type) => {
                            // TODO ensure expr_type matches int
                            curr_type = *var_type;
                        },
                        VarType::Map(key_vtype,value_vtype) => {
                            // TODO expr_type matches key_type
                            curr_type = *value_vtype;
                        }
                        _ => {
                            return Err(StaticError::General("Error".to_string()));
                        }
                    }
                },
                IdentifierHelper::StructIndex(attribute) =>  {
                    match curr_type {
                        VarType::Struct(struct_name) => {
                            let structure_map = match state.struct_map.get(&struct_name) {
                                Some(structure_map) => structure_map,
                                None => {
                                    return Err(StaticError::General("Error".to_string()));
                                }
                            };
                            curr_type = match get_from_vec(&attribute, structure_map) {
                                Some(vtype) => vtype,
                                None => {
                                    return Err(StaticError::General("Error".to_string()));
                                }
                            };
                        }
                        _ => {
                            return Err(StaticError::General("Error".to_string()));
                        }
                    }
                },
            }
        }
        Ok(curr_type)
    }
    
    fn eval_ast(&mut self, state:&State, ast: AstNode) -> Result<Option<VarType>, StaticError> {
        match ast {
            AstNode::Function(_) => {
                panic!("I didn't know functions were here, is this an inner function maybe");
            }
            AstNode::Assignment(vtype, identifier, exp) => {
                // type check, variable type must match the result of expression
                let value_type = self.type_of_expr(state, exp)?;

                match vtype {
                    Some(var_type) => {
                        if type_match(var_type.clone(), value_type.clone()) {
                            self.execution_state.save_variable(identifier.var_name, var_type);
                        } else {
                            return Err(StaticError::TypeViolation(var_type, value_type));
                        }
                    },
                    None => {
                        // if no variable type, turn it into an expression and parse value (error if dne)
                        let var_type = self.get_type_of_identifier(state, identifier)?;
                        if ! type_match(var_type.clone(), value_type.clone()) {
                            return Err(StaticError::TypeViolation(var_type, value_type));
                        }
                    }
                };
            }
            AstNode::StructIndexAssignment(_,_,_) => (), // TODO
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
                self.execution_state.increment_stack_level();
                for _ in 0..2 {
                    // not currently type checking need to add that later on
                    if pipe {
                        self.execution_state.save_variable(variable.clone(), VarType::Int);
                    }
                    self.type_of_expr(state, value_exp.clone())?;
                }
                self.execution_state.pop_stack();
                self.execution_state.save_variable(name, VarType::Array(Box::new(var_type)));
            }
            AstNode::IndexAssignment(name, index_exp, value_exp) => {
                let var_type = match self.get_value(name.clone())? {
                    VarType::Array(var_type) => (var_type),
                    VarType::Map(key_type, expected_value_type) => {
                        let index_type = self.type_of_expr(state, index_exp)?;
                        let val_type = self.type_of_expr(state, value_exp)?;
                        if index_type == *key_type {
                            if val_type == *expected_value_type {
                                return Ok(None);
                            }
                            return Err(StaticError::TypeViolation(*expected_value_type, val_type))
                        }
                        return Err(StaticError::MapKeyTypeViolation(*key_type, index_type))
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
                if !type_match(*var_type, value_type) {
                    return Err(StaticError::General(format!(
                        "length of array:\'{}\' must be int",
                        name
                    )));
                };
            }
            AstNode::If(if_pairs) => {
                // TOOO: need to check every single branch
                self.execution_state.increment_stack_level();
                let mut return_val = None;
                for (conditional, mut stms) in if_pairs {
                    self.check_bool_ast(state, &conditional)?;
                    while stms.len() > 0 {
                        match self.eval_ast(state, *stms.remove(0))? {
                            Some(eval) => { return_val = Some(eval) }, 
                            None => (),
                        }
                    }
                }
                self.execution_state.pop_stack();
                return Ok(return_val);
            }
            AstNode::While(conditional, stms) => {
                self.check_bool_ast(state, &conditional)?;
                self.execution_state.increment_stack_level();
                for stm in stms.iter() {
                    match self.eval_ast(state, *stm.clone())? {
                        Some(eval) => return Ok(Some(eval)), //if there was a return statement, return the value
                        None => (),
                    }
                }
                self.execution_state.pop_stack();
            }
            AstNode::BuiltIn(builtin) => {
                match builtin {
                    BuiltIn::Print(exp) => {
                        // println!("compilerPrint: {}", exp);
                        self.type_of_expr(state, exp)?;
                    }
                    BuiltIn::StaticPrint(exp) => {
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
                        match self.execution_state.var_map.get(&name) {
                            Some(var_type) => Ok(var_type.clone()),
                            None => return Err(StaticError::ValueDne(name)),
                        }
                    }
                    Object::Constant(Constant::Array(var_type, _elements)) => {
                        Ok(VarType::Array(Box::new(var_type)))
                    }
                    Object::Constant(Constant::Index(name, index_exp)) => {
                        // get array from state map
                        match self.execution_state.var_map.get(&name.clone()) {
                            Some(value) => match value.clone() {
                                VarType::Array(var_type) => {
                                    // TODO support X-Dimensional arrays
                                    Ok(*var_type)
                                },
                                VarType::Map(key_type, value_type) => {
                                    let index_type = self.type_of_expr(state, *index_exp)?;
                                    if &index_type != &*key_type {
                                        return Err(StaticError::MapKeyTypeViolation(*key_type, index_type))
                                    }
                                    Ok(*value_type)
                                }
                                VarType::Int => Ok(VarType::Int), // I think this should error out
                                VarType::String => {
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
                    Object::Constant(Constant::Struct(s)) => Ok(VarType::Struct(s.name)),
                    Object::Constant(Constant::StructVal(struct_name, attribute)) => {
                        match self.execution_state.var_map.get(&struct_name) {
                            Some(var_type) => {
                                match var_type {
                                    VarType::Struct(s) => {
                                        match state.struct_map.get(s) {
                                            Some(worm_struct) => {
                                                match get_from_vec(&attribute, worm_struct) {//.get(&attribute) {
                                                    Some(var_type) => Ok(var_type),
                                                    None => Err(StaticError::ValueDne(attribute))
                                                }
                                        },
                                            None => Err(StaticError::ValueDne(s.to_string()))
                                        }
                                    }
                                    _ => Err(StaticError::ValueDne(struct_name))
                                }
                                
                            },
                            None => Err(StaticError::ValueDne(struct_name))
                        }
                },
                    Object::Constant(Constant::Map(key_type, value_type, _)) => Ok(
                        VarType::Map(
                            Box::new(key_type),
                            Box::new(value_type)
                        )
                    ),
                    Object::FnCall(func_call) => {
                        self.type_of_func_call(state, func_call)
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

    fn arg_count_helper(&mut self, expected_params:usize, fn_call: &FnCall) -> Result<(), StaticError> {
        if fn_call.params.len() != expected_params {
            Err(StaticError::General(
                format!("Error {} requires exactly {} arg", fn_call.name, expected_params),
            ))
        } else {
            Ok(())
        }
    }

    fn reserved_function(&mut self, state: &State, func_call: &FnCall) -> Result<Option<VarType>, StaticError> {
        if func_call.name == "len" {
            self.arg_count_helper(1, func_call)?;
            Ok(Some(VarType::Int))
        } else if func_call.name == "parse_int" {
            self.arg_count_helper(1, func_call)?;
            Ok(Some(VarType::Int))
        } else if func_call.name == "user_input" {
            Ok(Some(VarType::String))
        } else if func_call.name == "to_str" {
            self.arg_count_helper(1, func_call)?;
            Ok(Some(VarType::String))
        } else if func_call.name == "append" {
            self.arg_count_helper(2, func_call)?;
            // append(List<T>, T)
            let list = self.type_of_expr(state, func_call.params[0].clone())?;
            let value = self.type_of_expr(state,func_call.params[1].clone())?;
            if type_match(VarType::Array(Box::new(value.clone())), list) {
                Ok(Some(VarType::Array(Box::new(value))))
            }
            else {
                Err(StaticError::General(
                    format!("BROKE ON NEW THING"),
                ))
            }
        } else {
            Ok(None)
        }
    }

    fn user_created_function(&mut self, state:&State, func_call: FnCall) -> Result<VarType, StaticError> {
        let function = match state.func_map.get(&func_call.name.clone()) {
            Some(func) => func,
            None => {
                // No function found. Check if struct constructor
                match state.struct_map.get(&func_call.name.clone()) {
                    Some(_) => {
                        return Ok(VarType::Struct(func_call.name.clone()));
                    },
                    None => return Err(StaticError::CannotFindFunction(func_call.name)),
                }
            },
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

    fn type_of_func_call(&mut self, state:&State, func_call: FnCall) -> Result<VarType, StaticError> {
        // retrive function from memory, make sure its value matches
        match self.reserved_function(state, &func_call)? {
           Some(var_type) => Ok(var_type),
           None => self.user_created_function(state, func_call)
        }
    }
    /*
    * returns a value or that a value does not exist
    */
    fn get_value(&mut self, name: String) -> Result<VarType, StaticError> {
        match self.execution_state.var_map.get(&name.clone()) {
            Some(value) => Ok(value.clone()),
            None => {
                Err(StaticError::ValueDne(name))
            }
        }
    }
}

fn type_match(a: VarType, b: VarType) -> bool {
    use VarType::*;
    match (a, b) {
        (Int, Int) | (Float, Float) | (String, String) | (Char, Char) => 
            true,
        (Struct(s1), Struct(s2)) => s1.eq(&s2),
        (Int, Char) => 
            true, // allow int => char conversion
        (Map(k1, v1), Map(k2, v2)) => 
            type_match(*k1, *k2) && type_match(*v1, *v2),
        (Array(arr1), Array(arr2)) => 
            type_match(*arr1, *arr2),
        _ => false,
    }
}


fn get_from_vec(name: &String, list: &Vec<(String, VarType)>) -> Option<VarType> {
    for (n, vtype) in list.iter() {
        if name.eq(n) {
            return Some(vtype.clone());
        }
    }
    None
}