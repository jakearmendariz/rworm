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
    ValueDne(String, usize),
    TypeViolation(VarType, VarType, usize),
    NeedReturnStm(String, usize),
    TypeMismatchInReturn(VarType, VarType, usize),
    CannotFindFunction(String, usize),
    General(String, usize),
}

impl std::fmt::Display for StaticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            StaticError::ValueDne(x, _) => write!(f, "variable \'{}\' does not exist", x),
            StaticError::TypeViolation(a, b, _) => {
                write!(f, "expected type \'{}\' recieved type \'{}\'", a, b)
            }
            StaticError::NeedReturnStm(name, _) => {
                write!(f, "need a return statment in function \'{}\'", name)
            }
            StaticError::CannotFindFunction(name, _) => {
                write!(f, "function \'{}\' does not exist", name)
            }
            StaticError::General(x, _) => write!(f, "{}", x),
            StaticError::TypeMismatchInReturn(expected, recieved, _) => write!(
                f,
                "Type mismatch on return, expected {}, recieved {}",
                expected, recieved
            ),
        }
    }
}
#[derive(Debug, Clone, Default)]
pub struct StaticAnalyzer {
    pub execution_state: FakeExecutionState,
    pub errors: Vec<(String, StaticError)>,
}

fn get_line_col(file_content: &String, position: usize) -> (usize, usize) {
    let mut row_counter = 1;
    let mut start_of_line = 0;
    for (idx, character) in file_content[..position].chars().enumerate() {
        if character == '\n' {
            row_counter += 1;
            start_of_line = idx;
        }
    }
    // println!("{} {} {}", position, row_counter, start_of_line);
    (row_counter, position - start_of_line)
}

fn get_position_from_error(file_content: &String, error: StaticError) -> (usize, usize) {
    let position: usize = match error {
        StaticError::ValueDne(_, pos) => pos,
        StaticError::TypeViolation(_, _, pos) => pos,
        StaticError::NeedReturnStm(_, pos) => pos,
        StaticError::CannotFindFunction(_, pos) => pos,
        StaticError::General(_, pos) => pos,
        StaticError::TypeMismatchInReturn(_, _, pos) => pos,
    };
    get_line_col(file_content, position)
}
pub fn log_errors(errors: Vec<(String, StaticError)>, file_content: String) {
    let count = errors.len();
    for (fn_name, error) in errors {
        let (line, col) = get_position_from_error(&file_content, error.clone());
        println!(
            "{} {} in function {} on {}:{}",
            "Static Error:".red().bold(),
            error,
            fn_name,
            line,
            col,
        );
    }
    println!(
        "{} aborting due to {} error(s)",
        "error:".red().bold(),
        count
    );
}

impl StaticAnalyzer {
    pub fn check_program(&mut self, ast: &AstMap) -> Vec<(String, StaticError)> {
        let errors = ast
            .func_map
            .keys()
            .flat_map(|name| self.inspect_function(name.to_string(), ast))
            .collect();
        errors
    }

    // same as check_function, except with a index instead of passing function
    pub fn inspect_function(
        &mut self,
        fn_name: String,
        ast: &AstMap,
    ) -> Vec<(String, StaticError)> {
        self.execution_state.increment_stack_level();
        // save parameters of the function into state
        let function = ast.func_map.get(&fn_name.to_string()).unwrap();
        function.params.iter().for_each(|(param_type, param_name)| {
            self.execution_state
                .save_variable(param_name.to_string(), param_type.clone())
        });
        let errors = self.check_statements(function, ast);
        self.execution_state.pop_stack();
        errors
            .into_iter()
            .map(|error| (fn_name.clone(), error))
            .collect()
    }

    fn check_statements(&mut self, function: &Function, ast: &AstMap) -> Vec<StaticError> {
        let mut errors = Vec::new();
        let mut return_flag = false;
        let expected_return = &function.return_type;
        for ast_node in &function.statements {
            // execute ast_node will run a single statement or loop, if there is a return value, exit out of function
            match self.eval_ast_node(ast, (**ast_node).clone()) {
                Ok(res) => match res {
                    Some(val) => {
                        if !type_match(&expected_return, &val) {
                            errors.push(StaticError::TypeMismatchInReturn(
                                expected_return.clone(),
                                val,
                                function.position,
                            ))
                        }
                        return_flag = true;
                    }
                    None => (),
                },
                Err(e) => errors.push(e),
            }
        }
        if !return_flag {
            errors.push(StaticError::NeedReturnStm(
                function.name.clone(),
                function.position,
            ));
        }
        errors
    }

    fn get_type_of_identifier(
        &mut self,
        ast: &AstMap,
        identifier: Identifier,
    ) -> Result<VarType, StaticError> {
        let mut curr_type = match self.execution_state.var_map.get(&identifier.var_name) {
            Some(vtype) => vtype.clone(),
            None => {
                return Err(StaticError::ValueDne(
                    identifier.var_name,
                    identifier.position,
                ))
            }
        };
        for indentifier_helper in identifier.tail {
            match indentifier_helper {
                IdentifierHelper::ArrayIndex(expr) => {
                    let (expr_type, position) = self.type_of_expr(ast, expr)?;
                    match curr_type {
                        VarType::Array(var_type) => {
                            expect_type(&VarType::Int, &expr_type, position)?;
                            curr_type = *var_type;
                        }
                        VarType::Map(key_vtype, value_vtype) => {
                            expect_type(&*key_vtype, &expr_type, position)?;
                            curr_type = *value_vtype;
                        }
                        VarType::String => {
                            expect_type(&VarType::Int, &expr_type, position)?;
                            curr_type = VarType::Char;
                        }
                        _ => {
                            return Err(StaticError::General(
                                format!("Expected Array or Map, got \"{:?}\"", curr_type,),
                                position,
                            ));
                        }
                    }
                }
                IdentifierHelper::StructIndex(attribute) => match curr_type {
                    VarType::Struct(struct_name) => {
                        let structure_map = match ast.struct_map.get(&struct_name) {
                            Some(structure_map) => structure_map,
                            None => {
                                return Err(StaticError::ValueDne(
                                    format!("Struct {}", struct_name,),
                                    identifier.position,
                                ));
                            }
                        };
                        curr_type = match get_from_vec(&attribute, structure_map) {
                            Some(vtype) => vtype,
                            None => {
                                return Err(StaticError::ValueDne(
                                    format!("attribute {} from struct {}", attribute, struct_name,),
                                    identifier.position,
                                ));
                            }
                        };
                    }
                    _ => {
                        return Err(StaticError::General(
                            "Tried to access attribute on a non-struct".to_string(),
                            identifier.position,
                        ));
                    }
                },
            }
        }
        Ok(curr_type)
    }

    fn eval_ast_node(&mut self, ast: &AstMap, ast_node: AstNode) -> Result<Option<VarType>, StaticError> {
        match ast_node {
            AstNode::Assignment {
                var_type,
                identifier,
                expr,
            } => {
                // type check, variable type must match the result of expression
                let (value_type, position) = self.type_of_expr(ast, expr)?;
                match var_type {
                    Some(vtype) => {
                        if type_match(&vtype, &value_type) {
                            self.execution_state
                                .save_variable(identifier.var_name, vtype);
                        } else {
                            // println!("Error at {}", position);
                            return Err(StaticError::TypeViolation(vtype, value_type, position));
                        }
                    }
                    None => {
                        // if no variable type, turn it into an expression and parse value (error if dne)
                        let var_type = self.get_type_of_identifier(ast, identifier)?;
                        if !type_match(&var_type, &value_type) {
                            return Err(StaticError::TypeViolation(var_type, value_type, position));
                        }
                    }
                };
            }
            AstNode::If(if_pairs) => {
                // TOOO: need to check every single branch
                self.execution_state.increment_stack_level();
                let mut return_val = None;
                for (conditional, mut stms) in if_pairs {
                    self.check_bool_expr(ast, conditional)?;
                    while stms.len() > 0 {
                        match self.eval_ast_node(ast, *stms.remove(0))? {
                            Some(eval) => return_val = Some(eval),
                            None => (),
                        }
                    }
                }
                self.execution_state.pop_stack();
                return Ok(return_val);
            }
            AstNode::While(conditional, stms) => {
                self.check_bool_expr(ast, conditional)?;
                self.execution_state.increment_stack_level();
                for stm in stms.iter() {
                    match self.eval_ast_node(ast, *stm.clone())? {
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
                        self.type_of_expr(ast, exp)?;
                    }
                    BuiltIn::StaticPrint(exp) => {
                        self.type_of_expr(ast, exp)?;
                    }
                    BuiltIn::Assert(bool_expr) => {
                        self.check_bool_expr(ast, bool_expr)?;
                    }
                }
                ()
            }
            AstNode::ReturnStm(expr) => {
                return Ok(Some(self.type_of_expr_wrapper(ast, expr)?));
            }
            AstNode::Skip() => (),
        }
        Ok(None)
    }

    /*
     * evalulates booleans based on their conjunction
     */
    fn check_bool_expr(&mut self, ast: &AstMap, expr: Expr) -> Result<(), StaticError> {
        let (bool_expr, pos) = self.type_of_expr(ast, expr)?;
        expect_type(&VarType::Bool, &bool_expr, pos)?;
        Ok(())
    }

    fn type_of_expr_wrapper(&mut self, ast: &AstMap, exp: Expr) -> Result<VarType, StaticError> {
        let (vtype, _) = self.type_of_expr(ast, exp)?;
        Ok(vtype)
    }
    /*
     * type_of_expr returns the type provided by an inline expression
     */
    fn type_of_expr(&mut self, ast: &AstMap, exp: Expr) -> Result<(VarType, usize), StaticError> {
        match exp {
            Expr::Identifier(identifier) => {
                // get variable as a constant value
                let position = identifier.position;
                Ok((self.get_type_of_identifier(ast, identifier)?, position))
            }
            Expr::Constant(constant, position) => match constant {
                Constant::Array(var_type, _elements) => {
                    Ok((VarType::Array(Box::new(var_type)), position))
                }
                Constant::Int(_) => Ok((VarType::Int, position)),
                Constant::Bool(_) => Ok((VarType::Bool, position)),
                Constant::Char(_) => Ok((VarType::Char, position)),
                Constant::String(_) => Ok((VarType::String, position)),
                Constant::Struct { name, pairs: _ } => Ok((VarType::Struct(name), position)),
                Constant::Map(key_type, value_type, _) => Ok((
                    VarType::Map(Box::new(key_type), Box::new(value_type)),
                    position,
                )),
            },
            Expr::FnCall {
                name,
                params,
                position,
            } => Ok((
                self.type_of_func_call(
                    ast,
                    FnCall {
                        name,
                        params,
                        position,
                    },
                )?,
                position,
            )),
            Expr::ListComprehension {
                piped_var,
                value_expr,
                in_expr: _,
            } => {
                self.execution_state.increment_stack_level();
                match piped_var {
                    Some(name) => self.execution_state.save_variable(name, VarType::Generic),
                    None => (),
                };
                let (value_expr_type, pos) = self.type_of_expr(ast, *value_expr)?;
                Ok((VarType::Array(Box::new(value_expr_type)), pos))
            }
            Expr::UnaryExpr(unary_op, expr) => {
                let (actual_type, pos) = self.type_of_expr(ast, *expr)?;
                match (unary_op, actual_type.clone()) {
                    (UnaryOp::Not, VarType::Bool) => Ok((VarType::Bool, pos)),
                    _ => Err(StaticError::TypeViolation(VarType::Bool, actual_type, pos))
                }
            }
            Expr::BinaryExpr(lhs, op, rhs) => {
                let (left, leftpos) = self.type_of_expr(ast, *lhs)?;
                let (right, rightpos) = self.type_of_expr(ast, *rhs)?;
                use OpType::*;
                use VarType::*;
                match op {
                    And | Or => {
                        // If the operator is `and` or `or` then both sides must eval to boolean.
                        expect_type(&VarType::Bool, &left, leftpos)?;
                        expect_type(&VarType::Bool, &right, rightpos)?;
                        Ok((VarType::Bool, leftpos))
                    }
                    Lt | Gt | Leq | Geq | Neq | Eq => {
                        expect_type(&left, &right, leftpos)?;
                        Ok((VarType::Bool, leftpos))
                    }
                    _ => match (left.clone(), right.clone()) {
                        (Char, Char) => Ok((String, leftpos)),
                        (Char, String) => Ok((String, leftpos)),
                        (String, Char) => Ok((String, leftpos)),
                        (Generic, a) => Ok((a, leftpos)),
                        (a, Generic) => Ok((a, leftpos)),
                        (_, _) => {
                            if type_match(&left, &right) {
                                Ok((left, leftpos))
                            } else {
                                Err(StaticError::TypeViolation(
                                    left.clone(),
                                    right.clone(),
                                    leftpos,
                                ))
                            }
                        }
                    },
                }
            }
        }
    }

    fn arg_count_helper(
        &mut self,
        expected_params: Vec<VarType>,
        fn_call: &FnCall,
    ) -> Result<(), StaticError> {
        // TODO types match
        if fn_call.params.len() != expected_params.len() {
            return Err(StaticError::General(
                format!(
                    "Error {} requires exactly {} arg",
                    fn_call.name,
                    expected_params.len()
                ),
                fn_call.position,
            ));
        }
        Ok(())
    }

    fn reserved_function(
        &mut self,
        ast: &AstMap,
        func_call: &FnCall,
    ) -> Result<VarType, StaticError> {
        match &func_call.name[..] {
            LEN => {
                self.arg_count_helper(vec![VarType::Array(Box::new(VarType::Int))], func_call)?;
                Ok(VarType::Int)
            }
            PARSE_INT => {
                self.arg_count_helper(vec![VarType::String], func_call)?;
                Ok(VarType::Int)
            }
            USER_INPUT => {
                self.arg_count_helper(vec![], func_call)?;
                Ok(VarType::String)
            }
            TO_STR => {
                self.arg_count_helper(vec![VarType::Int], func_call)?;
                Ok(VarType::String)
            }
            REMOVE => {
                self.arg_count_helper(
                    vec![VarType::Array(Box::new(VarType::Generic)), VarType::Int],
                    func_call,
                )?;
                let (list, pos) = self.type_of_expr(ast, func_call.params[0].clone())?;
                let (index, index_pos) = self.type_of_expr(ast, func_call.params[1].clone())?;
                expect_type(&VarType::Int, &index, index_pos)?;
                match list {
                    VarType::Array(inner) => Ok(VarType::Array(inner)),
                    VarType::String => Ok(VarType::String),
                    actual_type => Err(StaticError::TypeViolation(
                        VarType::Array(Box::new(VarType::Generic)),
                        actual_type,
                        pos,
                    )),
                }
            }
            APPEND | PREPEND => {
                self.arg_count_helper(
                    vec![VarType::Array(Box::new(VarType::Int)), VarType::Int],
                    func_call,
                )?;
                // append(List<T>, T)
                let (list, pos) = self.type_of_expr(ast, func_call.params[0].clone())?;
                let (inner_type, _) = self.type_of_expr(ast, func_call.params[1].clone())?;
                let value = VarType::Array(Box::new(inner_type));
                expect_type(&value, &list, pos)?;
                Ok(value)
            }
            _ => panic!("ERROR CANNOT FIND RESERVED FUNCTION"),
        }
    }

    fn user_created_function(
        &mut self,
        ast: &AstMap,
        func_call: FnCall,
    ) -> Result<VarType, StaticError> {
        let function = match ast.func_map.get(&func_call.name.clone()) {
            Some(func) => func,
            None => {
                // No function found. Check if struct constructor
                match ast.struct_map.get(&func_call.name.clone()) {
                    Some(_) => {
                        return Ok(VarType::Struct(func_call.name.clone()));
                    }
                    None => {
                        return Err(StaticError::CannotFindFunction(
                            func_call.name,
                            func_call.position,
                        ))
                    }
                }
            }
        };
        // iterate through the parameters provided and the function def,
        for (expr, (var_type, _)) in func_call.params.iter().zip(function.params.iter()) {
            let (param_const, position) = self.type_of_expr(ast, expr.clone())?;
            expect_type(&var_type, &param_const, position)?;
        }
        // function input types match expected values, return a empty constant of matching type
        Ok(function.return_type.clone())
    }

    fn type_of_func_call(
        &mut self,
        ast: &AstMap,
        func_call: FnCall,
    ) -> Result<VarType, StaticError> {
        // retrive function from memory, make sure its value matches
        if RESERVED_FUNCTIONS.contains(&func_call.name[..]) {
            self.reserved_function(ast, &func_call)
        } else {
            self.user_created_function(ast, func_call)
        }
    }
}

fn expect_type(expected: &VarType, actual: &VarType, position: usize) -> Result<(), StaticError> {
    if !type_match(expected, actual) {
        Err(StaticError::TypeViolation(
            expected.clone(),
            actual.clone(),
            position,
        ))
    } else {
        Ok(())
    }
}

fn type_match(a: &VarType, b: &VarType) -> bool {
    use VarType::*;
    match (a, b) {
        (Int, Int) | (String, String) | (Char, Char) | (Bool, Bool) => true,
        (Struct(s1), Struct(s2)) => s1.eq(s2),
        (Int, Char) => true, // allow int => char conversion
        (Map(k1, v1), Map(k2, v2)) => type_match(&*k1, &*k2) && type_match(&*v1, &*v2),
        (Array(arr1), Array(arr2)) => type_match(&*arr1, &*arr2),
        (Array(inner), String) => type_match(&*inner, &VarType::Char),
        (String, Array(inner)) => type_match(&*inner, &VarType::Char),
        (Generic, _) => true,
        (_, Generic) => true,
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
