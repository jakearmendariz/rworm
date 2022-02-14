/*
* static_analysis.rs
* Program analysis, but, without cloning, this part is still under development,
* but I think this addition could make the program run much more quickly and smoothly
*/

use crate::ast::*;
use crate::error_handling::{StaticError, WormResult};
use crate::state::*;

impl StaticAnalyzerState {
    /// Checks program for errors by evaluating each function
    pub fn check_program(&mut self, ast: &AstMap) -> Vec<StaticError> {
        ast.func_map
            .values()
            .flat_map(|function| {
                // For each function, save parameters then check its statements and types for errors.
                // Increment for scope
                self.increment_stack_level();
                // Save each parameter as local variables.
                function.params.iter().for_each(|(param_type, param_name)| {
                    self.save_variable(param_name.to_string(), param_type.clone())
                });
                // Evaluate function statements.
                let errors = self.check_function(function, ast);
                // Deletes all locally created variables
                self.pop_stack();
                errors
            })
            .collect::<Vec<StaticError>>()
    }

    fn check_function(&mut self, function: &Function, ast: &AstMap) -> Vec<StaticError> {
        let expected_return = &function.return_type;

        let errors = function
            .statements
            .iter()
            .filter_map(
                |ast_node| match self.eval_ast_node(ast, (**ast_node).clone()) {
                    Ok(res) => match res {
                        Some(val) => {
                            if !(expected_return == &val) {
                                return Some(StaticError::TypeMismatchInReturn(
                                    expected_return.clone(),
                                    val,
                                    function.position,
                                ));
                            }
                            None
                        }
                        None => None,
                    },
                    Err(e) => Some(e),
                },
            )
            .collect::<Vec<StaticError>>();
        errors
    }

    fn type_of_identifier(&mut self, ast: &AstMap, identifier: Identifier) -> WormResult<VarType> {
        let mut curr_type = match self.var_map.get(&identifier.var_name) {
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
                                    format!("Struct {}", struct_name),
                                    identifier.position,
                                ));
                            }
                        };
                        curr_type = match get_from_vec(&attribute, structure_map) {
                            Some(vtype) => vtype,
                            None => {
                                return Err(StaticError::ValueDne(
                                    format!("attribute {} from struct {}", attribute, struct_name),
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

    fn eval_ast_node(&mut self, ast: &AstMap, ast_node: AstNode) -> WormResult<Option<VarType>> {
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
                        if vtype == value_type {
                            self.save_variable(identifier.var_name, vtype);
                        } else {
                            return Err(StaticError::TypeViolation(vtype, value_type, position));
                        }
                    }
                    None => {
                        // if no variable type, turn it into an expression and parse value (error if dne)
                        let var_type = self.type_of_identifier(ast, identifier)?;
                        if !(var_type == value_type) {
                            return Err(StaticError::TypeViolation(var_type, value_type, position));
                        }
                    }
                };
            }
            AstNode::If(if_pairs) => {
                // TOOO: need to check every single branch for returns
                self.increment_stack_level();
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
                self.pop_stack();
                return Ok(return_val);
            }
            AstNode::While(conditional, stms) => {
                self.check_bool_expr(ast, conditional)?;
                self.increment_stack_level();
                for stm in stms.iter() {
                    match self.eval_ast_node(ast, *stm.clone())? {
                        Some(eval) => return Ok(Some(eval)), //if there was a return statement, return the value
                        None => (),
                    }
                }
                self.pop_stack();
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
    fn check_bool_expr(&mut self, ast: &AstMap, expr: Expr) -> WormResult<()> {
        let (bool_expr, pos) = self.type_of_expr(ast, expr)?;
        expect_type(&VarType::Bool, &bool_expr, pos)?;
        Ok(())
    }

    fn type_of_expr_wrapper(&mut self, ast: &AstMap, exp: Expr) -> WormResult<VarType> {
        let (vtype, _) = self.type_of_expr(ast, exp)?;
        Ok(vtype)
    }
    /*
     * type_of_expr returns the type provided by an inline expression
     */
    fn type_of_expr(&mut self, ast: &AstMap, exp: Expr) -> WormResult<(VarType, usize)> {
        let mut posi: usize = 0;
        let vtype = match exp {
            Expr::Identifier(identifier) => {
                // get variable as a constant value
                posi = identifier.position;
                self.type_of_identifier(ast, identifier)?
            }
            Expr::Literal(literal, position) => {
                posi = position;
                match literal {
                    Literal::Array(var_type, _elements) => VarType::Array(Box::new(var_type)),
                    Literal::Int(_) => VarType::Int,
                    Literal::Bool(_) => VarType::Bool,
                    Literal::Char(_) => VarType::Char,
                    Literal::String(_) => VarType::String,
                    Literal::Struct { name, pairs: _ } => VarType::Struct(name),
                    Literal::Map(key_type, value_type, _) => {
                        VarType::Map(Box::new(key_type), Box::new(value_type))
                    }
                }
            }
            Expr::FnCall {
                name,
                params,
                position,
            } => {
                posi = position;
                self.type_of_func_call(
                    ast,
                    FnCall {
                        name,
                        params,
                        position,
                    },
                )?
            }
            Expr::ListComprehension {
                piped_var,
                value_expr,
                in_expr: _,
            } => {
                self.increment_stack_level();
                match piped_var {
                    Some(name) => self.save_variable(name, VarType::Generic),
                    None => (),
                };
                let (value_expr_type, pos) = self.type_of_expr(ast, *value_expr)?;
                posi = pos;
                VarType::Array(Box::new(value_expr_type))
            }
            Expr::UnaryExpr(unary_op, expr) => {
                let (actual_type, pos) = self.type_of_expr(ast, *expr)?;
                posi = pos;
                match (unary_op, actual_type.clone()) {
                    (UnaryOp::Not, VarType::Bool) => VarType::Bool,
                    _ => return Err(StaticError::TypeViolation(VarType::Bool, actual_type, pos)),
                }
            }
            Expr::BinaryExpr(lhs, op, rhs) => {
                let (left, leftpos) = self.type_of_expr(ast, *lhs)?;
                let (right, rightpos) = self.type_of_expr(ast, *rhs)?;
                posi = leftpos;
                use OpType::*;
                use VarType::*;
                return Ok(match op {
                    And | Or => {
                        // If the operator is `and` or `or` then both sides must eval to boolean.
                        expect_type(&VarType::Bool, &left, leftpos)?;
                        expect_type(&VarType::Bool, &right, rightpos)?;
                        (VarType::Bool, leftpos)
                    }
                    Lt | Gt | Leq | Geq | Neq | Eq => {
                        expect_type(&left, &right, leftpos)?;
                        (VarType::Bool, leftpos)
                    }
                    _ => match (left.clone(), right.clone()) {
                        (Char, Char) => (String, leftpos),
                        (Char, String) => (String, leftpos),
                        (String, Char) => (String, leftpos),
                        (Generic, a) => (a, leftpos),
                        (a, Generic) => (a, leftpos),
                        (_, _) => {
                            if left == right {
                                (left, leftpos)
                            } else {
                                return Err(StaticError::TypeViolation(
                                    left.clone(),
                                    right.clone(),
                                    leftpos,
                                ));
                            }
                        }
                    },
                });
            }
        };
        Ok((vtype, posi))
    }

    fn arg_count_helper(
        &mut self,
        expected_params: Vec<VarType>,
        fn_call: &FnCall,
    ) -> WormResult<()> {
        // TODO types match
        if fn_call.params.len() != expected_params.len() {
            return Err(StaticError::General(
                format!(
                    "{} requires exactly {} arg",
                    fn_call.name,
                    expected_params.len()
                ),
                fn_call.position,
            ));
        }
        Ok(())
    }

    fn check_reserved_function(&mut self, ast: &AstMap, func_call: &FnCall) -> WormResult<VarType> {
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
            _ => panic!("Cannot find reserved function in static_anal"),
        }
    }

    fn check_user_created_fn(&mut self, ast: &AstMap, func_call: FnCall) -> WormResult<VarType> {
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
        if func_call.params.len() != function.params.len() {
            return Err(StaticError::General(
                format!(
                    "Wrong number of arguments provided to function. Expected {}, recieved: {}",
                    function.params.len(),
                    func_call.params.len(),
                ),
                func_call.position,
            ));
        }
        // iterate through the parameters provided and the function def,
        for (expr, (var_type, _)) in func_call.params.iter().zip(function.params.iter()) {
            let (param_const, position) = self.type_of_expr(ast, expr.clone())?;
            expect_type(&var_type, &param_const, position)?;
        }
        // function input types match expected values, return a empty literal of matching type
        Ok(function.return_type.clone())
    }

    fn type_of_func_call(
        &mut self,
        ast: &AstMap,
        func_call: FnCall,
    ) -> Result<VarType, StaticError> {
        // retrive function from memory, make sure its value matches
        if RESERVED_FUNCTIONS.contains(&func_call.name[..]) {
            self.check_reserved_function(ast, &func_call)
        } else {
            self.check_user_created_fn(ast, func_call)
        }
    }
}

fn expect_type(expected: &VarType, actual: &VarType, position: usize) -> Result<(), StaticError> {
    if !(expected == actual) {
        Err(StaticError::TypeViolation(
            expected.clone(),
            actual.clone(),
            position,
        ))
    } else {
        Ok(())
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
