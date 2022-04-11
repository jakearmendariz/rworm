use crate::ast::VarType;
use colored::Colorize;

#[derive(Debug, Clone)]
pub enum StaticError {
    ValueDne(String, usize),
    TypeViolation(VarType, VarType, usize),
    _NeedReturnStm(String, usize),
    TypeMismatchInReturn(VarType, VarType, usize),
    CannotFindFunction(String, usize),
    General(String, usize),
}

pub type WormResult<T> = Result<T, StaticError>;

impl std::fmt::Display for StaticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            StaticError::ValueDne(x, _) => write!(f, "variable \'{}\' does not exist", x),
            StaticError::TypeViolation(a, b, _) => {
                write!(f, "expected type \'{}\' recieved type \'{}\'", a, b)
            }
            StaticError::_NeedReturnStm(name, _) => {
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

/// Read entire file, find the line and column number of char position.
fn get_line_col(file_content: &str, position: usize) -> (usize, usize) {
    let (mut row_counter, mut start_of_line) = (1, 0);
    for (idx, character) in file_content[..position].chars().enumerate() {
        if character == '\n' {
            row_counter += 1;
            start_of_line = idx;
        }
    }
    (row_counter, position - start_of_line)
}

/// Every static error has a position in it, parse this value and return
fn get_position_of_error(file_content: &str, error: StaticError) -> (usize, usize) {
    let position: usize = match error {
        StaticError::ValueDne(_, pos) => pos,
        StaticError::TypeViolation(_, _, pos) => pos,
        StaticError::_NeedReturnStm(_, pos) => pos,
        StaticError::CannotFindFunction(_, pos) => pos,
        StaticError::General(_, pos) => pos,
        StaticError::TypeMismatchInReturn(_, _, pos) => pos,
    };
    get_line_col(file_content, position)
}

/// Logs errors with line number and column
pub fn log_errors(errors: Vec<StaticError>, file_content: String) {
    let error_count = errors.len();
    for error in errors {
        let (line, col) = get_position_of_error(&file_content, error.clone());
        println!("{} {} on {}:{}", "Error:".red().bold(), error, line, col,);
    }
    println!("Aborting due to {} error(s)", error_count);
}
