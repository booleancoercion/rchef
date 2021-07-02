mod lexer;
mod parser;

use thiserror::Error;

use std::fs;
use std::io;
use std::result;

pub type Result<T> = result::Result<T, RChefError>;

#[derive(Debug, Error)]
pub enum RChefError {
    #[error("io error while running: {0}")]
    Io(#[from] io::Error),

    #[error("lex error")]
    Lex,

    #[error("parse error")]
    Parse,
}

pub fn run(filename: &str) -> Result<()> {
    let source = fs::read_to_string(filename)?;

    let tokens = lexer::process(&source)?;

    let recipes = parser::process(tokens)?;

    Ok(())
}

pub fn report_error<S: std::fmt::Display>(line: u32, prefix: &str, msg: S) {
    eprintln!("[line {}] {}error: {}", line + 1, prefix, msg);
}
