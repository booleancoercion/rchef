mod first_pass;
mod second_pass;

use crate::Result;
pub use second_pass::{Token, TokenKind};

pub fn process(source: &str) -> Result<Vec<Token>> {
    let subtokens = first_pass::process(source);
    let tokens = second_pass::process(source, subtokens)?;
    todo!()
}
