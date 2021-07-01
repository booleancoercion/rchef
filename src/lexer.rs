mod first_pass;
mod second_pass;

use crate::Result;
pub use second_pass::{Token, TokenKind};

pub fn process(source: &str) -> Result<Vec<Token>> {
    let subtokens = first_pass::process(source);
    println!("{:?}", subtokens);
    second_pass::process(subtokens)
}
