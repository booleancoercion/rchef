mod first_pass;
mod second_pass;

use crate::Result;
pub use first_pass::{Token, TokenKind};
pub use second_pass::{Coin, CoinKind};

pub fn process(source: &str) -> Result<Vec<Coin>> {
    let tokens = first_pass::process(source);
    println!("{:?}", tokens);
    todo!()
}
