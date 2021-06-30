#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
}
