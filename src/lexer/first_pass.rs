use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenKind<'a> {
    Word(&'a str),
    FullStop,
    InvalidWhitespace,
    InvalidChar,
    NonWhitespace,
}

#[derive(Debug)]
struct FirstPassLexer<'a> {
    source: &'a str,
    indices: Peekable<CharIndices<'a>>,
    line: u32,
    current: usize,
    start: usize,
    end: usize,
    tokens: Vec<Token<'a>>,
}

pub fn process(source: &str) -> Vec<Token> {
    FirstPassLexer::new(source).process()
}

impl<'a> FirstPassLexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            indices: source.char_indices().peekable(),
            line: 0,
            current: 0,
            start: 0,
            end: 0,
            tokens: Vec::new(),
        }
    }

    fn process(mut self) -> Vec<Token<'a>> {
        while let Some((i, ch)) = self.indices.next() {
            self.current = i;

            if ch.is_alphanumeric() {
                // update the end of the current token
                self.end = self.next_index().unwrap_or_else(|| self.source.len());
            } else if ch == '.' {
                self.dot();
            } else if ch.is_ascii_whitespace() && ch != '\n' {
                self.end_token();

                // ascii whitespace is always 1 byte long
                self.start += 1;
                self.end += 1;
            } else if ch == '\n' {
                self.end_token();
                self.line += 1;
                self.start += 1;
                self.end += 1;
            } else if ch.is_whitespace() {
                self.invalid_whitespace();
            } else {
                self.invalid_char();
            }
        }

        self.end_token();

        self.tokens
    }

    fn dot(&mut self) {
        if self.end > self.start {
            self.end_token();
        }

        self.end += 1; // '.' is one byte long
        self.end_token();
        if let Some((_, ch)) = self.indices.next() {
            if !ch.is_ascii_whitespace() {
                if ch.is_whitespace() {
                    self.invalid_whitespace();
                } else {
                    self.non_whitespace();
                }
            }
        }
    }

    fn end_token(&mut self) {
        if self.start < self.end {
            let slice = &self.source[self.start..self.end];

            if slice == "." {
                self.tokens.push(Token {
                    kind: TokenKind::FullStop,
                    line: self.line,
                });
            } else {
                self.tokens.push(Token {
                    kind: TokenKind::Word(slice),
                    line: self.line,
                })
            }
        }
        self.start = self.current;
        self.end = self.current;
    }

    fn invalid_something(&mut self) {
        self.end_token();
        self.current = self.next_stop();
        self.start = self.current;
        self.end = self.current;
    }

    fn invalid_whitespace(&mut self) {
        self.invalid_something();

        if let Some(last) = self.tokens.last() {
            if last.kind == TokenKind::InvalidWhitespace && last.line == self.line {
                return;
            }
        }

        self.tokens.push(Token {
            kind: TokenKind::InvalidWhitespace,
            line: self.line,
        });
    }

    fn invalid_char(&mut self) {
        self.invalid_something();

        self.tokens.push(Token {
            kind: TokenKind::InvalidChar,
            line: self.line,
        });
    }

    fn non_whitespace(&mut self) {
        self.invalid_something();

        self.tokens.push(Token {
            kind: TokenKind::NonWhitespace,
            line: self.line,
        });
    }

    fn next_index(&mut self) -> Option<usize> {
        self.indices.peek().map(|&(i, _)| i)
    }

    /// Same as next_index, but returns the length of source in case the next index
    /// doesn't exist.
    fn next_stop(&mut self) -> usize {
        self.next_index().unwrap_or_else(|| self.source.len())
    }
}
