#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SubToken<'a> {
    pub kind: SubTokenKind<'a>,
    pub line: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SubTokenKind<'a> {
    Word(&'a str),
    FullStop,
    BlankLine,
    Eof,

    InvalidChar(char),
}

pub fn process(source: &str) -> Vec<SubToken> {
    FirstPassLexer::new(source).process()
}

#[derive(Debug)]
struct FirstPassLexer<'a> {
    source: &'a str,
    indices: Vec<(usize, char)>,
    line: u32,
    start: usize,
    current: usize,
    subtokens: Vec<SubToken<'a>>,
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\r' || c == '\n'
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric()
}

impl<'a> FirstPassLexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            indices: source.char_indices().collect(),
            line: 0,
            start: 0,
            current: 0,
            subtokens: Vec::new(),
        }
    }

    fn process(mut self) -> Vec<SubToken<'a>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_subtoken();
        }

        self.add_subtoken(SubTokenKind::Eof);
        self.subtokens
    }

    fn scan_subtoken(&mut self) {
        let ch = self.advance();

        match ch {
            '.' => {
                if !is_whitespace(self.peek()) {
                    self.add_subtoken(SubTokenKind::InvalidChar(ch));
                } else {
                    self.add_subtoken(SubTokenKind::FullStop);
                }
            }
            '\r' | ' ' => {}
            '\n' => {
                self.line += 1;
                if self.last_line() + 2 == self.line {
                    self.add_subtoken(SubTokenKind::BlankLine);
                }
            }

            ch => {
                if is_identifier_char(ch) {
                    self.identifier();
                } else {
                    self.add_subtoken(SubTokenKind::InvalidChar(ch));
                }
            }
        }
    }

    fn identifier(&mut self) {
        while is_identifier_char(self.peek()) {
            self.advance();
        }

        let s = self.current_substring();
        self.add_subtoken(SubTokenKind::Word(s));
    }

    fn current_range(&self) -> (usize, usize) {
        let start = self.indices[self.start].0;
        let end = self.indices[self.current.min(self.indices.len() - 1)].0;

        (start, end)
    }

    fn current_substring(&self) -> &'a str {
        let (start, end) = self.current_range();
        &self.source[start..end]
    }

    fn add_subtoken(&mut self, kind: SubTokenKind<'a>) {
        self.subtokens.push(SubToken {
            kind,
            line: self.line,
        })
    }

    fn advance(&mut self) -> char {
        let ch = self.indices[self.current].1;
        self.current += 1;
        ch
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.indices[self.current].1
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.indices.len()
    }

    fn last_line(&self) -> u32 {
        if let Some(subtoken) = self.subtokens.last() {
            subtoken.line
        } else {
            self.line
        }
    }
}
