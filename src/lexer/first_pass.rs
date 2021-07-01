#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SubToken<'a> {
    pub kind: SubTokenKind<'a>,
    pub line: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SubTokenKind<'a> {
    Word(&'a str),
    FullStop,
    NewLine,
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
                if self.peek_match(|c| !is_whitespace(c)) {
                    self.add_subtoken(SubTokenKind::InvalidChar(ch));
                } else {
                    self.add_subtoken(SubTokenKind::FullStop);
                }
            }
            '\r' => {}
            ' ' => {
                if self.peek_match(|c| c == ' ') {
                    self.add_subtoken(SubTokenKind::InvalidChar(' '));
                    // not advancing, to catch multiple spaces in a row
                }
            }
            '\n' => self.newline(),
            ch => {
                if is_identifier_char(ch) {
                    self.identifier();
                } else {
                    self.add_subtoken(SubTokenKind::InvalidChar(ch));
                }
            }
        }
    }

    fn newline(&mut self) {
        let mut count = 1;
        while let Some(ch) = self.peek() {
            if ch != '\n' && ch != '\r' {
                break;
            }

            if ch == '\n' {
                count += 1;
            }
            self.advance();
        }

        if count == 1 {
            self.add_subtoken(SubTokenKind::NewLine);
        } else {
            for _ in 0..count {
                self.line += 1;
                self.add_subtoken(SubTokenKind::BlankLine);
            }
        }
    }

    fn identifier(&mut self) {
        while let Some(ch) = self.peek() {
            if !is_identifier_char(ch) {
                break;
            }
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

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.indices[self.current].1)
        }
    }

    fn peek_match(&self, f: impl FnOnce(char) -> bool) -> bool {
        match self.peek() {
            Some(ch) => f(ch),
            None => false,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.indices.len()
    }
}
