use std::num::NonZeroU32;

use super::first_pass::{SubToken, SubTokenKind};
use crate::{RChefError, Result};

#[derive(Clone, Debug, PartialEq)]
#[rustfmt::skip]
pub enum TokenKind {
    // Types of user identifiers
    Identifier(String), Ordinal(NonZeroU32), Number(i64),

    // Header keywords
    Ingredients, Method,

    // Ingredient keywords
    DryMeasure, LiquidMeasure,
    AmbiguousMeasure, MeasureType,

    // Method keywords,
    Take, FromRefrigerator, Put, Into, MixingBowl, Fold,
    Add, To, Remove, Combine, Divide, DryIngredients,
    Liquefy, ContentsOf, The, Stir, For, Minutes, Mix,
    Well, Clean, Pour, Baking, Dish, Until, Set, Aside,
    ServeWith, Refrigerate, Hours,

    Serves,

    EOF,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
}

pub fn process(subtokens: Vec<SubToken>) -> Result<Vec<Token>> {
    SecondPassLexer::new(subtokens).process()
}

struct SecondPassLexer<'a> {
    subtokens: Vec<SubToken<'a>>,
    line: u32,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    errored: bool,
    at_title: bool,
}

impl<'a> SecondPassLexer<'a> {
    fn new(subtokens: Vec<SubToken<'a>>) -> Self {
        Self {
            subtokens,
            line: 0,
            start: 0,
            current: 0,
            tokens: Vec::new(),
            errored: false,
            at_title: true,
        }
    }

    fn process(mut self) -> Result<Vec<Token>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        if self.errored {
            Err(RChefError::Lex)
        } else {
            Ok(self.tokens)
        }
    }

    fn scan_token(&mut self) {
        let st = self.advance();

        self.line = st.line;

        if st.kind == SubTokenKind::EOF {
            self.add_token(TokenKind::EOF);
            return;
        }

        if self.at_title {
            match st.kind {
                SubTokenKind::Word(_) => self.title(),
                SubTokenKind::BlankLine => {}
                SubTokenKind::FullStop => self.invalid_char('.'),
                SubTokenKind::InvalidChar(c) => self.invalid_char('c'),
                SubTokenKind::EOF => unreachable!(),
            }
        }
    }

    fn title(&mut self) {
        while let Some(SubToken {
            kind: SubTokenKind::Word(_),
            ..
        }) = self.peek()
        {
            self.advance();
        }

        let start = self.subtokens[self.start].range.0;
        let end = self.subtokens[self.current].range.1;
    }

    fn advance(&mut self) -> SubToken<'a> {
        let st = self.subtokens[self.current];
        self.current += 1;

        st
    }

    fn peek(&self) -> Option<SubToken<'a>> {
        if self.is_at_end() {
            None
        } else {
            Some(self.subtokens[self.current])
        }
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            line: self.line,
        })
    }

    fn invalid_char(&self, c: char) {
        crate::report_error(self.line, "syntax ", format!("invalid character: '{}'", c));
        self.errored = true;
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.subtokens.len()
    }
}
