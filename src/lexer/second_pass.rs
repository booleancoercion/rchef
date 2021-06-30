use super::first_pass::{SubToken, SubTokenKind};
use crate::{RChefError, Result};

use if_chain::if_chain;
use lazy_static::lazy_static;

use std::collections::HashMap;
use std::num::NonZeroU32;

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
    Well, Clean, Pour, BakingDish, Until, SetAside,
    ServeWith, Refrigerate, Hours,

    Serves,

    Eof, BlankLine, FullStop,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
}

pub fn process(source: &str, subtokens: Vec<SubToken>) -> Result<Vec<Token>> {
    SecondPassLexer::new(source, subtokens).process()
}

struct SecondPassLexer<'a> {
    source: &'a str,
    subtokens: Vec<SubToken<'a>>,
    line: u32,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    errored: bool,
    at_title: bool,
}

#[rustfmt::skip]
fn is_ordinal(ident: &str) -> Option<NonZeroU32> {
    if !ident.ends_with("1st")
        && !ident.ends_with("2nd")
        && !ident.ends_with("3rd")
        && !ident.ends_with("th")
    {
        return None;
    }

    // this will never panic, because we know the last two chars are ascii
    let nums = &ident[..ident.len() - 2];
    let chars = nums.chars();

    if ident.is_empty() 
        || chars.next().unwrap() == '0'
        || chars.any(|c| !('0'..='9').contains(&c))
    {
        return None;
    }

    nums.parse().ok()
}

impl<'a> SecondPassLexer<'a> {
    fn new(source: &'a str, subtokens: Vec<SubToken<'a>>) -> Self {
        Self {
            source,
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

        match st.kind {
            SubTokenKind::BlankLine => self.add_token(TokenKind::BlankLine),
            SubTokenKind::Eof => self.add_token(TokenKind::Eof),
            SubTokenKind::InvalidChar(c) => self.invalid_char(c),
            SubTokenKind::Word(_) => {
                if self.at_title {
                    self.title();
                } else {
                    self.identifier();
                }
            }
            SubTokenKind::FullStop => {
                if self.at_title {
                    // the full stop should not be at the beginning!
                    // if there was already a title, this case wouldn't happen.
                    self.invalid_char('.');
                } else {
                    self.add_token(TokenKind::FullStop);
                }
            }
        }
    }

    #[rustfmt::skip]
    fn identifier(&mut self) {
        if let Some(x) = self.matches_single_keyword() {
            self.add_token(x);
        } else if let Some(x) = self.matches_double_keyword() {
            self.advance(); // consume the second token
            self.add_token(x);
        } else {
            while let Some(SubToken { kind: SubTokenKind::Word(_), .. }) = self.peek() {
                if self.matches_single_keyword().is_some()
                    || self.matches_double_keyword().is_some()
                {
                    break;
                }

                self.line = self.advance().line;
            }

            let (start, end) = self.current_range();
            let s = &self.source[start..end];

            if s.chars().all(|c| ('0'..='9').contains(&c)) {
                // if the string is wholly composed of numbers
                let n = s.parse();
                if let Ok(n) = n {
                    self.add_token(TokenKind::Number(n));
                } else {
                    crate::report_error(
                        self.line,
                        "numeric ",
                        "invalid number literal (numbers must fit into a signed 64-bit integer)".into()
                    );
                    self.errored = true;
                }
            } else if let Some(n) = is_ordinal(s) {
                self.add_token(TokenKind::Ordinal(n));
            } else {
                self.add_token(TokenKind::Identifier(s.into()));
            }
        }
    }

    #[rustfmt::skip]
    fn title(&mut self) {
        self.at_title = false;
        while let Some(SubToken { kind: SubTokenKind::Word(_), .. }) = self.peek() {
            self.advance();
        }

        let (start, end) = self.current_range();
        let s = &self.source[start..end];
        self.add_token(TokenKind::Identifier(s.to_string()));

        let mut should_continue = true;

        if let Some(SubToken { kind: SubTokenKind::FullStop, .. }) = self.peek() {
            self.advance();
            self.add_token(TokenKind::FullStop);
        } else {
            crate::report_error(
                self.line,
                "syntax ",
                "expected FULLSTOP '.' at end of recipe title".into(),
            );
            self.errored = true;
            should_continue = false;
        }

        if let Some(SubToken { kind: SubTokenKind::BlankLine, .. }) = self.peek() {
            self.advance();
            self.add_token(TokenKind::BlankLine);
        } else {
            crate::report_error(
                self.line,
                "syntax ",
                "expected BLANKLINE at end of recipe title after FULLSTOP".into(),
            );
            self.errored = true;
            should_continue = false;
        }

        if !should_continue {
            return;
        }

        // this loop eats all the subtokens in the comments after the title
        while let Some(st) = self.peek() {
            if_chain! {
                if let SubToken { kind: SubTokenKind::BlankLine, .. } = st;
                if let Some(SubToken { kind: SubTokenKind::Word(s), .. }) = self.peek_nth(1);
                if s == "Ingredients" || s == "Method";
                if let Some(SubToken { kind: SubTokenKind::FullStop, line: line1, .. }) = self.peek_nth(2);
                if let Some(SubToken { line: line2, .. }) = self.peek_nth(3);
                if line2 > line1;
                then {
                    return;
                }
            }

            self.advance();
        }
    }

    fn advance(&mut self) -> SubToken<'a> {
        let st = self.subtokens[self.current];
        self.current += 1;

        st
    }

    fn peek(&self) -> Option<SubToken<'a>> {
        self.peek_nth(0)
    }

    fn peek_nth(&self, n: usize) -> Option<SubToken<'a>> {
        if self.current + n >= self.subtokens.len() {
            None
        } else {
            Some(self.subtokens[self.current + n])
        }
    }

    fn current_range(&self) -> (usize, usize) {
        let start = self.subtokens[self.start].range.0;
        let end = self.subtokens[self.current].range.1;

        (start, end)
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            line: self.line,
        })
    }

    fn invalid_char(&mut self, c: char) {
        crate::report_error(self.line, "syntax ", format!("invalid character: '{}'", c));
        self.errored = true;
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.subtokens.len()
    }
}

lazy_static! {
    static ref SINGLE_KEYWORDS: HashMap<String, TokenKind> = {
        use TokenKind::*;

        let mut m = HashMap::new();

        m.insert("Ingredients".into(), Ingredients);
        m.insert("Method".into(), Method);
        m.insert("g".into(), DryMeasure);
        m.insert("kg".into(), DryMeasure);
        m.insert("pinch".into(), DryMeasure);
        m.insert("pinches".into(), DryMeasure);
        m.insert("ml".into(), LiquidMeasure);
        m.insert("l".into(), LiquidMeasure);
        m.insert("dash".into(), LiquidMeasure);
        m.insert("dashes".into(), LiquidMeasure);
        m.insert("cup".into(), AmbiguousMeasure);
        m.insert("cups".into(), AmbiguousMeasure);
        m.insert("teaspoon".into(), AmbiguousMeasure);
        m.insert("teaspoons".into(), AmbiguousMeasure);
        m.insert("tablespoon".into(), AmbiguousMeasure);
        m.insert("tablespoons".into(), AmbiguousMeasure);
        m.insert("heaped".into(), MeasureType);
        m.insert("level".into(), MeasureType);
        m.insert("Take".into(), Take);
        m.insert("Put".into(), Put);
        m.insert("into".into(), Into);
        m.insert("Fold".into(), Fold);
        m.insert("Add".into(), Add);
        m.insert("to".into(), To);
        m.insert("Remove".into(), Remove);
        m.insert("Combine".into(), Combine);
        m.insert("Divide".into(), Divide);
        m.insert("Liquefy".into(), Liquefy);
        m.insert("the".into(), The);
        m.insert("Stir".into(), Stir);
        m.insert("for".into(), For);
        m.insert("minutes".into(), Minutes);
        m.insert("Mix".into(), Mix);
        m.insert("well".into(), Well);
        m.insert("Clean".into(), Clean);
        m.insert("Pour".into(), Pour);
        m.insert("until".into(), Until);
        m.insert("Refrigerate".into(), Refrigerate);
        m.insert("hours".into(), Hours);
        m.insert("Serves".into(), Serves);

        m
    };
    static ref DOUBLE_KEYWORDS: HashMap<(String, String), TokenKind> = {
        use TokenKind::*;

        let mut m = HashMap::new();

        m.insert(("from".into(), "refrigerator".into()), FromRefrigerator);
        m.insert(("mixing".into(), "bowl".into()), MixingBowl);
        m.insert(("dry".into(), "ingredients".into()), DryIngredients);
        m.insert(("contents".into(), "of".into()), ContentsOf);
        m.insert(("baking".into(), "dish".into()), BakingDish);
        m.insert(("Set".into(), "aside".into()), SetAside);
        m.insert(("Serve".into(), "with".into()), ServeWith);

        m
    };
}
