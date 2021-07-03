use crate::lexer::{Token, TokenKind};
use crate::{RChefError, Result};
use TokenKind::*;

use std::array::IntoIter;
use std::convert::TryInto;
use std::iter::Peekable;
use std::num::NonZeroU32;

#[derive(Clone, Debug)]
pub struct Recipe {
    pub title: String,
    pub ingredients: Option<Vec<Ingredient>>,
    pub method: Vec<Stmt>,
    pub serves: Option<NonZeroU32>,
}

#[derive(Clone, Debug)]
pub struct Ingredient {
    pub name: String,
    pub measure: Measure,
    pub initial_value: Option<i64>,
}

#[derive(Clone, Copy, Debug)]
pub enum Measure {
    Dry,
    Liquid,
    Ambiguous,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub line: u32,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Take(String),
    Put(IgdtBowl),
    Fold(IgdtBowl),
    Add(IgdtBowl),
    Remove(IgdtBowl),
    Combine(IgdtBowl),
    Divide(IgdtBowl),
    AddDry(BowlNo),
    Liquefy(String),
    LiquefyConts(BowlNo),
    Stir(BowlNo, u32),
    StirInto(IgdtBowl),
    Mix(BowlNo),
    Clean(BowlNo),
    Pour(BowlNo, BowlNo),
    Loop {
        // Verb ... until verbed
        igdt1: String,
        igdt2: Option<String>,
        stmts: Vec<Stmt>,
    },
    SetAside,
    ServeWith(String),
    Refrigerate(Option<NonZeroU32>),
}

pub type BowlNo = Option<NonZeroU32>;

/// Shortcut for a (IngredientName, MixingBowlNumber) type,
/// heavily used in arithmetic statements.
#[derive(Clone, Debug)]
pub struct IgdtBowl(pub String, pub BowlNo);

pub fn process(tokens: Vec<Token>) -> Result<Vec<Recipe>> {
    Parser::new(tokens.into_iter()).process()
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    line: u32,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
            line: 0,
        }
    }

    fn process(mut self) -> Result<Vec<Recipe>> {
        let mut recipes = vec![];

        let mut errored = false;

        while !self.is_at_end() {
            let recipe = self.parse_recipe();
            if let Ok(recipe) = recipe {
                recipes.push(recipe);
            } else {
                errored = true;
            }

            while !self.matches(|k| *k == BlankLine) && !self.is_at_end() {
                self.advance();
            }
        }

        if errored {
            Err(RChefError::Parse)
        } else {
            Ok(recipes)
        }
    }

    fn parse_recipe(&mut self) -> Result<Recipe> {
        self.eat_blanklines();

        let title = self.expect_ident()?;
        self.expect_fs_bl()?;
        self.eat_blanklines();

        let ingredients = match self.advance() {
            Some(t) if t.kind == Ingredients => {
                let ingredients = self.parse_ingredients()?;

                Some(ingredients)
            }
            Some(t) if t.kind == Method => None,
            t => {
                crate::report_error(
                    self.line,
                    "syntax ",
                    format!("expected INGREDIENTS or METHOD, found {:?}", t),
                );
                return Err(RChefError::Parse);
            }
        };

        self.expect(Method)?;
        let method = self.parse_method()?;

        let serves = match self.advance() {
            Some(t) if t.kind == Serves => {
                let n = self.expect_nonzero_u32()?;
                self.expect_fs()?;

                Some(n)
            }
            Some(t) if t.kind == BlankLine || t.kind == Eof => None,
            t => {
                crate::report_error(
                    self.line,
                    "syntax ",
                    format!("expected SERVES or BLANKLINE or EOF, found {:?}", t),
                );
                return Err(RChefError::Parse);
            }
        };

        Ok(Recipe {
            title,
            ingredients,
            method,
            serves,
        })
    }

    fn parse_ingredients(&mut self) -> Result<Vec<Ingredient>> {
        self.expect_fs()?;
        let mut ingredients = vec![];

        self.expect_nl()?;
        ingredients.push(self.parse_ingredient()?);

        while let Some(t) = self.advance() {
            match t.kind {
                NewLine => ingredients.push(self.parse_ingredient()?),
                BlankLine => return Ok(ingredients),
                k => {
                    return self.error(format!("expected NEWLINE or BLANKLINE, found {:?}", k));
                }
            }
        }

        self.error("unexpected EOF")
    }

    fn parse_ingredient(&mut self) -> Result<Ingredient> {
        let initial_value = if let Some(Number(n)) = self.peek() {
            let n = *n;
            self.advance();
            Some(n)
        } else {
            None
        };

        let measure = match self.peek() {
            Some(MeasureType) => {
                self.advance();
                self.expect(AmbiguousMeasure)?;
                Measure::Liquid
            }
            Some(AmbiguousMeasure) => {
                self.advance();
                Measure::Ambiguous
            }
            Some(LiquidMeasure) => {
                self.advance();
                Measure::Liquid
            }
            Some(DryMeasure) => {
                self.advance();
                Measure::Dry
            }
            _ => Measure::Ambiguous,
        };

        let name = self.expect_ident()?;

        Ok(Ingredient {
            name,
            initial_value,
            measure,
        })
    }

    fn parse_method(&mut self) -> Result<Vec<Stmt>> {
        self.expect_fs_nl()?;

        let mut stmts = vec![];
        let mut error = false;

        while let Some(token) = self.advance() {
            if token.kind == NewLine {
                continue;
            } else if token.kind == BlankLine || token.kind == Eof {
                break;
            }

            if let Ok(stmt) = self.parse_stmt(token) {
                stmts.push(stmt);
            } else {
                error = true;
                self.eat_upto_fullstop();
            }
        }

        if error {
            Err(RChefError::Parse)
        } else {
            Ok(stmts)
        }
    }

    fn parse_stmt(&mut self, token: Token) -> Result<Stmt> {
        // the terminal fullstop is expected at the end!
        let res = match token.kind {
            Take => {
                let ing = self.expect_ident()?;
                self.expect_multiple([From, Refrigerator])?;
                self.stmt(StmtKind::Take(ing))
            }
            Put => {
                let ing = self.expect_ident()?;
                self.expect_multiple([Into, The])?;
                let bowl = self.opt_ordinal();
                self.expect(MixingBowl)?;

                self.stmt(StmtKind::Put(IgdtBowl(ing, bowl)))
            }
            Fold => {
                let ing = self.expect_ident()?;
                self.expect_multiple([Into, The])?;
                let bowl = self.opt_ordinal();
                self.expect(MixingBowl)?;

                self.stmt(StmtKind::Fold(IgdtBowl(ing, bowl)))
            }
            Add => match self.advexp()?.kind {
                Identifier(ing) => {
                    let bowl = self.opt_word_mxbowl(To)?;
                    self.stmt(StmtKind::Add(IgdtBowl(ing, bowl)))
                }
                DryIngredients => {
                    let bowl = self.opt_word_mxbowl(To)?;
                    self.stmt(StmtKind::AddDry(bowl))
                }
                k => self.error(format!(
                    "expected IDENTIFIER or DRYINGREDIENTS, found {:?}",
                    k
                ))?,
            },
            Remove => {
                let ing = self.expect_ident()?;
                let bowl = self.opt_word_mxbowl(From)?;
                self.stmt(StmtKind::Remove(IgdtBowl(ing, bowl)))
            }
            Combine => {
                let ing = self.expect_ident()?;
                let bowl = self.opt_word_mxbowl(Into)?;
                self.stmt(StmtKind::Combine(IgdtBowl(ing, bowl)))
            }
            Divide => {
                let ing = self.expect_ident()?;
                let bowl = self.opt_word_mxbowl(Into)?;
                self.stmt(StmtKind::Divide(IgdtBowl(ing, bowl)))
            }
            Liquefy => match self.advexp()?.kind {
                Identifier(ing) => self.stmt(StmtKind::Liquefy(ing)),
                ContentsOf => {
                    self.expect(The)?;
                    let bowl = self.opt_ordinal();
                    self.expect(MixingBowl)?;

                    self.stmt(StmtKind::LiquefyConts(bowl))
                }
                k => self.error(format!("expected IDENTIFIER or CONTENTSOF, found {:?}", k))?,
            },
            Stir => match self.advexp()?.kind {
                The => {
                    let bowl = self.opt_ordinal();
                    self.expect(MixingBowl)?;
                    self.expect(For)?;
                    let num: u32 = if let Ok(n) = self.expect_number()?.try_into() {
                        n
                    } else {
                        self.error("number literal is either negative or too big")?
                    };
                    self.expect(Minutes)?;

                    self.stmt(StmtKind::Stir(bowl, num))
                }
                For => {
                    let num: u32 = if let Ok(n) = self.expect_number()?.try_into() {
                        n
                    } else {
                        self.error("number literal is either negative or too big")?
                    };
                    self.expect(Minutes)?;
                    self.stmt(StmtKind::Stir(None, num))
                }
                Identifier(ing) => {
                    self.expect_multiple([Into, The])?;
                    let bowl = self.opt_ordinal();
                    self.expect(MixingBowl)?;

                    self.stmt(StmtKind::StirInto(IgdtBowl(ing, bowl)))
                }
                k => self.error(format!("expected IDENTIFIER or THE or FOR, found {:?}", k))?,
            },
            Mix => {
                let bowl = self.opt_word_mxbowl(The)?;
                self.expect(Well)?;
                self.stmt(StmtKind::Mix(bowl))
            }
            Clean => {
                self.expect(The)?;
                let bowl = self.opt_ordinal();
                self.expect(MixingBowl)?;
                self.stmt(StmtKind::Clean(bowl))
            }
            Pour => {
                self.expect_multiple([ContentsOf, The])?;
                let bowl = self.opt_ordinal();
                self.expect_multiple([MixingBowl, Into, The])?;
                let dish = self.opt_ordinal();
                self.expect(BakingDish)?;

                self.stmt(StmtKind::Pour(bowl, dish))
            }
            Identifier(verb) => self.parse_loop(verb)?,
            SetAside => {
                self.expect_fs()?;
                self.stmt(StmtKind::SetAside)
            }
            ServeWith => {
                let recipe = self.expect_ident()?;
                self.stmt(StmtKind::ServeWith(recipe))
            }
            Refrigerate => {
                let num = if let Some(For) = self.peek() {
                    self.advance();
                    let num = self.expect_nonzero_u32()?;

                    self.expect(Hours)?;
                    Some(num)
                } else {
                    None
                };

                self.stmt(StmtKind::Refrigerate(num))
            }
            kind => self.error(format!("expected method keyword, found {:?}", kind))?,
        };
        self.expect_fs()?;

        Ok(res)
    }

    fn parse_loop(&mut self, verb: String) -> Result<Stmt> {
        self.expect(The)?;
        let ing1 = self.expect_ident()?;
        self.expect_fs()?;
        self.parse_loop_after_opening(verb, ing1)
    }

    fn parse_loop_after_opening(&mut self, verb: String, ing1: String) -> Result<Stmt> {
        let first_line = self.line;
        let mut stmts = vec![];
        let mut error = false;

        let mut ing2;
        while let Some(token) = self.advance() {
            if token.kind == NewLine {
                continue;
            } else if token.kind == BlankLine || token.kind == Eof {
                self.error("unexpected EOL or blank line")?;
            } else if let Identifier(word) = token.kind {
                ing2 = if let Some(The) = self.peek() {
                    self.advance();
                    Some(self.expect_ident()?)
                } else {
                    None
                };

                match self.advexp()?.kind {
                    Until => {
                        let verb2 = self.expect_ident()?;

                        if !correct_verbination(&verb, &verb2) {
                            crate::report_error(
                                first_line,
                                "syntax ",
                                format!("incorrect loop counterpart on line {}", self.line + 1),
                            );
                            error = true;
                        }

                        if error {
                            return Err(RChefError::Parse);
                        } else {
                            return Ok(Stmt {
                                kind: StmtKind::Loop {
                                    igdt1: ing1,
                                    igdt2: ing2,
                                    stmts,
                                },
                                line: first_line,
                            });
                        }
                    }
                    FullStop => {
                        if let Some(ing) = ing2 {
                            stmts.push(self.parse_loop_after_opening(word, ing)?);
                            self.expect_fs()?;
                        } else {
                            return self.error("expected THE, IDENTIFIER before FULLSTOP");
                        }
                    }
                    k => return self.error(format!("expected UNTIL or FULLSTOP, found {:?}", k)),
                }
            } else if let Ok(stmt) = self.parse_stmt(token) {
                stmts.push(stmt);
            } else {
                error = true;
                self.eat_upto_fullstop();
            }
        }

        Err(RChefError::Parse)
    }

    fn opt_ordinal(&mut self) -> BowlNo {
        if let Some(Ordinal(n)) = self.peek() {
            let n = *n;
            self.advance();
            Some(n)
        } else {
            None
        }
    }

    fn opt_word_mxbowl(&mut self, kind: TokenKind) -> Result<BowlNo> {
        Ok(if self.matches(|k| *k == kind) {
            self.advance();
            self.expect(The)?;
            let bowl = self.opt_ordinal();
            self.expect(MixingBowl)?;

            bowl
        } else {
            None
        })
    }

    fn stmt(&self, kind: StmtKind) -> Stmt {
        Stmt {
            line: self.line,
            kind,
        }
    }

    #[rustfmt::skip]
    fn expect_ident(&mut self) -> Result<String> {
        let t = self.advance();

        if let Some(Token { kind: Identifier(ident), .. }) = t {
            Ok(ident)
        } else {
            return self.error(format!("expected IDENTIFIER, found {:?}", t));
        }
    }

    #[rustfmt::skip]
    fn expect_number(&mut self) -> Result<i64> {
        let t = self.advance();

        if let Some(Token { kind: Number(n), .. }) = t {
            Ok(n)
        } else {
            return self.error(format!("expected NUMBER, found {:?}", t));
        }
    }

    fn expect_nonzero_u32(&mut self) -> Result<NonZeroU32> {
        let num = if let Ok(n) = self.expect_number()?.try_into() {
            n
        } else {
            self.error("number literal is either negative or too big")?
        };

        if let Some(n) = NonZeroU32::new(num) {
            Ok(n)
        } else {
            self.error("number literal is zero")?
        }
    }

    fn expect_fs_nl(&mut self) -> Result<()> {
        self.expect_fs()?;
        self.expect_nl()?;

        Ok(())
    }

    fn expect_nl(&mut self) -> Result<()> {
        self.expect(NewLine)?;

        Ok(())
    }

    fn expect_fs_bl(&mut self) -> Result<()> {
        self.expect_fs()?;
        self.expect_bl()?;

        Ok(())
    }

    fn expect_fs(&mut self) -> Result<()> {
        self.expect(FullStop)?;

        Ok(())
    }

    fn expect_bl(&mut self) -> Result<()> {
        self.expect(BlankLine)?;

        Ok(())
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if let Some(t) = self.advance() {
            if t.kind == kind {
                Ok(t)
            } else {
                self.error(format!(
                    "expected {}, found {:?}",
                    token_name(&kind),
                    t.kind
                ))
            }
        } else {
            self.error(format!("expected {}", token_name(&kind)))
        }
    }

    fn expect_multiple<const N: usize>(&mut self, kinds: [TokenKind; N]) -> Result<()> {
        let kinds = IntoIter::new(kinds);

        for kind in kinds {
            self.expect(kind)?;
        }

        Ok(())
    }

    fn error<S, D: std::fmt::Display>(&self, msg: D) -> Result<S> {
        crate::report_error(self.line, "syntax ", msg);
        Err(RChefError::Parse)
    }

    fn eat_blanklines(&mut self) {
        while self.matches(|k| *k == BlankLine) {
            self.advance();
        }
    }

    fn eat_upto_fullstop(&mut self) {
        while !self.matches(|k| *k == FullStop) {
            self.advance();
        }
        self.advance();
    }

    fn advance(&mut self) -> Option<Token> {
        let t = self.tokens.next();
        if let Some(t) = &t {
            self.line = t.line;
        }

        t
    }

    fn advexp(&mut self) -> Result<Token> {
        if let Some(t) = self.advance() {
            Ok(t)
        } else {
            self.error("unexpected EOF")
        }
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.tokens.peek().map(|t| &t.kind)
    }

    fn matches(&mut self, f: impl FnOnce(&TokenKind) -> bool) -> bool {
        if let Some(t) = self.tokens.peek() {
            f(&t.kind)
        } else {
            false
        }
    }

    fn is_at_end(&mut self) -> bool {
        let t = self.tokens.peek();

        if let Some(t) = t {
            t.kind == Eof
        } else {
            true
        }
    }
}

fn correct_verbination(verb1: &str, verb2: &str) -> bool {
    if verb1.ends_with('e') {
        verb2.ends_with('d') && verb1.to_lowercase() == verb2[..verb2.len() - 1].to_lowercase()
    } else {
        verb2.ends_with("ed") && verb1.to_lowercase() == verb2[..verb2.len() - 2].to_lowercase()
    }
}

fn token_name(kind: &TokenKind) -> &'static str {
    match kind {
        Identifier(_) => "IDENTIFIER",
        Ordinal(_) => "ORDINAL",
        Number(_) => "NUMBER",
        Ingredients => "INGREDIENTS",
        Method => "METHOD",
        DryMeasure => "DRYMEASURE",
        LiquidMeasure => "LIQUIDMEASURE",
        AmbiguousMeasure => "AMBIGUOUSMEASURE",
        MeasureType => "MEASURETYPE",
        Take => "TAKE",
        From => "FROM",
        Refrigerator => "REFRIGERATOR",
        Put => "PUT",
        Into => "INTO",
        MixingBowl => "MIXINGBOWL",
        Fold => "FOLD",
        Add => "ADD",
        To => "TO",
        Remove => "REMOVE",
        Combine => "COMBINE",
        Divide => "DIVIDE",
        DryIngredients => "DRYINGREDIENTS",
        Liquefy => "LIQUEFY",
        ContentsOf => "CONTENTSOF",
        The => "THE",
        Stir => "STIR",
        For => "FOR",
        Minutes => "MINUTES",
        Mix => "MIX",
        Well => "WELL",
        Clean => "CLEAN",
        Pour => "POUR",
        BakingDish => "BAKINGDISH",
        Until => "UNTIL",
        SetAside => "SETASIDE",
        ServeWith => "SERVEWITH",
        Refrigerate => "REFRIGERATE",
        Hours => "HOURS",
        Serves => "SERVES",
        Eof => "EOF",
        NewLine => "NEWLINE",
        BlankLine => "BLANKLINE",
        FullStop => "FULLSTOP",
    }
}
