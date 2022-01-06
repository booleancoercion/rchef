use crate::lexer::Token;
use crate::{RChefError, Result, Span};
use Token::*;

use chumsky::{prelude::*, Stream};
use num_bigint::BigInt;

use std::num::NonZeroU32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Recipe {
    pub title: String,
    pub ingredients: Option<Vec<Ingredient>>,
    pub method: Vec<Stmt>,
    pub serves: Option<NonZeroU32>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ingredient {
    pub name: String,
    pub measure: Measure,
    pub initial_value: Option<BigInt>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Measure {
    Dry,
    Liquid,
    Ambiguous,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
    Stir(BowlNo, usize),
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct IgdtBowl(pub String, pub BowlNo);

pub fn process(
    source: &str,
    tokens: Vec<(Token, Span)>,
) -> Result<Vec<Recipe>> {
    let last = &tokens.last().unwrap().1;
    let stream = Stream::from_iter(last.end..last.end + 1, tokens.into_iter());
    Ok(Recipe::parser(source)
        .then_ignore(
            just([NewLine, NewLine])
                .ignored()
                .or(just(NewLine).repeated().then(end()).ignored()),
        )
        .repeated()
        .parse(stream)
        .unwrap())
}

impl Recipe {
    fn parser(
        source: &str,
    ) -> impl Parser<Token, Self, Error = Simple<Token>> + '_ {
        let title = just::<_, _, Simple<Token>>(Ident)
            .map_with_span(|_, span| source[span].to_owned())
            .then_ignore(just(FullStop))
            .then_ignore(take_until(
                just([NewLine, NewLine])
                    .then(one_of([Ingredients, Method]).rewind()),
            ));

        title.map(|x| {
            println!("{:?}", x);
            todo!()
        })
    }
}

/// Checks if the verbs in the given strings match: verb2 essentially needs to be verb1 + "ed",
/// with special cases considered.
fn correct_verbination(verb1: &str, verb2: &str) -> bool {
    if verb1.ends_with('e') {
        verb2.ends_with('d')
            && verb1.to_lowercase() == verb2[..verb2.len() - 1].to_lowercase()
    } else {
        verb2.ends_with("ed")
            && verb1.to_lowercase() == verb2[..verb2.len() - 2].to_lowercase()
    }
}
