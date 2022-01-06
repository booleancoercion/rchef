use crate::lexer::Token;
use crate::Span;
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

pub fn process(source: &str, tokens: Vec<(Token, Span)>) -> Vec<Recipe> {
    let last = &tokens.last().unwrap().1;
    let stream = Stream::from_iter(last.end..last.end + 1, tokens.into_iter());
    Recipe::parser(source)
        .then_ignore(just(NewLine).repeated())
        .repeated()
        .then_ignore(end())
        .parse(stream)
        .unwrap()
}

impl Recipe {
    fn parser(
        source: &str,
    ) -> impl Parser<Token, Self, Error = Simple<Token>> + '_ {
        let title = just::<_, _, Simple<Token>>(Ident)
            .map_with_span(map_string(source))
            .then_ignore(just([FullStop, NewLine, NewLine]))
            .then_ignore(
                one_of([Ingredients, Method])
                    .rewind()
                    .ignored()
                    .or(take_until(just([NewLine, NewLine])).ignored()),
            ); // comments are a SINGLE PARAGRAPH

        // TODO: Typo detection after title, as a typo will cause
        // a comment block to form

        let ingredients =
            just::<_, _, Simple<Token>>([Ingredients, FullStop, NewLine])
                .ignore_then(
                    Ingredient::parser(source)
                        .then_ignore(just(NewLine))
                        .repeated()
                        .at_least(1),
                )
                .then_ignore(just(NewLine)); // for the blank line

        let method = just::<_, _, Simple<Token>>([Method, FullStop, NewLine])
            .ignore_then(
                just(NewLine)
                    .repeated()
                    .ignore_then(Stmt::parser(source))
                    .repeated()
                    .at_least(1),
            )
            .then_ignore(
                just([NewLine, NewLine])
                    .then(one_of([Serves, Ident]).rewind())
                    .ignored()
                    .or(just(NewLine).repeated().then(end()).ignored()),
            );

        let serves = just::<_, _, Simple<Token>>(Serves)
            .ignore_then(just(Num).try_map(try_map_nonzero_u32(source)))
            .then_ignore(just(FullStop))
            .then_ignore(
                just([NewLine, NewLine])
                    .then(just(Ident).rewind())
                    .ignored()
                    .or(just(NewLine).repeated().then(end()).ignored()),
            );

        title
            .then(ingredients.or_not())
            .then(method)
            .then(serves.or_not())
            .map(|(((title, ingredients), method), serves)| Recipe {
                title,
                ingredients,
                method,
                serves,
            })
    }
}

impl Ingredient {
    fn parser(
        source: &str,
    ) -> impl Parser<Token, Self, Error = Simple<Token>> + '_ {
        just(Num)
            .map_with_span(number(source))
            .or_not()
            .then(
                just(MeasureType)
                    .or_not()
                    .then(choice((
                        just(DryMeasure).to(Measure::Dry),
                        just(LiquidMeasure).to(Measure::Liquid),
                        just(AmbiguousMeasure).to(Measure::Ambiguous),
                    )))
                    .try_map(|(mtype, measure), span| {
                        if mtype.is_some() {
                            if measure == Measure::Ambiguous {
                                Ok(Measure::Dry)
                            } else {
                                Err(Simple::custom(span, "Measure type cannot be used with dry or liquid measure"))
                            }
                        } else {
                            Ok(measure)
                        }
                    }).or_not(),
            )
            .then(just(Ident).map_with_span(map_string(source)))
            .map(|((initial_value, measure), name)| Ingredient {
                name, measure: measure.unwrap_or(Measure::Ambiguous), initial_value
            })
    }
}

impl Stmt {
    fn parser(
        source: &str,
    ) -> impl Parser<Token, Self, Error = Simple<Token>> + '_ {
        one_of([
            Take,
            Put,
            Fold,
            Add,
            Remove,
            Combine,
            Divide,
            Liquefy,
            Stir,
            Mix,
            Clean,
            Pour,
            Ident,
            SetAside,
            ServeWith,
            Refrigerate,
        ])
        .ignore_then(take_until(just(FullStop)).to(StmtKind::SetAside)) // placeholder
        .map_with_span(|kind, span| Stmt { kind, span })
    }
}

fn map_string(source: &str) -> impl Fn(Token, Span) -> String + '_ {
    |_, span| source[span].to_owned()
}

fn try_map_nonzero_u32(
    source: &str,
) -> impl Fn(Token, Span) -> crate::StdResult<NonZeroU32, Simple<Token>> + '_ {
    |_, span| {
        let string = &source[span.clone()];
        str::parse::<NonZeroU32>(string)
            .map_err(|e| Simple::custom(span, format!("{}", e)))
    }
}

fn number(source: &str) -> impl Fn(Token, Span) -> BigInt + '_ {
    |_, span| source[span].parse().unwrap()
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
