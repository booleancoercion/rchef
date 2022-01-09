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
    Put(String, BowlNo),
    Fold(String, BowlNo),
    Add(String, BowlNo),
    Remove(String, BowlNo),
    Combine(String, BowlNo),
    Divide(String, BowlNo),
    AddDry(BowlNo),
    Liquefy(String),
    LiquefyConts(BowlNo),
    Stir(BowlNo, BigInt),
    StirInto(String, BowlNo),
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

pub fn process(
    source: &str,
    tokens: Vec<(Token, Span)>,
) -> Result<Vec<Recipe>, Vec<Simple<Token>>> {
    let last = &tokens.last().unwrap().1;
    let stream = Stream::from_iter(last.end..last.end + 1, tokens.into_iter());
    Recipe::parser(source)
        .then_ignore(just(NewLine).repeated())
        .repeated()
        .then_ignore(end())
        .parse(stream)
}

impl Recipe {
    fn parser(
        source: &str,
    ) -> impl Parser<Token, Self, Error = Simple<Token>> + '_ {
        let title = ident(source)
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

        let method = just::<_, _, Simple<Token>>([Method, FullStop])
            .ignore_then(
                just(NewLine)
                    .repeated()
                    .at_least(1)
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
            .ignore_then(nonzero_u32(source))
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
        number(source)
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
            .then(ident(source))
            .map(|((initial_value, measure), name)| Ingredient {
                name, measure: measure.unwrap_or(Measure::Ambiguous), initial_value
            })
    }
}

impl Stmt {
    fn parser(
        source: &str,
    ) -> impl Parser<Token, Self, Error = Simple<Token>> + '_ {
        let opt_the = || just(The).or_not();

        let the_nth_mixing_bowl = || {
            opt_the()
                .ignore_then(ordinal(source).or_not())
                .then_ignore(just(MixingBowl))
        };

        let opt_the_nth_mixing_bowl =
            || the_nth_mixing_bowl().or_not().map(Option::flatten);

        let common_tail = |tok| {
            ident(source).then(
                just(tok)
                    .ignore_then(the_nth_mixing_bowl())
                    .or_not()
                    .map(Option::flatten),
            )
        };

        let common = |tok1, tok2, func: fn(_, _) -> _| {
            just(tok1)
                .ignore_then(common_tail(tok2))
                .map(move |(name, bowlno)| func(name, bowlno))
        };

        recursive(|stmt| {
            choice((
                just(Take)
                    .ignore_then(ident(source).map(StmtKind::Take))
                    .then_ignore(just([From, Refrigerator])),
                common(Put, Into, StmtKind::Put),
                common(Fold, Into, StmtKind::Fold),
                common(Remove, From, StmtKind::Remove),
                common(Combine, Into, StmtKind::Combine),
                common(Divide, Into, StmtKind::Divide),
                just(Add).ignore_then(choice((
                    common_tail(To)
                        .map(|(name, bowlno)| StmtKind::Add(name, bowlno)),
                    just(DryIngredients)
                        .ignore_then(
                            just(To)
                                .ignore_then(the_nth_mixing_bowl())
                                .or_not()
                                .map(Option::flatten),
                        )
                        .map(StmtKind::AddDry),
                ))),
                just(Liquefy).ignore_then(choice((
                    ident(source).map(StmtKind::Liquefy),
                    just([ContentsOf])
                        .ignore_then(the_nth_mixing_bowl())
                        .map(StmtKind::LiquefyConts),
                ))),
                just(Stir).ignore_then(choice((
                    opt_the_nth_mixing_bowl()
                        .then_ignore(just(For))
                        .then(number(source))
                        .then_ignore(just(Minutes))
                        .map(|(bowlno, number)| StmtKind::Stir(bowlno, number)),
                    common_tail(Into)
                        .map(|(name, bowlno)| StmtKind::StirInto(name, bowlno)),
                ))),
                just(Mix)
                    .ignore_then(opt_the_nth_mixing_bowl())
                    .then_ignore(just(Well))
                    .map(StmtKind::Mix),
                just(Clean)
                    .ignore_then(opt_the_nth_mixing_bowl())
                    .map(StmtKind::Clean),
                just([Pour, ContentsOf])
                    .ignore_then(the_nth_mixing_bowl())
                    .then_ignore(just(Into).then(opt_the()))
                    .then(ordinal(source).or_not())
                    .then_ignore(just(BakingDish))
                    .map(|(bowlno, dishno)| StmtKind::Pour(bowlno, dishno)),
                ident(source)
                    .map_with_span(|verb, span| (verb, span))
                    .then_ignore(just(The))
                    .then(ident(source))
                    .then_ignore(just(FullStop))
                    .then(
                        just(NewLine)
                            .repeated()
                            .at_least(1)
                            .ignore_then(stmt)
                            .repeated(),
                    )
                    .then_ignore(just(NewLine).repeated().at_least(1))
                    .then_ignore(just(Ident))
                    .then(just(The).ignore_then(ident(source)).or_not())
                    .then_ignore(just(Until))
                    .then(
                        ident(source).map_with_span(|verb, span| (verb, span)),
                    )
                    .try_map(
                        |((((verb, igdt_cond), stmts), idgt_opt), verbed),
                         _| {
                            if correct_verbination(&verb.0, &verbed.0) {
                                Ok(StmtKind::Loop {
                                    igdt1: igdt_cond,
                                    igdt2: idgt_opt,
                                    stmts,
                                })
                            } else {
                                // TODO: Use a custom error type for this!
                                Err(Simple::unclosed_delimiter(
                                    verb.1, Ident, verbed.1, Ident, None,
                                ))
                            }
                        },
                    ),
                just(SetAside).to(StmtKind::SetAside),
                just(ServeWith)
                    .ignore_then(ident(source))
                    .map(StmtKind::ServeWith),
                just(Refrigerate)
                    .ignore_then(
                        just(For)
                            .ignore_then(nonzero_u32(source))
                            .then_ignore(just(Hours))
                            .or_not(),
                    )
                    .map(StmtKind::Refrigerate),
            ))
            .then_ignore(just(FullStop))
            .map_with_span(|kind, span| Stmt { kind, span })
        })
    }
}

fn ident(
    source: &str,
) -> impl Parser<Token, String, Error = Simple<Token>> + '_ {
    just::<_, _, Simple<Token>>(Ident)
        .map_with_span(|_, span| source[span].to_owned())
}

fn nonzero_u32(
    source: &str,
) -> impl Parser<Token, NonZeroU32, Error = Simple<Token>> + '_ {
    just::<_, _, Simple<Token>>(Num).try_map(|_, span| {
        let string = &source[span.clone()];
        str::parse::<NonZeroU32>(string)
            .map_err(|e| Simple::custom(span, format!("{}", e)))
    })
}

fn ordinal(
    source: &str,
) -> impl Parser<Token, NonZeroU32, Error = Simple<Token>> + '_ {
    just::<_, _, Simple<Token>>(Ord).try_map(|_, span| {
        let span = span.start..span.end - 2;
        let string = &source[span.clone()];
        str::parse::<NonZeroU32>(string)
            .map_err(|e| Simple::custom(span, format!("{}", e)))
    })
}

fn number(
    source: &str,
) -> impl Parser<Token, BigInt, Error = Simple<Token>> + '_ {
    just::<_, _, Simple<Token>>(Num)
        .map_with_span(|_, span| source[span].parse().unwrap())
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
