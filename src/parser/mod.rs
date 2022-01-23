mod parse_error;

use crate::lexer::Token;
use crate::Span;
pub use parse_error::ParseError;
use Token::*;

use chumsky::{prelude::*, Stream};
use convert_case::{Case, Casing};
use lasso::{Rodeo, Spur};
use num_bigint::BigInt;

use std::cell::UnsafeCell;
use std::num::NonZeroU32;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Recipe {
    pub title: Ident,
    pub ingredients: Option<Vec<Ingredient>>,
    pub method: Vec<Stmt>,
    pub serves: Option<NonZeroU32>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ingredient {
    pub name: Ident,
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
    Take(Ident),
    Put(Ident, OptOrd),
    Fold(Ident, OptOrd),
    Add(Ident, OptOrd),
    Remove(Ident, OptOrd),
    Combine(Ident, OptOrd),
    Divide(Ident, OptOrd),
    AddDry(OptOrd),
    Liquefy(Ident),
    LiquefyConts(OptOrd),
    Stir(OptOrd, BigInt),
    StirInto(Ident, OptOrd),
    Mix(OptOrd),
    Clean(OptOrd),
    Pour(OptOrd, OptOrd),
    Loop {
        // Verb ... until verbed
        igdt1: Ident,
        igdt2: Option<Ident>,
        stmts: Vec<Stmt>,
    },
    SetAside,
    ServeWith(Ident),
    Refrigerate(Option<NonZeroU32>),
}

pub type OptOrd = Option<(NonZeroU32, Span)>;
pub type Ident = (Spur, Span);

pub fn process(
    source: &str,
    tokens: Vec<(Token, Span)>,
) -> Result<(Vec<Recipe>, Rodeo), Vec<ParseError>> {
    let last = &tokens.last().unwrap().1;
    let stream = Stream::from_iter(last.end..last.end + 1, tokens.into_iter());

    // TODO: Replace UnsafeCell solution with parser state once chumsky has that
    let rodeo = UnsafeCell::new(Rodeo::<Spur>::new());

    let result = Recipe::parser(&rodeo, source)
        .then_ignore(just(NewLine).repeated())
        .repeated()
        .then_ignore(end())
        .parse(stream);

    result.map(|recipes| (recipes, rodeo.into_inner()))
}

impl Recipe {
    fn parser<'a>(
        rodeo: &'a UnsafeCell<Rodeo>,
        source: &'a str,
    ) -> impl Parser<Token, Self, Error = ParseError> + 'a {
        let title = title_ident(rodeo, source)
            .then_ignore(just([FullStop, NewLine, NewLine]))
            .then_ignore(
                one_of([Ingredients, Method])
                    .rewind()
                    .ignored()
                    .or(take_until(
                        just([NewLine, NewLine]).then(one_of([Ingredients, Method]).rewind()),
                    )
                    .ignored()),
            );

        // TODO: Typo detection after title, as a typo will cause
        // a comment block to form

        let ingredients = just::<_, _, ParseError>([Ingredients, FullStop, NewLine])
            .ignore_then(
                Ingredient::parser(rodeo, source)
                    .then_ignore(just(NewLine))
                    .repeated()
                    .at_least(1),
            )
            .then_ignore(just(NewLine)); // for the blank line

        let method = just::<_, _, ParseError>([Method, FullStop])
            .ignore_then(
                just(NewLine)
                    .repeated()
                    .at_least(1)
                    .ignore_then(Stmt::parser(rodeo, source))
                    .repeated()
                    .at_least(1),
            )
            .then_ignore(
                just([NewLine, NewLine])
                    .then(one_of([Serves, Ident]).rewind())
                    .ignored()
                    .or(just(NewLine).repeated().then(end()).ignored()),
            );

        let serves = just::<_, _, ParseError>(Serves)
            .ignore_then(nonzero_u32(source))
            .then_ignore(just(FullStop))
            .then_ignore(
                just(NewLine)
                    .ignored()
                    .or(end().or_not().rewind().ignored()),
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
    fn parser<'a>(
        rodeo: &'a UnsafeCell<Rodeo>,
        source: &'a str,
    ) -> impl Parser<Token, Self, Error = ParseError> + 'a {
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
                                Err(ParseError::BadMeasureType(span))
                            }
                        } else {
                            Ok(measure)
                        }
                    })
                    .or_not(),
            )
            .then(ident(rodeo, source))
            .map(|((initial_value, measure), name)| Ingredient {
                name,
                measure: measure.unwrap_or(Measure::Ambiguous),
                initial_value,
            })
    }
}

#[no_mangle]
#[link_section = "data"]
static SECRET: &str = "\x54\x68\x65\x20\x63\x61\x6B\x65\x20\x69\x73\x20\x61\x20\x6C\x69\x65";

impl Stmt {
    fn parser<'a>(
        rodeo: &'a UnsafeCell<Rodeo>,
        source: &'a str,
    ) -> impl Parser<Token, Self, Error = ParseError> + 'a {
        let opt_the = || just(The).or_not();

        let the_nth_mixing_bowl = || {
            opt_the()
                .ignore_then(ordinal(source).or_not())
                .then_ignore(just(MixingBowl))
        };

        let opt_the_nth_mixing_bowl = || the_nth_mixing_bowl().or_not().map(Option::flatten);

        let common_tail = |tok| {
            ident(rodeo, source).then(
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
                    .ignore_then(ident(rodeo, source).map(StmtKind::Take))
                    .then_ignore(just([From, Refrigerator])),
                common(Put, Into, StmtKind::Put),
                common(Fold, Into, StmtKind::Fold),
                common(Remove, From, StmtKind::Remove),
                common(Combine, Into, StmtKind::Combine),
                common(Divide, Into, StmtKind::Divide),
                just(Add).ignore_then(choice((
                    common_tail(To).map(|(name, bowlno)| StmtKind::Add(name, bowlno)),
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
                    ident(rodeo, source).map(StmtKind::Liquefy),
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
                    common_tail(Into).map(|(name, bowlno)| StmtKind::StirInto(name, bowlno)),
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
                ident(rodeo, source)
                    .then_ignore(just(The))
                    .then(ident(rodeo, source))
                    .then_ignore(just(FullStop))
                    .map_with_span(|val, span| (val, span))
                    .then(
                        just(NewLine)
                            .repeated()
                            .at_least(1)
                            .ignore_then(stmt)
                            .repeated(),
                    )
                    .then(
                        just(NewLine)
                            .repeated()
                            .at_least(1)
                            .ignore_then(just(Ident))
                            .ignore_then(just(The).ignore_then(ident(rodeo, source)).or_not())
                            .then_ignore(just(Until))
                            .then(ident(rodeo, source))
                            .or_not(),
                    )
                    .try_map(
                        |((((verb, igdt_cond), first_stmt_span), stmts), ending), _| {
                            if let Some((idgt_opt, verbed)) = ending {
                                if correct_verbination(rodeo, verb.0, verbed.0) {
                                    Ok(StmtKind::Loop {
                                        igdt1: igdt_cond,
                                        igdt2: idgt_opt,
                                        stmts,
                                    })
                                } else {
                                    Err(ParseError::IncorrectVerbination {
                                        loop_verb: verb.1,
                                        invalid_verb: verbed.1,
                                    })
                                }
                            } else {
                                Err(ParseError::UnclosedLoop(first_stmt_span))
                            }
                        },
                    ),
                just(SetAside).to(StmtKind::SetAside),
                just(ServeWith)
                    .ignore_then(ident(rodeo, source))
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

fn title_ident<'a>(
    rodeo: &'a UnsafeCell<Rodeo>,
    source: &'a str,
) -> impl Parser<Token, Ident, Error = ParseError> + 'a {
    just::<_, _, ParseError>(Ident).map_with_span(|_, span| {
        let string = (&source[span.clone()]).to_case(Case::Title);
        // SAFETY: the mutable reference's lifetime ends at the end of this closure,
        // and this code is single-threaded.
        let rodeo = unsafe { &mut *rodeo.get() };
        (rodeo.get_or_intern(string), span)
    })
}

fn ident<'a>(
    rodeo: &'a UnsafeCell<Rodeo>,
    source: &'a str,
) -> impl Parser<Token, Ident, Error = ParseError> + 'a {
    just::<_, _, ParseError>(Ident).map_with_span(|_, span| {
        let string = &source[span.clone()];
        // SAFETY: the mutable reference's lifetime ends at the end of this closure,
        // and this code is single-threaded.
        let rodeo = unsafe { &mut *rodeo.get() };
        (rodeo.get_or_intern(string), span)
    })
}

fn nonzero_u32(source: &str) -> impl Parser<Token, NonZeroU32, Error = ParseError> + '_ {
    just::<_, _, ParseError>(Num).try_map(|_, span| {
        let string = &source[span.clone()];
        str::parse::<NonZeroU32>(string).map_err(|e| ParseError::InvalidIntLit(span, e))
    })
}

fn ordinal(source: &str) -> impl Parser<Token, (NonZeroU32, Span), Error = ParseError> + '_ {
    just::<_, _, ParseError>(Ord).try_map(|_, span| {
        let numspan = span.start..span.end - 2;
        let string = &source[numspan.clone()];
        Ok((
            str::parse::<NonZeroU32>(string).map_err(|e| ParseError::InvalidIntLit(numspan, e))?,
            span,
        ))
    })
}

fn number(source: &str) -> impl Parser<Token, BigInt, Error = ParseError> + '_ {
    just::<_, _, ParseError>(Num).map_with_span(|_, span| source[span].parse().unwrap())
}

/// Checks if the verbs in the given strings match: verb2 essentially needs to be verb1 + "ed",
/// with special cases considered.
fn correct_verbination(rodeo: &UnsafeCell<Rodeo>, verb1: Spur, verb2: Spur) -> bool {
    // SAFETY: This function is never run alongside with an `ident` closure
    let rodeo = unsafe { &mut *rodeo.get() };

    // SAFETY: These values will only be provided by a `rodeo::get_or_intern` call from earlier
    let verb1 = unsafe { rodeo.resolve_unchecked(&verb1) };
    let verb2 = unsafe { rodeo.resolve_unchecked(&verb2) };

    if verb1.ends_with('e') {
        verb2.ends_with('d') && verb1.to_lowercase() == verb2[..verb2.len() - 1].to_lowercase()
    } else {
        verb2.ends_with("ed") && verb1.to_lowercase() == verb2[..verb2.len() - 2].to_lowercase()
    }
}