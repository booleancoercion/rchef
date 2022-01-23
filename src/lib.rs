mod direct_interpreter;
mod lexer;
mod parser;

use parser::{ParseError, StmtKind};

pub type Span = std::ops::Range<usize>;

use ariadne::{Color, Fmt, Label, Report, ReportKind};
use itertools::Itertools;
use lasso::Rodeo;
use lexer::Token;
use thiserror::Error;

use std::num::NonZeroU32;
use std::result::Result as StdResult;
use std::{fs, io};

pub type Result<T> = StdResult<T, RChefError>;

#[derive(Debug, Error)]
pub enum RChefError {
    #[error("io error while running: {0}")]
    Io(#[from] io::Error),

    #[error("lex error")]
    Lex,

    #[error("parse error")]
    Parse,

    #[error("runtime error")]
    Runtime,
}

pub fn run(filename: &str, spaced: bool) -> Result<()> {
    let mut source = fs::read_to_string(filename)?;
    source.retain(|c| c != '\r');
    let tokens = lexer::process(&source);
    let (recipes, rodeo) = match parser::process(&source, tokens) {
        Ok(recipes) => recipes,
        Err(errors) => {
            errors
                .into_iter()
                .for_each(|error| handle_parse_error(error, filename, &source));
            return Err(RChefError::Parse);
        }
    };

    // TODO: Add lint for duplicate recipe names

    let mut errored = false;
    for recipe in &recipes {
        errored |= ensure_consistent_ordinals(&rodeo, recipe, filename, &source).is_err();
    }
    if errored {
        return Err(RChefError::Parse);
    }

    direct_interpreter::run(recipes, spaced)
}

fn handle_parse_error(error: ParseError, filename: &str, source: &str) {
    let builder = Report::build(ReportKind::Error, filename.to_owned(), error.offset());

    use ParseError::*;
    let report = match error {
        Unexpected {
            span,
            expected,
            found,
        } => {
            if found == Some(Token::Error) {
                builder.with_message("illegal token").with_label(
                    Label::new((filename.to_owned(), span))
                        .with_message("here")
                        .with_color(Color::Red),
                )
            } else {
                builder.with_message("unexpected token").with_label(
                    Label::new((filename.to_owned(), span))
                        .with_message(format!(
                            "expected {}, found {}",
                            expected
                                .into_iter()
                                .map(pretty_token_opt)
                                .map(|s| s.fg(Color::Cyan))
                                .join(", "),
                            pretty_token_opt(found).fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                )
            }
        }

        InvalidIntLit(span, err) => builder
            .with_message("invalid integer")
            .with_label(
                Label::new((filename.to_owned(), span))
                    .with_message("here")
                    .with_color(Color::Red),
            )
            .with_note(format!("parsing yielded: {}", err)),

        IncorrectVerbination {
            loop_verb,
            invalid_verb,
        } => builder
            .with_message("incompatible loop verbs")
            .with_label(
                Label::new((filename.to_owned(), loop_verb))
                    .with_message("beginning verb here")
                    .with_color(Color::Green),
            )
            .with_label(
                Label::new((filename.to_owned(), invalid_verb))
                    .with_message("incompatible verb here")
                    .with_color(Color::Red),
            )
            .with_note(format!(
                "loop verbs must make sense together - e.g. '{} ... until {}ed.'",
                "Climb".fg(Color::Green),
                "climb".fg(Color::Green)
            )),

        UnclosedLoop(span) => builder.with_message("unclosed loop").with_label(
            Label::new((filename.to_owned(), span))
                .with_message("this loop is opened but never closed")
                .with_color(Color::Red),
        ),

        BadMeasureType(span) => builder
            .with_message("measure type cannot be used with a dry or liquid measure")
            .with_label(
                Label::new((filename.to_owned(), span))
                    .with_message("measure type used here")
                    .with_color(Color::Red),
            ),
    }
    .finish();

    report
        .eprint(ariadne::sources([(filename.to_owned(), source)]))
        .unwrap();
}

fn pretty_token_opt(tok: Option<Token>) -> String {
    if let Some(tok) = tok {
        format!("{:?}", tok)
    } else {
        "EOF".to_owned()
    }
}

macro_rules! consistency {
    ($filename:ident, $title:expr, $source:ident, $implicit:ident, $explicit:ident, $word:literal) => {{
        if !($implicit.is_empty() || $explicit.is_empty()) {
            Report::build(ReportKind::Error, $filename.to_owned(), $implicit[0].start)
                .with_message(concat!(
                    "recipes with more than two ",
                    $word,
                    " cannot refer to ",
                    $word,
                    " implicitly"
                ))
                .with_label(
                    Label::new(($filename.to_owned(), $implicit[0].clone()))
                        .with_color(Color::Red)
                        .with_message("implicit reference used here"),
                )
                .with_label(
                    Label::new(($filename.to_owned(), $explicit[0].clone()))
                        .with_color(Color::Red)
                        .with_message("explicit reference used here"),
                )
                .with_note(format!("in recipe '{}'", $title))
                .finish()
                .eprint(ariadne::sources([($filename.to_owned(), $source)]))
                .unwrap();

            true
        } else {
            false
        }
    }};
}

fn ensure_consistent_ordinals(
    rodeo: &Rodeo,
    recipe: &parser::Recipe,
    filename: &str,
    source: &str,
) -> Result<()> {
    let mut implicit_bowls = vec![];
    let mut explicit_bowls = vec![];

    let mut implicit_dishes = vec![];
    let mut explicit_dishes = vec![];

    for stmt in &recipe.method {
        let helper = |opt: &Option<(NonZeroU32, Span)>,
                      explicit: &mut Vec<Span>,
                      implicit: &mut Vec<Span>| {
            if let Some((num, span)) = opt {
                if u32::from(*num) > 1 {
                    explicit.push(span.clone())
                }
            } else {
                implicit.push(stmt.span.clone())
            }
        };

        match &stmt.kind {
            StmtKind::Put(_, bowl)
            | StmtKind::Fold(_, bowl)
            | StmtKind::Add(_, bowl)
            | StmtKind::Remove(_, bowl)
            | StmtKind::Combine(_, bowl)
            | StmtKind::Divide(_, bowl)
            | StmtKind::AddDry(bowl)
            | StmtKind::LiquefyConts(bowl)
            | StmtKind::Stir(bowl, _)
            | StmtKind::StirInto(_, bowl)
            | StmtKind::Mix(bowl)
            | StmtKind::Clean(bowl) => helper(bowl, &mut explicit_bowls, &mut implicit_bowls),

            StmtKind::Pour(bowl, dish) => {
                helper(bowl, &mut explicit_bowls, &mut implicit_bowls);
                helper(dish, &mut explicit_dishes, &mut implicit_dishes);
            }

            _ => {}
        }
    }

    let title = rodeo.resolve(&recipe.title.0);

    let mut error = consistency!(
        filename,
        title,
        source,
        implicit_bowls,
        explicit_bowls,
        "bowls"
    );

    error |= consistency!(
        filename,
        title,
        source,
        implicit_dishes,
        explicit_dishes,
        "dishes"
    );

    if error {
        Err(RChefError::Parse)
    } else {
        Ok(())
    }
}
