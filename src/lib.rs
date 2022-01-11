//mod direct_interpreter;
mod lexer;
mod parser;

use parser::StmtKind;

pub type Span = std::ops::Range<usize>;

use ariadne::Color;
use ariadne::Fmt;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use chumsky::error::SimpleReason;
use chumsky::prelude::Simple;
use itertools::Itertools;
use lexer::Token;
use thiserror::Error;

use std::fs;
use std::io;
use std::num::NonZeroU32;
use std::result::Result as StdResult;

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
    let recipes = match parser::process(&source, tokens) {
        Ok(recipes) => recipes,
        Err(errors) => {
            errors
                .into_iter()
                .for_each(|error| handle_parse_error(error, filename, &source));
            return Err(RChefError::Parse);
        }
    };

    let mut errored = false;
    for recipe in &recipes {
        errored |= ensure_consistent_ordinals(recipe, filename, &source).is_err();
    }
    if errored {
        return Err(RChefError::Parse);
    }

    dbg!(recipes);
    //direct_interpreter::run(recipes, spaced)
    Ok(())
}

fn handle_parse_error(error: Simple<Token>, filename: &str, source: &str) {
    let builder = Report::build(ReportKind::Error, filename.to_owned(), error.span().start);

    let report = match error.reason() {
        SimpleReason::Unexpected => {
            if error.found() == Some(&Token::Error) {
                builder.with_message("illegal token").with_label(
                    Label::new((filename.to_owned(), error.span()))
                        .with_message("here")
                        .with_color(Color::Red),
                )
            } else {
                builder.with_message("unexpected token").with_label(
                    Label::new((filename.to_owned(), error.span()))
                        .with_message(format!(
                            "expected {}, found {}",
                            error
                                .expected()
                                .copied()
                                .map(pretty_token_opt)
                                .map(|s| s.fg(Color::Cyan))
                                .join(", "),
                            pretty_token_opt(error.found().copied()).fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                )
            }
        }
        SimpleReason::Custom(msg) => builder
            .with_message(msg)
            .with_label(Label::new((filename.to_owned(), error.span())).with_message("here")),
        SimpleReason::Unclosed {
            span: verb1_span, ..
        } => {
            if error.found().is_some() {
                // wrong verbs
                builder
                    .with_message("incompatible loop verbs")
                    .with_label(
                        Label::new((filename.to_owned(), verb1_span.clone()))
                            .with_message("beginning verb here")
                            .with_color(Color::Green),
                    )
                    .with_label(
                        Label::new((filename.to_owned(), error.span()))
                            .with_message("incompatible verb here")
                            .with_color(Color::Red),
                    )
                    .with_note(format!(
                        "loop verbs must make sense together - e.g. '{} ... until {}ed.'",
                        "Climb".fg(Color::Green),
                        "climb".fg(Color::Green)
                    ))
            } else {
                // unclosed loop
                builder.with_message("unclosed loop").with_label(
                    Label::new((filename.to_owned(), error.span()))
                        .with_message("this loop is opened but never closed")
                        .with_color(Color::Red),
                )
            }
        }
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

fn ensure_consistent_ordinals(recipe: &parser::Recipe, filename: &str, source: &str) -> Result<()> {
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

    if !(implicit_bowls.is_empty() || explicit_bowls.is_empty()) {
        Report::build(
            ReportKind::Error,
            filename.to_owned(),
            implicit_bowls[0].start,
        )
        .with_message("recipes with more than two bowls cannot refer to bowls implicitly")
        .with_label(
            Label::new((filename.to_owned(), implicit_bowls[0].clone()))
                .with_color(Color::Red)
                .with_message("implicit reference used here"),
        )
        .with_label(
            Label::new((filename.to_owned(), explicit_bowls[0].clone()))
                .with_color(Color::Red)
                .with_message("explicit reference used here"),
        )
        .with_note(format!("in recipe '{}'", &recipe.title))
        .finish()
        .eprint(ariadne::sources([(filename.to_owned(), source)]))
        .unwrap();
    }

    if !(implicit_dishes.is_empty() || explicit_dishes.is_empty()) {
        Report::build(
            ReportKind::Error,
            filename.to_owned(),
            implicit_dishes[0].start,
        )
        .with_message("recipes with more than two dishes cannot refer to dishes implicitly")
        .with_label(
            Label::new((filename.to_owned(), implicit_dishes[0].clone()))
                .with_color(Color::Red)
                .with_message("implicit reference used here"),
        )
        .with_label(
            Label::new((filename.to_owned(), explicit_dishes[0].clone()))
                .with_color(Color::Red)
                .with_message("explicit reference used here"),
        )
        .with_note(format!("in recipe '{}'", &recipe.title))
        .finish()
        .eprint(ariadne::sources([(filename.to_owned(), source)]))
        .unwrap();
    }

    todo!()
}
