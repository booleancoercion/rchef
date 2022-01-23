use crate::{lexer::Token, Span};

use chumsky::Error;

use std::collections::HashSet;
use std::num::ParseIntError;

#[derive(Clone, Debug)]
pub enum ParseError {
    Unexpected {
        span: Span,
        expected: HashSet<Option<Token>>,
        found: Option<Token>,
    },
    InvalidIntLit(Span, ParseIntError),
    IncorrectVerbination {
        loop_verb: Span,
        invalid_verb: Span,
    },
    UnclosedLoop(Span),
    BadMeasureType(Span),
}

impl ParseError {
    pub fn offset(&self) -> usize {
        use ParseError::*;
        match self {
            Unexpected { span, .. }
            | InvalidIntLit(span, _)
            | IncorrectVerbination {
                loop_verb: span, ..
            }
            | UnclosedLoop(span)
            | BadMeasureType(span) => span.start,
        }
    }
}

impl Error<Token> for ParseError {
    type Span = Span;

    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token>,
    ) -> Self {
        Self::Unexpected {
            span,
            expected: expected.into_iter().collect(),
            found,
        }
    }

    fn unclosed_delimiter(
        _: Self::Span,
        _: Token,
        _: Self::Span,
        _: Token,
        _: Option<Token>,
    ) -> Self {
        unimplemented!()
    }

    fn with_label(self, _: ()) -> Self {
        self
    }

    fn merge(self, other: Self) -> Self {
        if let (
            ParseError::Unexpected {
                span: span1,
                expected: mut expected1,
                found: found1,
            },
            ParseError::Unexpected {
                span: span2,
                expected: expected2,
                found: found2,
            },
        ) = (self, other)
        {
            assert_eq!(span1, span2);
            assert_eq!(found1, found2);
            expected1.extend(expected2);

            ParseError::Unexpected {
                span: span1,
                expected: expected1,
                found: found1,
            }
        } else {
            panic!("cannot merge two ParseErrors that aren't of kind 'Unexpected'")
        }
    }
}
