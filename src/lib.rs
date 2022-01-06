//mod direct_interpreter;
mod lexer;
mod parser;

pub type Span = std::ops::Range<usize>;

use thiserror::Error;

use std::fs;
use std::io;
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
    let source = fs::read_to_string(filename)?;
    let tokens = lexer::process(&source);
    let recipes = parser::process(&source, tokens);
    dbg!(recipes);
    //direct_interpreter::run(recipes, spaced)
    Ok(())
}

/*
#[cfg(test)]
mod test {
    use super::direct_interpreter::{Interpreter, Value};
    use super::lexer;
    use super::parser::{self, IgdtBowl, Ingredient, Measure, Recipe, Stmt, StmtKind};
    use super::Result;

    use num_bigint::BigInt;

    use std::num::NonZeroU32;

    fn do_parse(filename: &str) -> Result<Vec<Recipe>> {
        let source = std::fs::read_to_string(filename)?;
        let tokens = lexer::process(&source);
        parser::process(tokens)
    }

    fn ingredient<S: ToString>(
        name: S,
        measure: Measure,
        initial_value: Option<BigInt>,
    ) -> Ingredient {
        Ingredient {
            name: name.to_string(),
            measure,
            initial_value,
        }
    }

    fn stmt(kind: StmtKind, line: u32) -> Stmt {
        Stmt { kind, line }
    }

    fn value(num: BigInt, measure: Measure) -> Value {
        Value { num, measure }
    }

    #[test]
    fn hello_world_souffle() -> Result<()> {
        use Measure::*;
        use StmtKind::*;
        let recipes = do_parse("programs/hello_world_souffle.chef")?;

        let expected_recipes = vec![Recipe {
            title: "Hello World Souffle".into(),
            ingredients: Some(vec![
                ingredient("haricot beans", Dry, Some(72.into())),
                ingredient("eggs", Ambiguous, Some(101.into())),
                ingredient("lard", Dry, Some(108.into())),
                ingredient("oil", Ambiguous, Some(111.into())),
                ingredient("zucchinis", Ambiguous, Some(32.into())),
                ingredient("water", Liquid, Some(119.into())),
                ingredient("red salmon", Dry, Some(114.into())),
                ingredient("dijon mustard", Dry, Some(100.into())),
                ingredient("potatoes", Ambiguous, Some(33.into())),
            ]),
            method: vec![
                stmt(Put(IgdtBowl("potatoes".into(), None)), 16),
                stmt(Put(IgdtBowl("dijon mustard".into(), None)), 17),
                stmt(Put(IgdtBowl("lard".into(), None)), 18),
                stmt(Put(IgdtBowl("red salmon".into(), None)), 19),
                stmt(Put(IgdtBowl("oil".into(), None)), 20),
                stmt(Put(IgdtBowl("water".into(), None)), 21),
                stmt(Put(IgdtBowl("zucchinis".into(), None)), 22),
                stmt(Put(IgdtBowl("oil".into(), None)), 23),
                stmt(Put(IgdtBowl("lard".into(), None)), 24),
                stmt(Put(IgdtBowl("lard".into(), None)), 25),
                stmt(Put(IgdtBowl("eggs".into(), None)), 26),
                stmt(Put(IgdtBowl("haricot beans".into(), None)), 27),
                stmt(LiquefyConts(None), 28),
                stmt(Pour(None, None), 29),
            ],
            serves: Some(NonZeroU32::new(1).unwrap()),
        }];

        assert_eq!(recipes, expected_recipes);

        let interpreter = Interpreter::new(recipes, false)?;
        let state = interpreter.run_and_return_state()?;

        let dish = state.dishes.get(&NonZeroU32::new(1).unwrap()).unwrap();
        assert_eq!(
            dish.values,
            "Hello world!"
                .chars()
                .rev()
                .map(|c| value((c as u32).into(), Measure::Liquid))
                .collect::<Vec<_>>(),
        );
        Ok(())
    }

    #[test]
    fn hello_world_cake() -> Result<()> {
        use Measure::*;
        use StmtKind::*;
        let recipes = do_parse("programs/hello_world_cake.chef")?;

        let expected_recipes = vec![
            Recipe {
                title: "Hello World Cake with Chocolate sauce".into(),
                ingredients: Some(vec![
                    ingredient("chocolate chips", Dry, Some(33.into())),
                    ingredient("butter", Dry, Some(100.into())),
                    ingredient("double cream", Liquid, Some(54.into())),
                    ingredient("baking powder", Dry, Some(2.into())),
                    ingredient("sugar", Dry, Some(114.into())),
                    ingredient("beaten eggs", Liquid, Some(111.into())),
                    ingredient("flour", Dry, Some(119.into())),
                    ingredient("cocoa powder", Dry, Some(32.into())),
                    ingredient("cake mixture", Dry, Some(0.into())),
                ]),
                method: vec![
                    stmt(Put(IgdtBowl("chocolate chips".into(), None)), 26),
                    stmt(Put(IgdtBowl("butter".into(), None)), 27),
                    stmt(Put(IgdtBowl("sugar".into(), None)), 28),
                    stmt(Put(IgdtBowl("beaten eggs".into(), None)), 29),
                    stmt(Put(IgdtBowl("flour".into(), None)), 30),
                    stmt(Put(IgdtBowl("baking powder".into(), None)), 31),
                    stmt(Put(IgdtBowl("cocoa powder".into(), None)), 32),
                    stmt(Stir(None, 1), 33),
                    stmt(Combine(IgdtBowl("double cream".into(), None)), 34),
                    stmt(Stir(None, 4), 35),
                    stmt(LiquefyConts(None), 36),
                    stmt(Pour(None, None), 37),
                    stmt(
                        Loop {
                            igdt1: "cake mixture".into(),
                            igdt2: None,
                            stmts: vec![],
                        },
                        38,
                    ),
                    stmt(ServeWith("chocolate sauce".into()), 40),
                ],
                serves: None,
            },
            Recipe {
                title: "Chocolate sauce".into(),
                ingredients: Some(vec![
                    ingredient("sugar", Dry, Some(111.into())),
                    ingredient("hot water", Liquid, Some(108.into())),
                    ingredient("heated double cream", Liquid, Some(108.into())),
                    ingredient("dark chocolate", Dry, Some(101.into())),
                    ingredient("milk chocolate", Dry, Some(72.into())),
                ]),
                method: vec![
                    stmt(Clean(None), 52),
                    stmt(Put(IgdtBowl("sugar".into(), None)), 53),
                    stmt(Put(IgdtBowl("hot water".into(), None)), 54),
                    stmt(Put(IgdtBowl("heated double cream".into(), None)), 55),
                    stmt(
                        Loop {
                            igdt1: "sugar".into(),
                            igdt2: Some("sugar".into()),
                            stmts: vec![],
                        },
                        56,
                    ),
                    stmt(Liquefy("dark chocolate".into()), 58),
                    stmt(Put(IgdtBowl("dark chocolate".into(), None)), 59),
                    stmt(Liquefy("milk chocolate".into()), 60),
                    stmt(Put(IgdtBowl("milk chocolate".into(), None)), 61),
                    stmt(LiquefyConts(None), 62),
                    stmt(Pour(None, None), 63),
                    stmt(Refrigerate(Some(NonZeroU32::new(1).unwrap())), 64),
                ],
                serves: None,
            },
        ];

        assert_eq!(recipes, expected_recipes);

        let interpreter = Interpreter::new(recipes, false)?;
        let state = interpreter.run_and_return_state()?;

        let dish = state.dishes.get(&NonZeroU32::new(1).unwrap()).unwrap();
        assert_eq!(
            dish.values,
            " world!" // the "Hello" part comes from the sous-chef
                .chars()
                .rev()
                .map(|c| value((c as u32).into(), Measure::Liquid))
                .collect::<Vec<_>>(),
        );
        Ok(())
    }

    /*
    #[test]
    fn fib_parses() -> Result<()> {
        do_parse("programs/fib.chef")
    }

    */
}
*/
