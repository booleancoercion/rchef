use crate::parser::{IgdtBowl, Measure, Recipe, Stmt, StmtKind};
use crate::{RChefError, Result};

use if_chain::if_chain;

use std::collections::HashMap;
use std::io;
use std::num::NonZeroU32;

pub fn run(recipes: Vec<Recipe>) -> Result<()> {
    Interpreter::new(recipes)?.run()
}

#[derive(Debug)]
struct Interpreter {
    recipes: HashMap<String, Recipe>,
    main: String,
}

impl Interpreter {
    fn new(recipes: Vec<Recipe>) -> Result<Self> {
        if recipes.is_empty() {
            crate::report_error(0, "syntax ", "programs must contain at least one recipe");
            return Err(RChefError::Runtime);
        }

        let main = recipes[0].title.clone();
        let recipes = recipes
            .into_iter()
            .map(|rec| (rec.title.clone(), rec))
            .collect();

        Ok(Self { recipes, main })
    }

    fn run(self) -> Result<()> {
        let main = self.recipes.get(&self.main).unwrap();

        RecipeRunner::new(&self, main).run()?;
        Ok(())
    }
}

#[derive(Debug)]
struct RecipeRunner<'a> {
    interpreter: &'a Interpreter,
    recipe: &'a Recipe,
    ingredients: Option<HashMap<String, Option<Value>>>,
    bowls: HashMap<NonZeroU32, ValueStack>,
    dishes: HashMap<NonZeroU32, ValueStack>,
    only_first_bowl: bool,
    only_first_dish: bool,
    line: u32,
}

#[derive(Copy, Clone, Debug)]
struct Value {
    num: i64,
    measure: Measure,
}

#[derive(Clone, Debug)]
struct ValueStack {
    number: NonZeroU32,
    values: Vec<Value>,
}

impl ValueStack {
    fn new(number: NonZeroU32) -> Self {
        Self {
            number,
            values: vec![],
        }
    }

    fn push(&mut self, val: Value) {
        self.values.push(val);
    }

    fn pop(&mut self) -> Option<Value> {
        self.values.pop()
    }

    fn peek(&self) -> Option<Value> {
        self.values.last().copied()
    }
}

impl<'a> RecipeRunner<'a> {
    fn new(interpreter: &'a Interpreter, recipe: &'a Recipe) -> Self {
        let ingredients = recipe.ingredients.as_ref().map(|vec| {
            vec.iter()
                .map(|ing| {
                    (
                        ing.name.clone(),
                        ing.initial_value.map(|num| Value {
                            num,
                            measure: ing.measure,
                        }),
                    )
                })
                .collect()
        });

        Self {
            interpreter,
            recipe,
            ingredients,
            bowls: HashMap::new(),
            dishes: HashMap::new(),
            only_first_bowl: true,
            only_first_dish: true,
            line: 0,
        }
    }

    fn with_dishes(
        interpreter: &'a Interpreter,
        recipe: &'a Recipe,
        dishes: HashMap<NonZeroU32, ValueStack>,
    ) -> Self {
        Self {
            dishes,
            ..Self::new(interpreter, recipe)
        }
    }

    fn run(mut self) -> Result<Option<ValueStack>> {
        for stmt in &self.recipe.method {
            self.execute_stmt(stmt)?;
        }

        Ok(self.dishes.remove(&NonZeroU32::new(1).unwrap()))
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        use StmtKind::*;

        match &stmt.kind {
            Take(ing) => {
                let ing = self.get_ingredient_mut(ing)?;
                let mut buffer = String::new();
                io::stdin().read_line(&mut buffer)?;

                if let Ok(num) = buffer.trim().parse() {
                    *ing = Some(Value {
                        num,
                        measure: Measure::Dry,
                    });
                } else {
                    return self.error("could not read number from stdin");
                };
            }
            Put(IgdtBowl(igdt, bowl)) => {
                let igdt = self.get_ingredient(igdt)?;
                let bowl = self.get_bowl_mut(bowl)?;

                bowl.push(igdt);
            }
            Fold(IgdtBowl(igdt, bowl)) => {
                let val = if let Some(val) = self.get_bowl_mut(bowl)?.pop() {
                    val
                } else {
                    return self.error(format!(
                        "attempted to get value from bowl {}, but it's empty!",
                        bowl.map(NonZeroU32::get).unwrap_or(1)
                    ));
                };

                *self.get_ingredient_mut(igdt)? = Some(val);
            }
            Add(x) => self.arithmetic_helper(x, |a, b| a + b)?,
            Remove(x) => self.arithmetic_helper(x, |a, b| b - a)?,
            Combine(x) => self.arithmetic_helper(x, |a, b| a * b)?,
            Divide(x) => self.arithmetic_helper(x, |a, b| b / a)?,
            AddDry(bowl) => {
                let num = if let Some(ingredients) = &self.ingredients {
                    ingredients
                        .values()
                        .flatten()
                        .filter_map(|val| {
                            if val.measure == Measure::Dry {
                                Some(val.num)
                            } else {
                                None
                            }
                        })
                        .sum()
                } else {
                    return self
                        .error("attempted to access ingredients, but recipe has no ingredients!");
                };

                self.get_bowl_mut(bowl)?.push(Value {
                    num,
                    measure: Measure::Dry,
                });
            }
            Liquefy(name) => {
                let igdt = self.get_ingredient_mut(name)?;

                if let Some(igdt) = igdt.as_mut() {
                    igdt.measure = Measure::Liquid;
                } else {
                    return self
                        .error(format!("attempted to use the value of ingredient '{}', but ingredient is uninitialized!", name));
                }
            }
            LiquefyConts(bowl) => {
                let bowl = self.get_bowl_mut(bowl)?;

                bowl.values
                    .iter_mut()
                    .for_each(|val| val.measure = Measure::Liquid);
            }
            Stir(bowl, n) => {
                if *n == 0 {
                    return Ok(());
                }
                let bowl = self.get_bowl_mut(bowl)?;
                let len = bowl.values.len();
                let n = (*n as usize).min(len - 1);

                bowl.values[len - 1 - n..].rotate_right(1);
            }
            StirInto(IgdtBowl(igdt, bowl)) => todo!(),
            Mix(bowl) => todo!(),
            Clean(bowl) => todo!(),
            Pour(bowl, dish) => todo!(),
            Loop {
                igdt1,
                igdt2,
                stmts,
            } => todo!(),
            SetAside => todo!(),
            ServeWith(recipe) => todo!(),
            Refrigerate(n) => todo!(),
        }

        Ok(())
    }

    fn arithmetic_helper(
        &mut self,
        igdtbowl: &IgdtBowl,
        f: impl FnOnce(i64, i64) -> i64,
    ) -> Result<()> {
        let IgdtBowl(igdt, bowl) = igdtbowl;

        let igdt = self.get_ingredient(igdt)?;
        let top = self.peek_bowl(bowl)?;
        let num = f(igdt.num, top.num);

        let bowl = self.get_bowl_mut(bowl)?;
        bowl.push(Value {
            num,
            measure: Measure::Dry,
        });

        Ok(())
    }

    fn peek_bowl(&mut self, key: &Option<NonZeroU32>) -> Result<Value> {
        let key = Self::bowldish_key_helper(self.line, "bowl", key, &mut self.only_first_bowl)?;

        if_chain! {
            if let Some(stack) = self.bowls.get(&key);
            if let Some(val) = stack.peek();
            then {
                Ok(val)
            } else {
                self.error(format!("attempted to access value in bowl {} but it was empty!", key))
            }
        }
    }

    fn get_bowl_mut(&mut self, key: &Option<NonZeroU32>) -> Result<&mut ValueStack> {
        Self::get_bowl_or_dish_mut(
            self.line,
            &mut self.bowls,
            "bowl",
            key,
            &mut self.only_first_bowl,
        )
    }

    fn get_dish_mut(&mut self, key: &Option<NonZeroU32>) -> Result<&mut ValueStack> {
        Self::get_bowl_or_dish_mut(
            self.line,
            &mut self.dishes,
            "dish",
            key,
            &mut self.only_first_dish,
        )
    }

    fn get_bowl_or_dish_mut<'h>(
        line: u32,
        things: &'h mut HashMap<NonZeroU32, ValueStack>,
        thing_name: &'static str,
        key: &Option<NonZeroU32>,
        only_first: &mut bool,
    ) -> Result<&'h mut ValueStack> {
        let key = Self::bowldish_key_helper(line, thing_name, key, only_first)?;

        Ok(things.entry(key).or_insert_with(|| ValueStack::new(key)))
    }

    fn bowldish_key_helper(
        line: u32,
        thing_name: &'static str,
        key: &Option<NonZeroU32>,
        only_first: &mut bool,
    ) -> Result<NonZeroU32> {
        if let Some(key) = key {
            if key.get() != 1 {
                *only_first = true;
            }
            Ok(*key)
        } else if *only_first {
            Ok(NonZeroU32::new(1).unwrap())
        } else {
            Self::error_with_line(
                line,
                format!(
                    "attempted to access {0} without ordinal, yet more than one {0} is in use!",
                    thing_name
                ),
            );
            Err(RChefError::Runtime)
        }
    }

    fn get_ingredient(&self, ing: &str) -> Result<Value> {
        let ingredients = if let Some(x) = self.ingredients.as_ref() {
            x
        } else {
            return self.error("attempted to access ingredient, but recipe has no ingredients!");
        };

        let val = if let Some(val) = ingredients.get(ing) {
            val
        } else {
            return self.error(format!(
                "attempted to access ingredient '{}', but recipe has no such ingredient!",
                ing
            ));
        };

        if let Some(val) = val.as_ref() {
            Ok(*val)
        } else {
            self.error(format!(
                "attempted to use the value of ingredient '{}', but ingredient is uninitialized!",
                ing
            ))
        }
    }

    fn get_ingredient_mut(&mut self, ing: &str) -> Result<&mut Option<Value>> {
        let line = self.line;
        // ^ blame the borrow checker, not me
        // i would've done this much more simply but no, the borrow checker
        // can't figure out that i'm not using the mutable reference to self
        // when i call self.error()

        let ingredients = if let Some(x) = self.ingredients.as_mut() {
            x
        } else {
            Self::error_with_line(
                line,
                "attempted to access ingredient, but recipe has no ingredients!",
            );
            return Err(RChefError::Runtime);
        };

        if let Some(val) = ingredients.get_mut(ing) {
            Ok(val)
        } else {
            Self::error_with_line(
                line,
                format!(
                    "attempted to access ingredient '{}', but recipe has no such ingredient!",
                    ing
                ),
            );
            Err(RChefError::Runtime)
        }
    }

    fn report_error(&self, msg: impl std::fmt::Display) {
        Self::error_with_line(self.line, msg);
    }

    fn error<T, D: std::fmt::Display>(&self, msg: D) -> Result<T> {
        self.report_error(msg);
        Err(RChefError::Runtime)
    }

    fn error_with_line<D: std::fmt::Display>(line: u32, msg: D) {
        crate::report_error(line, "runtime ", msg);
    }
}
