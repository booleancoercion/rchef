use std::num::NonZeroU32;

// "Coin" because it's a different kind of token :)
#[derive(Clone, Debug)]
pub struct Coin<'a> {
    pub kind: CoinKind<'a>,
    pub line_start: u32,
    pub line_end: u32,
}

#[derive(Clone, Debug)]
pub enum CoinKind<'a> {
    // Item keywords
    Ingredients,
    Method,
    Serves,

    // Method starting keywords
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
    SetAside,
    ServeWith,
    Refrigerate,

    // Method middle keywords
    From,
    Refrigerator,
    Into,
    MixingBowl,
    To,
    The,
    For,
    Minutes,
    Well,
    BakingDish,
    Until,
    ContentsOf,
    DryIngredients,

    FullStop,

    // Types of identifiers
    Identifier(Vec<&'a str>),
    Ordinal(NonZeroU32),
}
