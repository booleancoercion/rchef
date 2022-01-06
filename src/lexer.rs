use super::Span;

use itertools::Itertools;
use logos::Logos;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Logos)]
pub enum Token {
    #[regex("[a-zA-Z]+")]
    Ident,

    #[regex(r"((\d*[234567890])?(1st|2nd|3rd)|\d*[0456789]th|\d*1[123]th)")]
    Ord,

    #[regex(r"[-+]?\d+")]
    Num,

    #[regex("k?g|pinch(es)?")]
    DryMeasure,

    #[regex("m?l|dash(es)?")]
    LiquidMeasure,

    #[regex("cups?|teaspoons?|tablespoons?")]
    AmbiguousMeasure,

    #[regex("heaped|level")]
    MeasureType,

    #[token("Ingredients")] Ingredients,
    #[token("Method")] Method,
    #[token("Take")] Take,
    #[token("Put")] Put,
    #[token("into")] Into,
    #[token("Fold")] Fold,
    #[token("Add")] Add,
    #[token("to")] To,
    #[token("Remove")] Remove,
    #[token("Combine")] Combine,
    #[token("Divide")] Divide,
    #[token("Liquefy")] Liquefy,
    #[token("the")] The,
    #[token("Stir")] Stir,
    #[token("for")] For,
    #[regex("minutes?")] Minutes,
    #[token("Mix")] Mix,
    #[token("well")] Well,
    #[token("Clean")] Clean,
    #[token("Pour")] Pour,
    #[token("until")] Until,
    #[token("Refrigerate")] Refrigerate,
    #[regex("hours?")] Hours,
    #[token("Serves")] Serves,
    #[token("from")] From,
    #[token("refrigerator")] Refrigerator,
    #[token("mixing bowl")] MixingBowl,
    #[token("dry ingredients")] DryIngredients,
    #[token("contents of")] ContentsOf,
    #[token("baking dish")] BakingDish,
    #[token("Set aside")] SetAside,
    #[token("Serve with")] ServeWith,

    #[token("\n")] NewLine,
    #[token(".")] FullStop,

    #[regex("[ \r\t]+", logos::skip)]
    #[error]
    Error,
}

pub fn process(source: &str) -> Vec<(Token, Span)> {
    Token::lexer(source)
        .spanned()
        .coalesce(|tok1, tok2| {
            if tok1.0 == Token::Ident && tok2.0 == Token::Ident {
                Ok((Token::Ident, tok1.1.start..tok2.1.end))
            } else {
                Err((tok1, tok2))
            }
        })
        .collect()
}

#[cfg(test)]
mod test {
    use itertools::Itertools;

    use super::process;
    use super::Token;

    #[test]
    fn coalesced_idents() {
        let source = "This is some refrigerator source. Fold mixing bowl into.";
        let tokens = process(source)
            .iter()
            .map(|(token, _)| *token)
            .collect::<Vec<_>>();

        use Token::*;

        assert_eq!(
            tokens,
            vec![
                Ident,
                Refrigerator,
                Ident,
                FullStop,
                Fold,
                MixingBowl,
                Token::Into,
                FullStop
            ]
        );
    }

    #[test]
    fn all_tokens() {
        let source = std::fs::read_to_string("programs/tokens.chef").unwrap();
        let tokens = process(&source);

        assert_eq!(
            &tokens
                .into_iter()
                .map(|(token, span)| format!(
                    "{:?}[{}..{}] \"{}\"",
                    token,
                    span.start,
                    span.end,
                    &source[span.clone()]
                ))
                .join("\n"),
            r#"Ident[0..6] "Tokens"
FullStop[6..7] "."
NewLine[8..9] "
"
NewLine[10..11] "
"
Ingredients[11..22] "Ingredients"
FullStop[22..23] "."
NewLine[24..25] "
"
Num[25..26] "1"
DryMeasure[27..28] "g"
Ident[29..34] "sugar"
NewLine[35..36] "
"
Num[36..37] "1"
DryMeasure[38..40] "kg"
Ident[41..46] "sugar"
NewLine[47..48] "
"
Num[48..49] "1"
DryMeasure[50..55] "pinch"
Ident[56..61] "sugar"
NewLine[62..63] "
"
Num[63..64] "2"
DryMeasure[65..72] "pinches"
Ident[73..78] "sugar"
NewLine[79..80] "
"
Num[80..81] "1"
LiquidMeasure[82..84] "ml"
Ident[85..90] "water"
NewLine[91..92] "
"
Num[92..93] "1"
LiquidMeasure[94..95] "l"
Ident[96..101] "water"
NewLine[102..103] "
"
Num[103..104] "1"
LiquidMeasure[105..109] "dash"
Ident[110..115] "water"
NewLine[116..117] "
"
Num[117..118] "2"
LiquidMeasure[119..125] "dashes"
Ident[126..131] "water"
NewLine[132..133] "
"
Num[133..134] "1"
AmbiguousMeasure[135..138] "cup"
Ident[139..143] "soda"
NewLine[144..145] "
"
Num[145..146] "2"
AmbiguousMeasure[147..151] "cups"
Ident[152..156] "soda"
NewLine[157..158] "
"
Num[158..159] "1"
AmbiguousMeasure[160..168] "teaspoon"
Ident[169..173] "soda"
NewLine[174..175] "
"
Num[175..176] "2"
AmbiguousMeasure[177..186] "teaspoons"
Ident[187..191] "soda"
NewLine[192..193] "
"
Num[193..194] "1"
AmbiguousMeasure[195..205] "tablespoon"
Ident[206..210] "soda"
NewLine[211..212] "
"
Num[212..213] "2"
AmbiguousMeasure[214..225] "tablespoons"
Ident[226..230] "soda"
NewLine[231..232] "
"
Num[232..233] "1"
Ident[234..240] "heaped"
AmbiguousMeasure[241..244] "cup"
Ident[245..250] "flour"
NewLine[251..252] "
"
Num[252..253] "1"
Ident[254..259] "level"
AmbiguousMeasure[260..263] "cup"
Ident[264..269] "flour"
NewLine[270..271] "
"
NewLine[272..273] "
"
Method[273..279] "Method"
FullStop[279..280] "."
NewLine[281..282] "
"
Take[282..286] "Take"
Ident[287..292] "sugar"
From[293..297] "from"
Refrigerator[298..310] "refrigerator"
FullStop[310..311] "."
NewLine[312..313] "
"
Put[313..316] "Put"
Ident[317..322] "sugar"
Into[323..327] "into"
The[328..331] "the"
MixingBowl[332..343] "mixing bowl"
FullStop[343..344] "."
NewLine[345..346] "
"
Put[346..349] "Put"
Ident[350..355] "sugar"
Into[356..360] "into"
The[361..364] "the"
Ord[364..368] " 2nd"
MixingBowl[369..380] "mixing bowl"
FullStop[380..381] "."
NewLine[382..383] "
"
Fold[383..387] "Fold"
Ident[388..393] "sugar"
Into[394..398] "into"
The[399..402] "the"
MixingBowl[403..414] "mixing bowl"
FullStop[414..415] "."
NewLine[416..417] "
"
Fold[417..421] "Fold"
Ident[422..427] "sugar"
Into[428..432] "into"
The[433..436] "the"
Ord[436..440] " 3rd"
MixingBowl[441..452] "mixing bowl"
FullStop[452..453] "."
NewLine[454..455] "
"
Add[455..458] "Add"
Ident[459..464] "sugar"
FullStop[464..465] "."
NewLine[466..467] "
"
Add[467..470] "Add"
Ident[471..476] "sugar"
Into[477..481] "into"
The[482..485] "the"
MixingBowl[486..497] "mixing bowl"
FullStop[497..498] "."
NewLine[499..500] "
"
Add[500..503] "Add"
Ident[504..509] "sugar"
Into[510..514] "into"
The[515..518] "the"
Ord[518..522] " 1st"
MixingBowl[523..534] "mixing bowl"
FullStop[534..535] "."
NewLine[536..537] "
"
Remove[537..543] "Remove"
Ident[544..549] "sugar"
FullStop[549..550] "."
NewLine[551..552] "
"
Remove[552..558] "Remove"
Ident[559..564] "sugar"
From[565..569] "from"
The[570..573] "the"
MixingBowl[574..585] "mixing bowl"
FullStop[585..586] "."
NewLine[587..588] "
"
Remove[588..594] "Remove"
Ident[595..600] "sugar"
From[601..605] "from"
The[606..609] "the"
Ord[610..613] "4th"
MixingBowl[614..625] "mixing bowl"
FullStop[625..626] "."
NewLine[627..628] "
"
Combine[628..635] "Combine"
Ident[636..641] "sugar"
FullStop[641..642] "."
NewLine[643..644] "
"
Combine[644..651] "Combine"
Ident[652..657] "sugar"
Into[658..662] "into"
The[663..666] "the"
MixingBowl[667..678] "mixing bowl"
FullStop[678..679] "."
NewLine[680..681] "
"
Divide[681..687] "Divide"
Ident[688..693] "sugar"
FullStop[693..694] "."
NewLine[695..696] "
"
Divide[696..702] "Divide"
Ident[703..708] "sugar"
Into[709..713] "into"
The[714..717] "the"
MixingBowl[718..729] "mixing bowl"
FullStop[729..730] "."
NewLine[731..732] "
"
Add[732..735] "Add"
DryIngredients[736..751] "dry ingredients"
FullStop[751..752] "."
NewLine[753..754] "
"
Add[754..757] "Add"
DryIngredients[758..773] "dry ingredients"
To[774..776] "to"
The[777..780] "the"
Ord[781..785] "10th"
MixingBowl[786..797] "mixing bowl"
FullStop[797..798] "."
NewLine[799..800] "
"
Liquefy[800..807] "Liquefy"
Ident[808..813] "sugar"
FullStop[813..814] "."
NewLine[815..816] "
"
Liquefy[816..823] "Liquefy"
ContentsOf[824..835] "contents of"
The[836..839] "the"
MixingBowl[840..851] "mixing bowl"
FullStop[851..852] "."
NewLine[853..854] "
"
Stir[854..858] "Stir"
For[859..862] "for"
Num[863..865] "10"
Minutes[866..873] "minutes"
FullStop[873..874] "."
NewLine[875..876] "
"
Stir[876..880] "Stir"
For[881..884] "for"
Num[885..886] "1"
Minutes[887..893] "minute"
FullStop[893..894] "."
NewLine[895..896] "
"
Stir[896..900] "Stir"
The[901..904] "the"
MixingBowl[905..916] "mixing bowl"
For[917..920] "for"
Num[921..922] "2"
Minutes[923..930] "minutes"
FullStop[930..931] "."
NewLine[932..933] "
"
Stir[933..937] "Stir"
Ident[938..943] "sugar"
Into[944..948] "into"
The[949..952] "the"
MixingBowl[953..964] "mixing bowl"
FullStop[964..965] "."
NewLine[966..967] "
"
Mix[967..970] "Mix"
Well[971..975] "well"
FullStop[975..976] "."
NewLine[977..978] "
"
Mix[978..981] "Mix"
The[982..985] "the"
MixingBowl[986..997] "mixing bowl"
Well[998..1002] "well"
FullStop[1002..1003] "."
NewLine[1004..1005] "
"
Clean[1005..1010] "Clean"
The[1011..1014] "the"
MixingBowl[1015..1026] "mixing bowl"
FullStop[1026..1027] "."
NewLine[1028..1029] "
"
Pour[1029..1033] "Pour"
ContentsOf[1034..1045] "contents of"
The[1046..1049] "the"
MixingBowl[1050..1061] "mixing bowl"
Into[1062..1066] "into"
The[1067..1070] "the"
Ord[1070..1074] " 2nd"
BakingDish[1075..1086] "baking dish"
FullStop[1086..1087] "."
NewLine[1088..1089] "
"
Pour[1089..1093] "Pour"
ContentsOf[1094..1105] "contents of"
The[1106..1109] "the"
Ord[1109..1113] " 1st"
MixingBowl[1114..1125] "mixing bowl"
Into[1126..1130] "into"
The[1131..1134] "the"
BakingDish[1135..1146] "baking dish"
FullStop[1146..1147] "."
NewLine[1148..1149] "
"
Pour[1149..1153] "Pour"
ContentsOf[1154..1165] "contents of"
The[1166..1169] "the"
Ord[1170..1174] "10th"
MixingBowl[1175..1186] "mixing bowl"
Into[1187..1191] "into"
The[1192..1195] "the"
Ord[1196..1200] "10th"
BakingDish[1201..1212] "baking dish"
FullStop[1212..1213] "."
NewLine[1214..1215] "
"
Ident[1215..1219] "Loop"
The[1220..1223] "the"
Ident[1224..1229] "sugar"
FullStop[1229..1230] "."
NewLine[1231..1232] "
"
SetAside[1232..1241] "Set aside"
FullStop[1241..1242] "."
NewLine[1243..1244] "
"
Ident[1244..1256] "Keep looping"
The[1257..1260] "the"
Ident[1261..1266] "sugar"
Until[1267..1272] "until"
Ident[1273..1279] "looped"
FullStop[1279..1280] "."
NewLine[1281..1282] "
"
ServeWith[1282..1292] "Serve with"
Ident[1293..1299] "tokens"
FullStop[1299..1300] "."
NewLine[1301..1302] "
"
Refrigerate[1302..1313] "Refrigerate"
FullStop[1313..1314] "."
NewLine[1315..1316] "
"
Refrigerate[1316..1327] "Refrigerate"
For[1328..1331] "for"
Num[1332..1333] "2"
Hours[1334..1339] "hours"
FullStop[1339..1340] "."
NewLine[1341..1342] "
"
Refrigerate[1342..1353] "Refrigerate"
For[1354..1357] "for"
Num[1358..1359] "1"
Hours[1360..1364] "hour"
FullStop[1364..1365] "."
NewLine[1366..1367] "
"
NewLine[1368..1369] "
"
Serves[1369..1375] "Serves"
Num[1376..1378] "56"
FullStop[1378..1379] "."
NewLine[1380..1381] "
"
NewLine[1382..1383] "
"
Ident[1383..1389] "Tokens"
Num[1390..1391] "2"
FullStop[1391..1392] "."
NewLine[1393..1394] "
"
NewLine[1395..1396] "
"
Ingredients[1396..1407] "Ingredients"
FullStop[1407..1408] "."
NewLine[1409..1410] "
"
Num[1410..1411] "1"
Ident[1412..1415] "egg"
NewLine[1416..1417] "
"
NewLine[1418..1419] "
"
Method[1419..1425] "Method"
FullStop[1425..1426] "."
NewLine[1427..1428] "
"
Clean[1428..1433] "Clean"
The[1434..1437] "the"
MixingBowl[1438..1449] "mixing bowl"
FullStop[1449..1450] "."
NewLine[1451..1452] "
"
Put[1452..1455] "Put"
Ident[1456..1459] "egg"
Into[1460..1464] "into"
The[1465..1468] "the"
MixingBowl[1469..1480] "mixing bowl"
FullStop[1480..1481] "."
NewLine[1482..1483] "
"
Refrigerate[1483..1494] "Refrigerate"
For[1495..1498] "for"
Num[1499..1501] "10"
Hours[1502..1507] "hours"
FullStop[1507..1508] "."
NewLine[1509..1510] "
"
NewLine[1511..1512] "
"
Serves[1512..1518] "Serves"
Num[1519..1521] "10"
FullStop[1521..1522] ".""#
        );
    }
}
