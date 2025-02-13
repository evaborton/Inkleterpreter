module Parser

open AST
open Combinator
open System

let DEBUG = false



 // not currently in use *           <expr> ::= <pattern><backgroundColor><patternColor>

(*
 *         <pattern> ::= pattern: <patternType> | pattern: <patternType> (width) | pattern: DIY (<length>, <width>)
 *     <patternType> ::= DIY | Diamonds | Snowflakes
 *               <n> ::= (any positive integer)
 *           <color> ::= rgb <n> <n> <n> | red | green | blue | purple | black | grey | white
 * <backgroundColor> ::= background color: <color>
 *    <patternColor> ::= pattern color: <color>
 *           <width> ::= <n>
 *          <length> ::= <n>
 *)


//let expr, exprImpl = recparser()

let pad p = pbetween pws0 pws0 p

let num = pmany1 pdigit |>> stringify |>> int

let width = pright (pad (pstr "width:")) num
let length = pright (pad (pstr "length:")) num

let rgb = pright (pstr "rgb") (pseq (pseq (pad num) (pad num) (fun (a, b) -> (a, b))) (pad num) (fun ((a, b), c) -> RGB(a, b, c)))
let color =
    pad (
        rgb <|>
        (pstr "red" |>> (fun _ -> Red)) <|>
        (pstr "orange" |>> (fun _ -> Orange)) <|>
        (pstr "yellow" |>> (fun _ -> Yellow)) <|>
        (pstr "green" |>> (fun _ -> Green)) <|>
        (pstr "blue" |>> (fun _ -> Blue)) <|>
        (pstr "purple" |>> (fun _ -> Purple)) <|>
        (pstr "pink" |>> (fun _ -> Pink)) <|>
        (pstr "brown" |>> (fun _ -> Brown)) <|>
        (pstr "black" |>> (fun _ -> Black)) <|>
        (pstr "grey" |>> (fun _ -> Grey)) <|>
        (pstr "gray" |>> (fun _ -> Grey)) <|>
        (pstr "white" |>> (fun _ -> White))
    )

let backgroundColor = pright (pad (pstr "background color:")) color 
let patternColor = pright (pad (pstr "pattern color:")) color

// can be parsed in either order but always outputs background * pattern
let colors = 
    pseq (pad backgroundColor) (pad patternColor) (fun (a, b) -> (a, b)) <|>
    pseq (pad patternColor) (pad backgroundColor) (fun (a, b) -> (b, a))

let patternType = 
    pad (
        // (pstr "diy" |>> (fun _ -> DIY (0, 0))) <|>
        (pseq (pright (pstr "diy") (pright (pad (pstr ",")) width)) (pright (pad (pstr ",")) length) (fun (w, l) -> DIY(w, l))) <|>
        ((pright (pstr "diy") (pright (pad (pstr ",")) width)) |>> (fun w -> DIY(w, 0))) <|>
        ((pright (pstr "diamonds") (pright (pad (pstr ",")) width)) |>> (fun w -> Diamonds(w))) <|>
        ((pright (pstr "snowflakes") (pright (pad (pstr ",")) width)) |>> (fun (w: int) -> Snowflakes(w))) <|>
        ((pright (pstr "braids") (pright (pad (pstr ",")) width)) |>> (fun (w: int) -> Braids(w))) <|>
        ((pright (pstr "diamonds variant") (pright (pad (pstr ",")) width)) |>> (fun w -> DiamondsVariant(w))) <|>
        ((pright (pstr "snowflakes variant") (pright (pad (pstr ",")) width)) |>> (fun (w: int) -> SnowflakesVariant(w))) <|>
        ((pright (pstr "braids variant") (pright (pad (pstr ",")) width)) |>> (fun (w: int) -> BraidsVariant(w))) //<|>
    )

let patternDef = pright (pad (pstr "pattern:")) (patternType)

// let pattern: Parser<Pattern> = 
//     pad (
//         // pseq (pseq patternDef (pright (pad (pstr ",")) (width)) (fun (a, b) -> (a, b))) (pright (pad (pstr ",")) (length)) (fun ((a, b), c) -> a b c) // <|>

//         patternDef (pright (pad (pstr ",")) (width)) 0
//     )

let weaving: Parser<Weaving> = pseq patternDef colors (fun (pt, (a, b)) -> (pt, a, b))

let grammar = pleft weaving peof

let rec parse (s: string)  =     
    let prepared = if DEBUG then debug s else prepare s
    match grammar (prepared) with
    |Success(wv, _) -> Some (wv)
    |Failure(_,_) -> None