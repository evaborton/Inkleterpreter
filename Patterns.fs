module Patterns

open AST

let rec patternCutterHelper (row: int list) (s: int) (e: int) = 
    match e - s with 
    | 0 -> []
    | _ -> row.[s] :: patternCutterHelper row (s + 1) e

(* Cuts a 13-wide pattern down to width w. w must be 5, 7, 9, 11, or 13. *)
let rec patternCutter(p: int list list) (w: int) = 
    match p with
    | [] -> []
    | _ ->
        let s = (13 - w) / 2
        let e = s + w
        patternCutterHelper p.Head s e :: patternCutter p.Tail w

let rec diy width len = 
    match len with 
    | 0 -> [] // default to width = 13, length = 24
    | _ -> (patternCutterHelper [0;0;0;0;0;0;0;0;0;0;0;0;0] 0 width) :: diy width (len - 1)


let diamonds =
    [[1;0;0;0;1;0;0;0;1;0;0;0;1];
     [0;0;0;1;0;0;0;0;0;1;0;0;0];
     [0;0;1;0;0;0;1;0;0;0;1;0;0];
     [0;1;0;0;0;1;1;1;0;0;0;1;0];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [0;0;0;1;0;1;1;1;0;1;0;0;0];
     [0;0;1;1;1;0;1;0;1;1;1;0;0];
     [0;1;1;1;1;1;0;1;1;1;1;1;0];
     [1;0;1;1;1;0;1;0;1;1;1;0;1];
     [1;1;0;1;0;1;0;1;0;1;0;1;1];
     [1;1;1;0;1;0;0;0;1;0;1;1;1];
     [1;1;0;1;0;1;0;1;0;1;0;1;1];
     [1;0;1;1;1;0;1;0;1;1;1;0;1];
     [0;1;1;1;1;1;0;1;1;1;1;1;0];
     [0;0;1;1;1;0;1;0;1;1;1;0;0];
     [0;0;0;1;0;1;1;1;0;1;0;0;0];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [0;1;0;0;0;1;1;1;0;0;0;1;0];
     [0;0;1;0;0;0;1;0;0;0;1;0;0];
     [0;0;0;1;0;0;0;0;0;1;0;0;0];
     [1;0;0;0;1;0;0;0;1;0;0;0;1];
     [1;1;0;0;0;1;0;1;0;0;0;1;1];
     [1;1;1;0;0;0;1;0;0;0;1;1;1];
     [1;1;0;0;0;0;0;0;0;0;0;1;1]]

let snowflakes  = 
    [[1;0;0;0;0;0;1;0;0;0;0;0;1];
     [1;1;0;0;0;1;0;1;0;0;0;1;1];
     [1;0;1;0;1;0;1;0;1;0;1;0;1];
     [0;0;1;1;0;1;1;1;0;1;1;0;0];
     [1;0;1;0;1;0;1;0;1;0;1;0;1];
     [1;1;0;0;0;1;0;1;0;0;0;1;1];
     [1;0;0;0;0;0;1;0;0;0;0;0;1];
     [0;0;0;1;0;0;0;0;0;1;0;0;0];
     [0;0;1;1;1;0;0;0;1;1;1;0;0];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [1;1;0;1;1;1;0;1;1;1;0;1;1];
     [1;1;1;1;1;0;1;0;1;1;1;1;1];
     [0;1;1;1;0;1;1;1;0;1;1;1;0];
     [1;1;1;1;1;0;1;0;1;1;1;1;1];
     [1;1;0;1;1;1;0;1;1;1;0;1;1];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [0;0;1;1;1;0;0;0;1;1;1;0;0];
     [0;0;0;1;0;0;0;0;0;1;0;0;0]]

let braids = 
    [[0;1;1;1;0;1;0;0;0;1;1;1;0];
     [1;1;1;1;1;0;0;0;1;1;1;1;1];
     [0;1;1;1;0;0;0;1;0;1;1;1;0];
     [0;0;1;0;0;0;1;0;0;0;1;0;0];
     [0;0;0;1;0;1;1;1;0;0;0;1;0];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [0;1;0;0;0;1;1;1;0;1;0;0;0];
     [0;0;1;0;0;0;1;0;0;0;1;0;0]]

let diamonds_variant = 
    [[1;0;1;0;0;0;1;0;0;0;1;0;1];
     [1;1;0;0;0;1;1;1;0;0;0;1;1];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [0;0;1;1;1;0;0;0;1;1;1;0;0];
     [0;1;1;1;1;1;0;1;1;1;1;1;0];
     [1;1;1;0;1;1;1;1;1;0;1;1;1];
     [1;1;0;0;0;1;1;1;0;0;0;1;1];
     [1;1;1;0;1;1;1;1;1;0;1;1;1];
     [0;1;1;1;1;1;0;1;1;1;1;1;0];
     [0;0;1;1;1;0;0;0;1;1;1;0;0];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [1;1;0;0;0;1;1;1;0;0;0;1;1];
     [1;0;1;0;0;0;1;0;0;0;1;0;1];
     [0;1;1;1;0;0;0;0;0;1;1;1;0];
     [0;0;1;0;1;0;0;0;1;0;1;0;0];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [1;0;0;0;1;0;1;0;1;0;0;0;1];
     [1;1;0;0;0;1;1;1;0;0;0;1;1];
     [1;0;0;0;1;0;1;0;1;0;0;0;1];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [0;0;1;0;1;0;0;0;1;0;1;0;0];
     [0;1;1;1;0;0;0;0;0;1;1;1;0]]

let snowflakes_variant = 
    [[1;0;1;0;0;0;1;0;0;0;1;0;1];
     [0;1;0;1;0;0;0;0;0;1;0;1;0];
     [0;0;1;0;1;0;0;0;1;0;1;0;0];
     [0;0;0;1;0;1;0;1;0;1;0;0;0];
     [0;0;0;1;1;0;1;0;1;1;0;0;0];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [1;1;1;0;1;1;0;1;1;0;1;1;1];
     [0;1;1;1;0;1;0;1;0;1;1;1;0];
     [0;0;1;1;1;0;1;0;1;1;1;0;0];
     [0;0;0;0;0;1;0;1;0;0;0;0;0];
     [0;0;1;1;1;0;1;0;1;1;1;0;0];
     [0;1;1;1;0;1;0;1;0;1;1;1;0];
     [1;1;1;0;1;1;0;1;1;0;1;1;1];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [0;0;0;1;1;0;1;0;1;1;0;0;0];
     [0;0;0;1;0;1;0;1;0;1;0;0;0];
     [0;0;1;0;1;0;0;0;1;0;1;0;0];
     [0;1;0;1;0;0;0;0;0;1;0;1;0]]

let braids_variant = 
    [[1;0;0;0;0;0;1;1;1;0;1;1;1];
     [0;0;0;1;0;0;0;1;1;1;1;1;0];
     [0;0;1;1;1;0;0;0;1;1;1;0;0];
     [0;1;1;1;0;0;0;1;1;1;0;0;0];
     [1;1;1;0;0;0;1;1;1;0;0;0;1];
     [1;1;0;0;0;1;1;1;0;0;0;1;1];
     [1;1;1;0;1;1;1;0;0;0;0;0;1];
     [0;1;1;1;1;1;0;0;0;1;0;0;0];
     [0;0;1;1;1;0;0;0;1;1;1;0;0];
     [0;0;0;1;1;1;0;0;0;1;1;1;0];
     [1;0;0;0;1;1;1;0;0;0;1;1;1];
     [1;1;0;0;0;1;1;1;0;0;0;1;1]]

let leaves = 
    [[1;0;1;0;1;0;0;0;1;0;1;0;1];
     [0;1;0;1;0;0;0;0;0;1;0;1;0];
     [1;0;1;0;0;0;1;0;0;0;1;0;1];
     [0;1;0;0;0;1;1;1;0;0;0;1;0];
     [1;0;0;0;1;0;1;0;1;0;0;0;1];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [0;0;1;0;1;1;1;1;1;0;1;0;0];
     [0;1;1;1;0;1;1;1;0;1;1;1;0];
     [1;0;1;1;1;0;1;0;1;1;1;0;1];
     [1;1;0;1;1;1;0;1;1;1;0;1;1];
     [1;1;1;0;1;1;1;1;1;0;1;1;1];
     [0;1;1;1;0;1;1;1;0;1;1;1;0];
     [1;0;1;1;1;0;1;0;1;1;1;0;1];
     [1;1;0;1;1;1;0;1;1;1;0;1;1];
     [1;1;1;0;1;1;1;1;1;0;1;1;1];
     [0;1;1;1;0;1;1;1;0;1;1;1;0];
     [0;0;1;1;1;0;1;0;1;1;1;0;0];
     [0;0;0;1;1;1;0;1;1;1;0;0;0];
     [1;0;0;0;1;1;1;1;1;0;0;0;1];
     [0;1;0;0;0;1;1;1;0;0;0;1;0];
     [1;0;1;0;0;0;1;0;0;0;1;0;1];
     [0;1;0;1;0;0;0;0;0;1;0;1;0]
    ]


// returns int list list containing the pattern * length * width
let preparePattern p =
    match p with 
    | DIY (w, l) -> 
        let len = 24
        let defaultW = 13
        match w with
        | 5 | 7 | 9 | 11 | 13 -> 
            if (l > 0) then ((diy w l), l, w) else ((diy w len), len, w)
        | _ -> 
            if (l > 0) then ((diy defaultW l), l, defaultW) else ((diy defaultW len ), len, defaultW)
    
    | Diamonds (w) -> 
        let len = 24
        let defaultW = 13
        match w with
        | 5 | 7 | 9 | 11 | 13 -> (patternCutter diamonds w, len, w)
        | _ -> (diamonds, len, defaultW)

    | Snowflakes (w) -> 
        let len = 20
        let defaultW = 13
        match w with
        | 5 | 7 | 9 | 11 | 13 -> (patternCutter snowflakes w, len, w)
        | _ -> (snowflakes, len, defaultW)

    | Braids (w: int) -> 
        let len = 8
        let defaultW = 11
        match w with
        | 5 | 7 | 9 | 11 | 13 -> (patternCutter braids w, len, w)
        | _ -> (patternCutter braids defaultW, len, defaultW)

    | DiamondsVariant (w) -> 
        let len = 24
        let defaultW = 13
        match w with
        | 5 | 7 | 9 | 11 | 13 -> (patternCutter diamonds_variant w, len, w)
        | _ -> (diamonds_variant, len, defaultW)

    | SnowflakesVariant (w) -> 
        let len = 18
        let defaultW = 13
        match w with
        | 5 | 7 | 9 | 11 | 13 -> (patternCutter snowflakes_variant w, len, w)
        | _ -> (snowflakes_variant, len, defaultW)

    | BraidsVariant (w: int) -> 
        let len = 12
        let defaultW = 13
        match w with
        | 5 | 7 | 9 | 11 | 13 -> (patternCutter braids_variant w, len, w)
        | _ -> (patternCutter braids_variant defaultW, len, defaultW)
