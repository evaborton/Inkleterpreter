module Evaluator

open AST 
open Patterns

let evalColor(c: Color): string =
    match c with
    | Red -> "rgb(179, 0, 0)"
    | Orange -> "rgb(255, 178, 0)"
    | Yellow -> "rgb(255, 220, 25)"
    | Green -> "rgb(0, 102, 17)"
    | Blue -> "rgb(0, 0, 220)"
    | Purple -> "rgb(128, 8, 165)"
    | Pink -> "rgb(255, 160, 170)"
    | Brown -> "rgb(102, 68, 0)"
    | Black -> "rgb(0, 0, 0)"
    | Grey -> "rgb(150, 150, 150)"
    | White -> "rgb(255, 255, 220)" // use a cream color instead of true white for visibility
    | RGB(r, g, b) -> "rgb(" + string r + ", " + string g + ", " + string b + ")";


// let evalCoord(c: Coordinate): string =
//     let x,y = c
//     "x=\"" + (string x) + "\" " + "y=\"" + (string y) + "\""

let evalDot(c: Coordinate) (col: Color) = 
    let x, y = c
    "<circle cx= \"" + (string x) + "\" cy= \"" + (string y) + "\" r= \"1\" stroke = \"" + evalColor col + "\" stroke-width=\"1\" fill = \"" + evalColor col + "\"/> \n"

let evalLine(l: Line): string =
    let (x1,y1),(x2,y2),color, width = l
    "  <line x1=\"" + (x1 |> string) + "\"" +
    " y1=\"" + (y1 |> string) + "\"" +
    " x2=\"" + (x2 |> string) + "\"" +
    " y2=\"" + (y2 |> string) + "\"" +
    " style=\"stroke:" +
    (evalColor color) + ";stroke-width:" + string width + "\" />\n"

let evalLineChangeable(l: Line)(c: Color): string =
    let (x1,y1),(x2,y2),color, width = l
    "  <line x1=\"" + (x1 |> string) + "\"" +
    " y1=\"" + (y1 |> string) + "\"" +
    " x2=\"" + (x2 |> string) + "\"" +
    " y2=\"" + (y2 |> string) + "\"" +
    " style=\"stroke:" + (evalColor color) + 
    ";stroke-width:" + string width + "\" " +
    "onclick=\"switchColor(this)\" />\n"

// draws n rows from least to greatest y (starting at arg y), rowW space apart, each row from minX to maxX
let rec layRows n y rowW minX maxX col = 
    match n with 
    | 0 -> ""
    | _ -> evalLine(Line(Coordinate(minX, y), Coordinate(maxX, y), col, 2)) + (layRows (n - 1) (y + rowW) rowW minX maxX col)

let rec layColumns n x colW minY maxY col = 
    match n with 
    | 0 -> ""
    | _ -> evalLine(Line(Coordinate(x, minY), Coordinate(x, maxY), col, 2)) + (layColumns (n - 1) (x + colW) colW minY maxY col)


let layGrid (rows: int) (columns: int) (center: Coordinate) (rowW: int) (colW: int) (col: Color) = 
    let x, y = center
    let minX = x - (columns * colW / 2)
    let minY = y - (rows * rowW / 2)
    let maxX = x + (columns * colW / 2)
    let maxY = y + (rows * rowW / 2)
    
    layRows (rows + 1) minY rowW minX maxX col + layColumns (columns + 1) minX colW minY maxY col

// note that x is center of cell not edge
let rec fillRow (row: int list) (bgc: Color) (ptc: Color) (colW: int) (x: int) (minY: int) (maxY: int) =
    match row with 
    | [] -> ""
    | _ -> 
        let color = if row.[0] = 0 then bgc else if row.[0] = 1 then ptc else RGB(255, 255, 255)
        evalLine(Line(Coordinate(x, minY), Coordinate(x, maxY), color, colW)) + fillRow row.Tail bgc ptc colW (x + colW) minY maxY
        

let rec fillRows (p: int list list) (bgc: Color) (ptc: Color) (topLeft: Coordinate) (rowW: int) (colW: int) = 
    match p with 
    | [] -> ""
    | _ -> 
        let x,y = topLeft
        let newTopLeft = Coordinate(x, y + rowW)
        (fillRow p.Head bgc ptc colW (x + (colW / 2)) y (y + rowW)) + fillRows p.Tail bgc ptc newTopLeft rowW colW

let rec fillRowChangeable (row: int list) (bgc: Color) (ptc: Color) (colW: int) (x: int) (minY: int) (maxY: int) =
    match row with 
    | [] -> ""
    | _ -> 
        let color = if row.[0] = 0 then bgc else if row.[0] = 1 then ptc else RGB(255, 255, 255) 
        let otherColor = if row.[0] = 0 then ptc else if row.[0] = 1 then bgc else RGB(255, 255, 255) 
        evalLineChangeable (Line(Coordinate(x, minY), Coordinate(x, maxY), color, colW)) otherColor + fillRowChangeable row.Tail bgc ptc colW (x + colW) minY maxY
        

let rec fillRowsChangeable (p: int list list) (bgc: Color) (ptc: Color) (topLeft: Coordinate) (rowW: int) (colW: int) = 
    match p with 
    | [] -> ""
    | _ -> 
        let x,y = topLeft
        let newTopLeft = Coordinate(x, y + rowW)
        (fillRowChangeable p.Head bgc ptc colW (x + (colW / 2)) y (y + rowW)) + fillRowsChangeable p.Tail bgc ptc newTopLeft rowW colW

let rec warpHelper w =
    match w with 
    | 0 -> ([], [])
    | _ -> 
        let a, b = warpHelper (w - 1) 
        (List.append a [1;1;-1;0;-1;-1;0;-1], List.append b [-1;-1;0;-1;1;1;-1;0])

let warp (w: int) (bgc: Color) (ptc: Color) gridColor = 
    let cellWidth = 20
    let centerX = 700
    let centerY = 50
    let columns = 4 * w + 10


    let topLeft = Coordinate(centerX - ((columns / 2) * cellWidth), centerY - cellWidth)
    
    let a, b = warpHelper (w / 2)

    let hed = List.append (List.append [0;-1;0;-1;0;-1] a) [1;1;-1;0;-1;0;-1;0]
    let unhed = List.append (List.append [-1;0;-1;0;-1;0] b) [-1;-1;0;-1;0;-1;0;-1]


    fillRows [hed; unhed] bgc ptc topLeft cellWidth cellWidth + 
    layGrid 2 columns (Coordinate(centerX, centerY)) cellWidth cellWidth gridColor

// compares to a single row
let rec getDotBoolHelperHelper (lst: int list) index width dotBool = 
    if (index >= width) then 0
    else
        if ((lst.[index] = 1 && dotBool) || (lst.[index] = 0 && not dotBool)) then 1 + getDotBoolHelperHelper lst (index + 1) width (not dotBool)
        else getDotBoolHelperHelper lst (index + 1) width (not dotBool)

// compares to a list for either true or false
let rec getDotBoolHelper (lst: int list list) index len width dotBool = 
    if (index >= len) then 0
    else getDotBoolHelperHelper lst.[index] index width dotBool + getDotBoolHelper lst (index + 1) len width (not dotBool)


// whether top left thread is heddled or unheadled
let getDotBool p = 
    let lst, l, w = preparePattern p
    let trueScore = getDotBoolHelper lst 0 l w true
    let falseScore = getDotBoolHelper lst 0 l w false
    
    (trueScore > falseScore)

let rec dotsHelper start w colW dotBool dotColor =
    match w with 
    | 0 -> ""
    | _ -> 
        let x, y = start
        let thisDot = if dotBool then (evalDot start dotColor) else ""
        thisDot + (dotsHelper (Coordinate(x + colW, y)) (w - 1) colW (not dotBool) dotColor)

// marks heddled threads
let rec dots topLeft w l rowW colW dotBool dotColor =
    match l with 
    | 0 -> ""
    | _ -> 
        let x, y = topLeft
        (dotsHelper topLeft w colW dotBool dotColor) + (dots (Coordinate(x, y + rowW)) w (l - 1) rowW colW (not dotBool) dotColor)


let evalSwitchColors ptc bgc =
    "<script>\n" +
    "function switchColor(element) {\n" +
    "   if (element.style.stroke == '" + evalColor ptc + "') {\n" +
    "   	element.style.stroke='" + evalColor bgc + "';\n" +
    "   }\n" +
    "   else if (element.style.stroke == '" + evalColor bgc + "') {\n" +
    "   	element.style.stroke='" + evalColor ptc + "';\n" +
    "   }\n" +
    "}\n" +
    "</script>\n"

let evalPattern (p: Pattern) (bgc: Color) (ptc: Color) = 
    let lst, len, width = preparePattern p
    let centerX = 700
    let centerY = 400
    let rowW = 25 
    let colW = 25

    let topLeft = Coordinate(centerX - (colW * width / 2), centerY - (rowW * len / 2))
    let topLeftX, topLeftY = topLeft

    let dotAndGridColor = 
        match bgc with 
        | Black -> 
            match ptc with
            | Grey -> White 
            | _ -> Grey 
        | Grey ->
            match ptc with
            | Black -> White 
            | _ -> Black 
        | _ ->
            match ptc with 
            | Black -> Grey 
            | _ -> Black

    let fillRowsString = 
        match p with 
        | DIY(_, _) -> fillRowsChangeable lst bgc ptc topLeft rowW colW
        | _ -> fillRows lst bgc ptc topLeft rowW colW 

    fillRowsString +
    layGrid len width (Coordinate(centerX, centerY)) rowW colW dotAndGridColor + 
    warp width bgc ptc dotAndGridColor + 
    dots (Coordinate(topLeftX + (colW / 2), topLeftY + (rowW/2))) width len rowW colW (getDotBool p) dotAndGridColor
    
    


let eval(p: Pattern) (bgc: Color) (ptc: Color): string =
    "<!DOCTYPE html>\n" +
    "<html>\n" +
    "<body>\n" +
    evalSwitchColors ptc bgc +
    "<svg width=\"1400\" height=\"800\"" + 
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n
    <style>
        .header {
            font: bold italic 60px times
        }
        .subheader {
            font: bold 36px times
        }
        .body {
            font: 24px times
        }
        .italic {
            font: italic 24px times
        }
    </style>
    <text x=\"30\" y=\"160\" fill=\"rgb(50, 0, 120)\" class=\"header\">Inkleterpreter</text>
    <text x=\"30\" y=\"200\" fill=\"rgb(50, 0, 150)\" class=\"body\">
        <tspan x=\"30\" y=\"200\">Inkleterpreter is a language designed for</tspan> 
        <tspan x=\"30\" y=\"225\">the easy creation of inkle loom patterns.</tspan> 
        <tspan x=\"30\" y=\"250\">The top grid is the warp, and the center</tspan> 
        <tspan x=\"30\" y=\"275\">grid is the pattern. All patterns are</tspan> 
        <tspan x=\"30\" y=\"300\">taken or adapted/extended from</tspan> 
        <tspan x=\"30\" y=\"325\" class=\"italic\">The Weaver's Inkle Pattern Directory</tspan> 
        <tspan x=\"30\" y=\"350\">by Anne Dixon.</tspan> 
    </text>
    <text x=\"30\" y=\"410\" fill=\"rgb(50, 0, 120)\" class=\"subheader\">Pattern Options:</text>
    <text x=\"30\" y=\"450\" fill=\"rgb(50, 0, 150)\" class=\"body\">
        <tspan x=\"30\" y=\"450\">diamonds</tspan> 
        <tspan x=\"30\" y=\"475\">diamonds variant</tspan> 
        <tspan x=\"30\" y=\"500\">snowflakes</tspan> 
        <tspan x=\"30\" y=\"525\">snowflakes variant</tspan> 
        <tspan x=\"30\" y=\"550\">braids</tspan> 
        <tspan x=\"30\" y=\"575\">braids variant</tspan>
        <tspan x=\"30\" y=\"600\">diy (a blank slate that you can edit)</tspan> 
    </text>\n" +
    (evalPattern p bgc ptc) +
    "</svg>\n</body>\n</html>"