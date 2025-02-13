module AST

type Coordinate = int * int
type Color =
| Red
| Orange 
| Yellow
| Green
| Blue
| Purple
| Pink
| Brown
| Black
| Grey
| White
| RGB of int * int * int
type Line = Coordinate * Coordinate * Color * int // float is line width
type Pattern = 
// for pre-coded patterns, int can be 5, 7, 9, 11, 13. passing int 0 uses default width
| Diamonds of int 
| Snowflakes of int
| Braids of int
| DiamondsVariant of int 
| SnowflakesVariant of int
| BraidsVariant of int
| DIY of int * int // width * length
type Weaving = Pattern * Color * Color // 1st color is background, 2nd is pattern