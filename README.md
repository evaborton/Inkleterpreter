# Inkleterpreter

Inkleterpreter is a programming language for quickly and easily producing patterns (warps included) for the creation of Baltic-style weaves on an inkle loom. Inkle looms are a type of small handheld loom used to create narrow bands that were traditionally used as belts, straps, and reins, among other uses. Baltic style refers to a specific type of inkle pattern where double-thick threads are used for the pattern threads and pattern threads are placed on the warp in a checkerboard pattern. An inkle pattern consists of both the warp, which are the vertical threads that are strung onto the loom before weaving, and the pattern, which shows how to manipulate the warp threads to produce a particular design.

Inkle patterns can be difficult to produce by hand, because not only do the patterns need to form the desired shape, they also need to follow certain rules so that the weaving remains structurally sound. Inkleterpreter makes production of inkle patterns fast and easy, whether the user wants to choose from a set of pre-coded patterns or create their own patterns with an interactive pattern grid.

## Dependencies

* .NET: https://dotnet.microsoft.com/en-us/

## Language Syntax

The first line of an Inkleterpreter program should be of the following form (replace words in brackets with desired option; do not include brackets in the final program):
pattern: 

```
[diy OR diamonds OR snowflakes OR braids OR diamonds variant
OR snowflakes variant OR braids variant], width: [number of pattern threads]
```

The DIY also has an option to specify pattern length. If not specified, it will revert to a default value of 24.

```
pattern: diy, width: [number of pattern threads], length: [number of rows]
```

The next two lines for either of the above options should be of the following form:

```
background color: [color]
pattern color: [color]
```

They could also be in the opposite order; it does not matter:

```
pattern color: [color]
background color: [color]
```

The color can be red, orange, yellow, green, blue, purple, pink, brown, white, black, or grey (spelled either ”gray” or ”grey”). It can also be of the form ”rgb [0-255] [0-255] [0-255]” to specify a color using RGB values.

The program should be saved in a .inkl file.

## Running the program

Run a .inkl program from the folder containing Program.fs with:

```
dotnet run filepath/program.inkl
```

This produces a weaving.html file that graphically displays the pattern (the lower grid), which tells the weaver how to manipulate the threads to get the desired pattern, and the warp (the long upper grid), which tells the weaver how to set up the warp, or lengthwise, threads on their loom before they begin weaving. The DIY case produces a warp of the requested size with an empty, interactive pattern grid so that the user can customize their pattern. The pre-existing patterns produce the complete pattern.

## Acknowledgements

Preprogrammed pattern options are adapted/extended from *The Weaver's Inkle Pattern Directory* by Anne Dixon.

