# rWorm PL Interpretter
Jake Armendariz

### Current Feautes
- arithmetic expressions
- variable assignments
- boolean expressions
- for loops, while loops, functions


## TODO
- String concatination and indexing
- structs
- Better error handling
- weird stuff to make it unique and fun
- Maybe....
    - forloops
    - json
    - calling command line stuff

## Complete
- Strongly typed
- function oriented, must have return types to functions
- arrays (see below)
- int, floats, strings are primary types, each has an array type
- nested expressions in every case, function calls, indexing
- built in functions, print(), assert()

## Unique
#### Arrays
Arrays can be defined as a type `int[] arr = [|i| i; size]` where i is the index value ex: `int[] arr = [|i| (i+1)^2; 3]` => `[1, 4, 9]` 




### Possible additonal features
llvmenv = "0.3.1"
llvm-sys = "110.0.1"