# rWorm PL Interpretter
Jake Armendariz


# Stages
Interpretter currently works in 3 stages
- **parsing** turns raw text into an AST, catching parser errors
- **static analysis** type checking, checks all values called exist
- **execution** runs the abstract syntax tree, code will panic if this fails, as all errors should have been found in previous stages

### Possible new stage: optimizer
Current abstract syntax trees

## TODO
- Better error handling
- Importing other filesn 
- weird stuff to make it unique and fun
- Maybe....
    - json
    - calling command line stuff

## Complete
- Strongly typed
- function oriented, must have return types to functions
- boolean expressions
- arrays (see below)
- int, floats, strings are primary types, each has an array type
- nested expressions in every case, function calls, indexing
- built in functions, print(), assert()

## Unique
#### Arrays
Arrays can be defined as a type `int[] arr = [|i| i; size]` where i is the index value ex: `int[] arr = [|i| (i+1)^2; 3]` => `[1, 4, 9]` 


When using an array as an AST::NODE, its type should be the type of value inside of the array
When defining a parameter or return value, it should be the VarType::Array(othertype)