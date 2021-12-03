# Worm PL Interpretter
Jake Armendariz

## Download Requirments
- Rust 2018
- Cargo (rust package manager)

## How to Compile
- make clean
- make

## How to Run (Submitting binary for assignment to make it easier to run if you don't want to download rust)
- ./worm-pl worm/<wormfile>
- ./worm-pl worm/tests.c (.c extension for syntax highlighting, this is Worm code, not C)
- ./worm-pl worm/arith.c (works similar to first assignment, user must add input)

## Stages
Interpretter currently works in 3 stages
- **parsing** turns raw text into an AST, catching parser errors
- **static analysis** type checking, checks all values called exist
- **execution** runs the abstract syntax tree, code will panic if this fails, as all errors should have been found in previous stages

### Possible new stage: optimizer
Current abstract syntax trees

## TODO
- Better error handling
- convert to from functions -> methods
- compile to llvm? (hopefully)

## Complete
- Strongly typed
- function oriented, must have return types to functions
- boolean expressions
- arrays (see below)
- int, floats, strings are primary types, each has an array type
- nested expressions in every case, function calls, indexing
- built in functions, print(), assert()

## Unique
**Pass by Value:** All parameters are pass by value, not pass by reference. 

**No Void Function** Because all functions are pass by value, every function must have a return value, otherwise they would have no purpose.

**No Declaration, only Assignment** In order to reduce the amount of errors, every variable must be assigned a value when it is declared

**Array Definition** I tried to create a unique way to declare arrays in worm. Every array can be defined either by a function return or by a `[value; size]` so if a programmer wants an array of all zeros they can do `[0; 20]`. For more dynamic changes, I allow a piping of values, so if `[|i| i; 20]` builds an array counting 0-19. Or even `[|index| index^2; 100]` builds an array of size 100

**map type** Integrated into the language is a custom hashmap type that can map from any variable type into any other. Right now there is no type checking involved in this stage, which is clearly a large problem. So to solve this problem, although the map type is from any type to any other, once a key is assigned to a value, the variable type initially assigned to the key is stuck for the lifetime of the hashmap.

Thus, if someone does `map["id"] = 0` then `map["id"] = "0"` there will be a type error, but `map["id"] = 2` causes no such violation


# TODO
In the static analysis I shouldn't be using execution state and fake constant values. Switch that out for a name:VaryType map.

Implement traits (generics) for constant and var type so I can treat them one way in static analysis and another in execution.

The Pair object contains a char number, this could be used in error cases to find the actual line number associatged.
If we have the pair, then we can rescan during error conditions to find the actual line number of the problem.



Instead of assignment being
string = expr

it should be

value_obj
    var_name ~ ('.' ~ var_name | '[' ~ expr ~ ']')*

## Performance
June 3: Before seperation of execution and function state
```
35 tests, 0 failures

sudo bash test.sh  3.49s user 0.71s system 89% cpu 4.685 total
```

```
35 tests, 0 failures

sudo bash test.sh  1.55s user 0.69s system 86% cpu 2.585 total
```


# Byte Code

### Assignment
- Var = OP Var Var
- Var = OP Var Const
- Var = OP Const Const

### Return
- return Const
- return Var


