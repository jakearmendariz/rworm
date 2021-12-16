# Worm PL Interpretter
Jake Armendariz


## Wut
Worm is a small interpretted language with heavy static analysis. Its mocked after `C` and very similar in syntax, with small
differences such as a string type and a dynamic list instead of array.

I designed/built Worm this with the goal of building the ideal language to learn. Its `C` but easy and friendly!

For a great example please check out the `worm/arith.c` file :)!

## Download Requirments
- Rust 2018
- Cargo (rust package manager)

## How to Build
- `cargo build` or `make` (make puts interpretter binary in directory for easier use)

## How to Run
- ./worm-pl worm/<wormfile>
- ./worm-pl worm/tests.c (.c extension for syntax highlighting, this is Worm code, not C)
- ./worm-pl worm/arith.c (works similar to first assignment, user must add input)

## Stages
Interpretter currently works in 3 stages
- **parsing** turns raw text into an AST, catching parser errors
- **static analysis** type checking, checks all values called exist
- **execution** runs the abstract syntax tree, code will panic if this fails, as all errors should have been found in previous stages

## TODO
- Minimize language (remove list functionality), add a better standard library

## Complete
- Strongly typed with checks
- Effective error messages
- Structs
- Types: int, floats, bool, strings, arrays, maps are primary types, each has an array type
- Nested expressions in every case, function calls, indexing
- built in functions, print(), assert()

## Effective error messages
- Line and column numbers are provided.
- Uses static analysis, all type, missing type

```
Error: expected type 'string' recieved type 'char' on 14:16
Error: variable 'd' does not exist on 26:5
Error: function 'providearr' does not exist on 34:15
Error: expected type 'int' recieved type 'string' on 40:9
Error: function 'fuck' does not exist on 45:16
Aborting due to 5 error(s)
```

## Unique
**Pass by Value:** All parameters are pass by value, not pass by reference. 

**No Void Function** Because all functions are pass by value, every function must have a return value, otherwise they would have no purpose.

**Array Definition** I tried to create a unique way to declare arrays in worm. Every array can be defined either by a function return or by a `[value; size]` so if a programmer wants an array of all zeros they can do `[0; 20]`. For more dynamic changes, I allow a piping of values, so if `[|i| i; 20]` builds an array counting 0-19. Or even `[|index| index^2; 100]` builds an array of size 100

**map type** Integrated into the language is a custom hashmap type that can map from any variable type into any other. Right now there is no type checking involved in this stage, which is clearly a large problem. So to solve this problem, although the map type is from any type to any other, once a key is assigned to a value, the variable type initially assigned to the key is stuck for the lifetime of the hashmap.

Thus, if someone does `map["id"] = 0` then `map["id"] = "0"` there will be a type error, but `map["id"] = 2` causes no such violation

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

