# rWorm PL Interpretter
Jake Armendariz

### Current Feautes
- arithmetic expressions
- variable assignments
- boolean expressions
- for loops, while loops, functions


## TODO
arrays
more complicatd sample program to test
print statement that is more dynamic
research llvm and either move in one two directions
1) convert the current version of code to a compiled language
2) Add dictionaries, then start messing around with weird language features and syntaxes to make this actually useful

### Arrays
Arrays can be defined as a type `int[] arr = [expr(index as i); size]` ex: `int[] arr = [i; 3]` => `[0, 1, 2]` 
- TODO
    - update values of array
    - arr[i] = 6
- Ideas
    - Add lazy evaluation
    - int[] arr = [expression; ..]

### Possible additonal features
llvmenv = "0.3.1"
llvm-sys = "110.0.1"