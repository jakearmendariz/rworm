# Wormlang Interpreter
Jake Armendariz - jsarmend

## Project Goal
My goal is to build an interpreter with rust for a statically typed programming language which I named Worm. When designing the syntax and semantics I am trying to follow best practices [1], but primarily my hope is to build a language that fits my preferences. I expect the result to look syntactically similar to a Rust, C, Python mix. As I build Wormlang, I hope to learn more about why other language designers made similar or different decisions when creating their language structure.

I want to implement multiple features [2] with the goal of creating a useful scripting/prototyping tool for myself. Features including, a type system with (signed/unsigned) integers, floats, strings, arrays, hash sets, and hash tables with multiple typed values. Custom structs, conditional logic, and loops. I also plan to add filesystem support for reading, writing, and editing. Since this is going to be a small language I don't plan on dealing with the heap nor do I expect Wormlang to run very fast. However, if there is time I would like to learn more about interpreter/compiler optimizations and would be interested in implementing some along the way.

## A Very Hopeful Development plan
- April 19: Development plan, tools, and basic practice
- April 26: Parsing types, conditionals, and converting to AST. Similar to while language
- May 3: Defining function definitions and calls
- May 10: Structure definition and use
- May 17: arrays, hash sets, and json
- May 24: File system support
- May 31: Working interpreter, either add a small std library or implement optimizations
- June 7: Final report is complete

## Challenges
I have been using rust and the pest parser for my most recent homework and have been struggling with parsing. Building a PEG parser grammar[3] is can be fairly difficult, especially in edge cases. I expect working on the parser will be a large portion of the project.
<br><br>
I want to implement a lot of features, I am not sure how my development plan will pan out seeing as I set high expectations in the middle of the quarter. As I have already finished my hw2, I expect class assignments and my project goals will diverge after this week.


## References
[1] K P, Naveen Reddy & .Y, Geyavalli & .D, Sujani & M, Rajesh. (2018). Comparison of Programming Languages: Review. 9. 113-122. 

[2] Benjamin C. Pierce. Types and Programming Languages. The MIT Press, Cambridge,
2002.

[3] Rossum, Guido van. “PEG Parsers.” Medium, Medium, 5 Sept. 2019, medium.com/@gvanrossum_83706/peg-parsers-7ed72462f97c. 
