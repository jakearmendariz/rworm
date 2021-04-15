# Worm PL Interpreter
Jake Armendariz - jsarmend

## Project Goal
My goal is to build an interpreter with rust for a statically typed programming language which I named worm. In any given language, there are plenty of trade-offs[1] which make the language ideal for different situations and not for others. In this project I hope to create a language that contains as little (subjective) syntactic and semantic losses/tradeoffs as possible. As I build wormlang, I hope to learn more about why other language designers made similar or different decisions when creating their language strucutre. Overall my goal is to learn about language design, "if you have not developed language, you simply don't have access to most of human experience"(Noam Chomsky). So in this project I will attempt to build a language that I can enjoy using, so I am trying to take my favorite parts from each language and build something small and easy to use. 

I want to implement multiple features for this language[2] in order for it to be usable for myself and anyone else who wants to use it. Including but not limited to, a type system with integers, floats, strings, arrays, hash sets, and hash tables with multiple typed values. In addition, I hope to add custom structs, and a way to classify sets of functions to be used on a struct implicitly. I will also need to implement basic conditional logic and for loops. In order to build a language that I enjoy writing the syntax will be very similar to rust, with a couple of tweeks. Since this is going to be a small language I don't plan on dealing with the heap nor do I expect it to run very fast, instead I plan to learn a lot and build something I like to use.

## A Very Hopeful Development plan
- April 19: Development plan, tools, and basic practice
- April 26: Parsing types, conditionals, and converting to AST. Similar to while language
- May 3: Defining function definitions and calls
- May 10: Structure definition and use
- May 17: arrays, hash sets, and json
- May 24: Play catchup on missing and broken sections
- May 31: Working interpreter
- June 7: Final report is complete

## Challenges
I have been using rust and the pest parser for my most recent homework and have been struggling with parsing. Building a PEG parser grammar[3] is very difficult, especially in edge cases. I expected becoming familiar and competent with the parser and rust, in general, will take time to get used to.
<br><br>
I didn't include a type checker in the program, but I would love to learn more about this and hopefully implement one, however, I expect that I bit off more than I can chew with the other assignments in this class, but I am very excited to work on this language. The experience to spend my quarter writing an interpreter for my own language in rust is a wonderful experience and I expect to love this assignment.

## References
[1] K P, Naveen Reddy & .Y, Geyavalli & .D, Sujani & M, Rajesh. (2018). Comparison of Programming Languages: Review. 9. 113-122. 

[2] Benjamin C. Pierce. Types and Programming Languages. The MIT Press, Cambridge,
2002.

[3] Rossum, Guido van. “PEG Parsers.” Medium, Medium, 5 Sept. 2019, medium.com/@gvanrossum_83706/peg-parsers-7ed72462f97c. 
