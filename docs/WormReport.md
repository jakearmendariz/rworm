# Building a Statically Typed Interpretted Language
Jake Armendariz

## Introduction
As the world of computer science expands the number and complexity of programming is increasing rapidly. With an increasing amount of languages there are an increasing amount of tradeoffs and design decisions to make in the process. When building larger languages (production level) they often grow in complexity and syntatic decisions are traded out for semantic needs. This quarter, my goal was to build a small language that is as simple, clean and with a structure that reduces bad programming practice.


## Design
In order to direct my design, I tried to build an ideal language for a beginner. Often, when learning to program, the first question a beginner asks is "Which language should I learn?". The most common languages recommended seem to be Python and Java, both of which fall short in a couple of categories. Python is missing types, lacks any enforced structure and because its strictly interprettedon the programmer. Java on the other hand, requires far too much overhead and can be intiminating to a new programmer as they have to see type way too many words before creating a simple hello world. 

My goal was to build a language that fit in the middle, simple like python, but less intimating then Java. I decided to call this language Worm, named for its small size, and my appreciation for python's simplicity.

I decided any beginner should learn about types, functions, arrays, hashmaps. I wanted to present these features in a very intential way. So similar to Java and C variables are expressed in the format `vartype varname` or `int x`. Functions must have explicit return types and parameters and arrays and maps can be built in types to reduce syntax complexity when learning new datastructures.

The "Hello World" of Worm is decidely simple. 

```
fn main() -> int {
	print("Hello, World");
	return 0;
}

```

Types

```
int, float, char, string, map,
int[], float[], char[], string[]
```

Along with some basic rules, I added some custom, unique semantic features to worm that I thought would help enforce good practice in a new programmer.

**Pass by Value:** All parameters are pass by value, not pass by reference. 

**No Void Function** Because all functions are pass by value, every function must have a return value, otherwise they would have no purpose.

**No Declaration, only Assignment** In order to reduce the amount of errors, every variable must be assigned a value when it is declared

**Array Definition** I tried to create a unique way to declare arrays in worm. Every array can be defined either by a function return or by a `[value; size]` so if a programmer wants an array of all zeros they can do `[0; 20]`. For more dynamic changes, I allow a piping of values, so if `[|i| i; 20]` builds an array counting 0-19. Or even `[|index| index^2; 100]` builds an array of size 100

**map type** Integrated into the language is a custom hashmap type that can map from any variable type into any other. Right now there is no type checking involved in this stage, which is clearly a large problem. So to solve this problem, although the map type is from any type to any other, once a key is assigned to a value, the variable type initially assigned to the key is stuck for the lifetime of the hashmap.

Thus, if someone does `map["id"] = 0` then `map["id"] = "0"` there will be a type error, but `map["id"] = 2` causes no such violation

## Static Analyzer and Error Checking
I have found good error messaging to be an essential part in programming in good language design. It's I acredit error messaging to be a large part in the shift from C to Rust in modern system programming.



## Sample Program

In order to demonstrate Worm's capabilities as a working language, I wrote some sample programs in it. One of which 


```
import "worm/wormstd.c";

/* checks if a character is an operator */
fn is_operator(char c) -> int {
    if c == '+' | c == '-' | c == '*' | c == '/' {
        return 0;
    }
    return -1;
}

/* builds a node in the expression tree */
fn build_node(char data) -> map {
    map node = {};
    node["data"] = data;
    node["is_op"] = is_operator(data);
    return node;
}

/* sets the order of precedence */
fn get_precedence_order() -> map {
    map po = {};
    po['+'] = 0;
    po['-'] = 0;
    po['*'] = 1;
    po['/'] = 1;
    return po;
}

/* extracts a substring that represents an integer */
fn int_substring(string s, int index) -> string {
    string result = "";
    int i = index;
    while index < len(s) & s[index] != ' ') {
        result = result + s[index];
        index = index + 1;
    }
    return result;
}


/* converts the infix expression to a postfix expression (easier to turn into an expression tree from there) */
fn infix_to_postfix(string infix) -> string {
    string postfix = "";
    map precedence_order = get_precedence_order();
    char[] stack = [' '; 0];
    int index = 0;
    while index < len(infix) {
        char curr = infix[index];
        if curr == ' ' {
            index = index + 1;
        }
        else {
            if (curr == '-' & infix[index+1] != ' ') | (is_operator(curr) != 0) {
                /* Operand */
                string num = int_substring(infix, index);
                postfix = postfix + ' ' + num;
                index = index + len(num);
            }
            /* is operator */
            else if len(stack) == 0 {
                stack = prepend_char(stack, curr);
                index = index + 1;
            } else {
                char top_element = stack[0];
                if precedence_order[curr] == precedence_order[top_element] {
                    postfix = postfix + ' ' + stack[0];
                    stack = pop_left(stack);
                } else if precedence_order[curr] > precedence_order[top_element] {
                    stack = prepend_char(stack, curr);
                    index = index + 1;
                } else {
                    char popped = stack[0];
                    stack = pop_left(stack);
                    postfix = postfix + ' ' + popped;
                }
            }
        }
    }
    /* add everything that is left */
    while len(stack) > 0 {
        char popped = stack[0];
        stack = pop_left(stack);
        postfix = postfix + ' ' + popped;
    }
    return postfix;
}

/* Convert the postfix notation to a tree */
fn postfix_to_tree(string postfix) -> map {
    map[] stack = [{}; 0];
    int i = 0;
    while i < len(postfix) {
        char curr = postfix[i];
        if curr == ' ' {
            skip;
        } else if is_operator(curr) != 0 | postfix[i+1] != ' ' {
            /* operand */
            map node = {};
            node["is_op"] = -1;
            string s = int_substring(postfix, i); /* retrieves the substring containing the integer */
            node["data"] = parse_int(s); /* parses the integer string, gets the interger underneath */
            i = i + len(s) - 1;
            stack = push_map(stack, node);
        } else {
            /* is an operator */
            map node = build_node(curr);
            node["right"] = stack[len(stack) -1];
            stack = pop_map(stack);
            node["left"] = stack[len(stack) -1];
            stack = pop_map(stack);
            stack = push_map(stack, node);
        }
        i = i + 1;
    }
    return stack[len(stack)-1];
}

/* executes the tree */
fn evaluate_tree(map root) -> int {
    if root["is_op"] == 0 {
        int left = evaluate_tree(root["left"]);
        int right = evaluate_tree(root["right"]);
        if root["data"] == '+' {
            return left + right;
        } else if root["data"] == '-' {
            return left - right;
        } else if root["data"] == '*' {
            return left * right;
        } else if root["data"] == '/' {
            return left / right;
        } else {
            print("unknown operation");
            return -1;
        }
    } else {
        return root["data"];
    }
    return 0;
}

/* turns string into postfix, then a tree, then it evaluates tree with a pres order traversal */
fn evaluate_expression(string infix) -> int {
    infix = pop_str(infix); /* remove the \n */
    string postfix = infix_to_postfix(infix);
    map root = postfix_to_tree(postfix + ' ');
    return evaluate_tree(root);
}

fn main() -> int {
    string infix = user_input();
    return evaluate_expression(infix);
}
```


## Conclusion
Building worm is not very special or relevant to modern research in programming langugaes. But I have found a lot of value in how I understand program semantics and design throught this process. I wrote this project in Rust, and I gained a much deeper sense of appreciation for the rust language design decisions as well as the complexities of the compiler as I learned more about it. This project isn't done either, I have a couple of goals that I would like to accomplish this summer, I plan on making my static analysis work as internal methods for state, so that I can edit the ast before running. This would help add some sugaring into the language that wouldn't be neeeded in the executable. I also would like to work more on th rust llvm bindings, its difficult to get llvm running on my machine and the projects on it are currently small, but I want to spend some time this summer 
