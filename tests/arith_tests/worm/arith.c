/**
 * Creates and parses a expression tree.
 * Best demonstration of worm
 */

import "/Users/jakearmendariz/Desktop/rworm/worm/wormstd.c";

struct Node {
    type: int,
    op: char,
    val: int,
    left: struct<Node>[],
    right: struct<Node>[],
}

fn build_value(int value) -> struct<Node> {
    return Node(1, 'a', value, [struct<Node>], [struct<Node>]);
}

fn build_op(char value) -> struct<Node> {
    return Node(2, value, 0, [struct<Node>], [struct<Node>]);
}

/* checks if a character is an operator */
fn is_operator(char c) -> int {
    if c == '+' | c == '-' | c == '*' | c == '/' {
        /* true */
        return 1;
    }
    return 0;
}

/* sets the order of precedence */
fn get_precedence_order() -> map<char, int> {
    map<char, int> po = {char: int};
    po['+'] = 0;
    po['-'] = 0;
    po['*'] = 1;
    po['/'] = 1;
    return po;
}

/* extracts a substring that represents an integer */
fn int_substring(string s, int index) -> string {
    string result = "";
    if s[index] == '-' {
        /* eat the negative */
        result = result + s[index];
        index = index + 1;
    }
    /* Error is here because - isn't beiong increased */
    while index < len(s) & s[index] != ' ' & s[index] != ')' & is_operator(s[index]) == 0 {
        result = result + s[index];
        index = index + 1;
    }
    return result;
}


/* converts the infix expression to a postfix expression (easier to turn into an expression tree from there) */
fn infix_to_postfix(string infix) -> string {
    string postfix = "";
    infix = infix + " ";
    map<char, int> precedence_order = get_precedence_order();
    char[] stack = [' '; 0];
    int index = 0;
    while index < len(infix) {
        char curr = infix[index];
        if curr == ' ' {
            index = index + 1;
        }
        else {
            if (curr == '(') {
                stack = prepend(stack, curr);
                index = index + 1;
            } else if (curr == ')') {
                char popped = stack[0];
                stack = remove(stack, 0);
                index = index + 1;
                while (popped != '(') {
                    postfix = postfix + ' ' + popped;
                    popped = stack[0];
                    stack = remove(stack, 0);
                }
            }
            else if (curr == '-' & infix[index+1] != ' ') | (is_operator(curr) != 1) {
                /* Operand */
                string num = int_substring(infix, index);
                postfix = postfix + ' ' + num;
                index = index + len(num);
            }
            /* is operator */
            else if len(stack) == 0{
                stack = prepend(stack, curr);
                index = index + 1;
            } else {
                char top_element = stack[0];
                if top_element == '(' {
                    stack = prepend(stack, curr);
                    index = index + 1;
                } else if precedence_order[curr] == precedence_order[top_element] {
                    postfix = postfix + ' ' + stack[0];
                    stack = remove(stack, 0);
                } else if precedence_order[curr] > precedence_order[top_element] {
                    stack = prepend(stack, curr);
                    index = index + 1;
                } else {
                    char popped = stack[0];
                    stack = remove(stack, 0);
                    postfix = postfix + ' ' + popped;
                }
            }
        }
    }
    /* add everything that is left */
    while len(stack) > 0 {
        char popped = stack[0];
        stack = remove(stack, 0);
        postfix = postfix + ' ' + popped;
    }
    return postfix;
}

/* Convert the postfix notation to a tree */
fn postfix_to_tree(string postfix) -> struct<Node> {
    /* print(postfix); */
    struct<Node>[] stack = [struct<Node>];
    int i = 0;
    while i < len(postfix) {
        char curr = postfix[i];
        if curr == ' ' {
            skip;
        } else if is_operator(curr) != 1 | postfix[i+1] != ' ' {
            string s = int_substring(postfix, i); /* retrieves the substring containing the integer */
            /* operand */
            struct<Node> node = build_value(parse_int(s));/* parses the integer string, gets the interger underneath */
            i = i + len(s) - 1;
            stack = append(stack, node);
        } else {
            /* is an operator */
            struct<Node> node = build_op(curr);
            node.right = append(node.right, stack[len(stack) -1]);
            stack = remove(stack, len(stack)-1);
            node.left = append(node.left, stack[len(stack) -1]);
            stack = remove(stack, len(stack)-1);
            stack = append(stack, node);
        }
        i = i + 1;
    }
    return stack[len(stack)-1];
}

/* executes the tree */
fn evaluate_tree(struct<Node> root) -> int {
    if root.type == 2 { /* is operation */
        int left = evaluate_tree(root.left[0]);
        int right = evaluate_tree(root.right[0]);
        if root.op == '+' {
            return left + right;
        } else if root.op == '-' {
            return left - right;
        } else if root.op == '*' {
            return left * right;
        } else if root.op == '/' {
            return left / right;
        } else {
            print("unknown operation");
            return -1000;
        }
    } else {
        return root.val;
    }
    return 0;
}

/* turns string into postfix, then a tree, then it evaluates tree with a pres order traversal */
fn evaluate_expression(string infix) -> int {
    infix = remove(infix, len(infix) -1); /* remove the \n */
    string postfix = infix_to_postfix(infix);
    struct<Node> root = postfix_to_tree(postfix + ' ');
    return evaluate_tree(root);
}

fn main() -> int {
    string infix = user_input();
    return evaluate_expression(infix);
}
