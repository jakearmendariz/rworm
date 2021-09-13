import "worm/wormstd.c";

/* checks if a character is an operator */
fn is_operator(char c) -> int {
    if c == '+' | c == '-' | c == '*' | c == '/' {
        return 0;
    }
    return -1;
}

/* builds a node in the expression tree */
fn build_node(char data) -> map<string, int> {
    map<string, int> node = {string:int};
    node["data"] = data;
    node["is_op"] = is_operator(data);
    return node;
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
    map<char, int> precedence_order = get_precedence_order();
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
fn postfix_to_tree(string postfix) -> map<string, int> {
    map<string, int>[] stack = [{string:int}; 0];
    int i = 0;
    while i < len(postfix) {
        char curr = postfix[i];
        if curr == ' ' {
            skip;
        } else if is_operator(curr) != 0 | postfix[i+1] != ' ' {
            /* operand */
            map<string, int> node = {string:int};
            node["is_op"] = -1;
            string s = int_substring(postfix, i); /* retrieves the substring containing the integer */
            node["data"] = parse_int(s); /* parses the integer string, gets the interger underneath */
            i = i + len(s) - 1;
            stack = push_map(stack, node);
        } else {
            /* is an operator */
            map<string, int> node  = build_node(curr);
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
fn evaluate_tree(map<string, int> root) -> int {
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
    map<string, int> root = postfix_to_tree(postfix + ' ');
    return evaluate_tree(root);
}

fn main() -> int {
    string infix = user_input();
    return evaluate_expression(infix);
}