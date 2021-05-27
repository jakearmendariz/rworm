import "worm/wormstd.c";

fn is_operator(char c) -> int {
    if c == '+' | c == '-' | c == '*' | c == '/' {
        return 0;
    }
    return -1;
}

fn build_node(char data) -> map {
    map node = {};
    node["data"] = data;
    node["is_op"] = is_operator(data);
    return node;
}

fn get_precedence_order(int empty) -> map {
    map po = {};
    po['+'] = 0;
    po['-'] = 0;
    po['*'] = 1;
    po['/'] = 1;
    return po;
}

fn infix_to_postfix(string infix) -> string {
    string postfix = "";
    map precedence_order = get_precedence_order(0);
    char[] stack = [' '; 0];
    int index = 0;
    while index < len(infix) {
        char curr = infix[index];
        if is_operator(curr) == 0 {
            /* is operator */
            if len(stack) == 0 {
                stack = prepend_char(stack, curr);
                index = index + 1;
            } else {
                char top_element = stack[0];
                if precedence_order[curr] == precedence_order[top_element] {
                    postfix = postfix + stack[0];
                    stack = pop_left(stack);
                } else if precedence_order[curr] > precedence_order[top_element] {
                    stack = prepend_char(stack, curr);
                    index = index + 1;
                } else {
                    char popped = stack[0];
                    stack = pop_left(stack);
                    postfix = postfix + popped;
                }
            }
        }
        else {
            postfix = postfix + curr;
            index = index + 1;
        }
    }
    while len(stack) > 0 {
        char popped = stack[0];
        stack = pop_left(stack);
        postfix = postfix + popped;
    }
    return postfix;
}

/*
* Convert the postfix notation to a tree
*/
fn postfix_to_tree(string postfix) -> map {
    map[] stack = [{}; 0];
    int i = 0;
    while i < len(postfix) {
        char curr = postfix[i];
        if is_operator(curr) != 0 {
            map node = build_node(curr);
            stack = push_map(stack, node);
        } else {
            /* is an operator */
            map node = build_node(curr);
            node["right"] = stack[len(stack) -1];
            stack = pop_map(stack);
            node["left"] = stack[len(stack) -1];
            stack = pop_map(stack);
            stack = push_map(stack, node);
            print(curr);
            map right = node["right"];
            print(right["data"]);
            map left = node["left"];
            print(left["data"]);
        }
        i = i + 1;
    }
    return stack[len(stack)-1];
}


fn evaluate_tree(map root) -> int {
    if root["is_op"] == 0 {
        int left = evaluate_tree(root["left"]);
        int right = evaluate_tree(root["right"]);
        print(left);
        print(right);
        print(root["data"]);
        int res = 0;
        if root["data"] == '+' {
            res = left + right;
        } else if root["data"] == '-' {
            res = left - right;
        } else if root["data"] == '*' {
            res = left * right;
        } else if root["data"] == '/' {
            res = left / right;
        } else {
            print("unknown operation");
            return -1;
        }
        print(res);
        return res;
    } else {
        char c = root["data"];
        int data = c;
        return data - 48;
    }
    return 0;
}


fn main() -> int {
    string infix = "2+5*7*5";
    string postfix = infix_to_postfix(infix);
    print(postfix);
    map root = postfix_to_tree(postfix);
    print("EVALUATE");
    int result = evaluate_tree(root);
    print(result);
    return 0;
}