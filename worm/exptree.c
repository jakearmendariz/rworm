
fn is_operator(char c) -> int {
    if c == '+' | c == '-' | c == '*' | c == '/' {
        return 0;
    }
    return -1;
}

fn build_node(char data) {
    map node = {};
    node["data"] = data;
    node["left"] = 0;
    node["right"] = 0;
    node["is_op"] = is_operator(data);
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
    char[] stack = [' ', 0];
    int index = 0;
    while index < len(infix) {
        char curr = infix[index];
        if is_operator(stack[0]) == 0 {
            // is operator
            char top_element = stack[0];
            if precedence_order[curr] == precedence_order[top_element] {
                postfix = postfix + stack[0];
                stack = pop_left(stack);
            }
        } else if precedence_order[curr] > precedence_order[top_element] {
            stack = prepend_char(stack, curr);
            index = index + 1;
        }
    }
}


fn main() -> int {
    string input = "1+2-4*6";


}