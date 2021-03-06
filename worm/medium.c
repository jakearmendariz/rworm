struct Tree {
    id: int,
    data: string,
}

fn hello(string s1, string s2) -> string {
    return s1 + s2;
}

fn playing_with_arrays(int[] b) -> int {
    string[] s = [""; 3];
    s[0] = "hello";
    s[1] = " ";
    s[2] = "world";
    assert(s[0] + s[1] + s[2] == "hello world");
    return 0;
}

fn test_chars(char a, char b) -> string {
    char c = 'c';
    return a + b + c;
}


fn test_mapping() -> int {
    map<char, int> dictionary = {char: int};
    int i = 0;
    string alpha = "abcdefghijklmnopqrstuvwxyz";
    print("Pre-loop");
    while i < len(alpha) {
        print("loop");
        dictionary[alpha[i]] = i;
        print("dictionary[alpha[i]]");
        i = i + 1;
    }
    assert(dictionary['a'] == 0);
    assert(dictionary['z'] == 25);
    return 0;
}

fn test_struct() -> int {
    struct<Tree> tree = Tree(1, "yolo");
    assert(tree.id == 1);
    assert(tree.data == "yolo");
    return 0;
}


struct Node {
    type: int,
    op: char,
    val: int,
    left: struct<Node>[],
    right: struct<Node>[],
}

fn build_value(int value) -> struct<Node> {
    struct<Node>[] left = [struct<Node>];
    struct<Node>[] right = [struct<Node>];
    return Node(1, 'a', value, left, right);
}

fn build_op(char value) -> struct<Node> {
    struct<Node>[] left = [struct<Node>];
    struct<Node>[] right = [struct<Node>];
    return Node(2, value, 0, left, right);
}
/*
fn test_append_structs() -> int {
    struct<Node> node = build_value(1);
    node.left = append(node.left, build_value(2));
    return 0;
}
    assert(test_append_structs() == 0);

*/

fn test_append() -> int {
    int[] arr = [0; 10];
    int[] arr1 = append(arr, 1);
    int[] arr2 = [1; 11];
    assert(arr1[10] == arr2[10]);
    assert(len(arr1) == 11);
    return 0;
}

fn test_arr_definition() -> int {
    int[] arr = [|i| i; 10];
    int[] arr2 = [i for i in 10];
    print(arr);
    print(arr2);
    assert(arr == arr2);
    return 0;
}

fn test_boolean_expressions() -> int {
    int x = 4;
    int result = ((x*x)+x);
    assert(x == 4 & x != 5);
    assert(result == 20);
    assert(x*x+x == 20 & 7 != 70);
    return 0;
}