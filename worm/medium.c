struct Tree {
    id: int,
    data: string,
}

fn floating_lcm(float a, float b) -> float {
    float ans = (a*b)/floating_gcd(a, b);
    return ans;
}

fn floating_gcd(float x, float y) -> float {
    while(true) {
        if x > y {
            x = x - y;
        } else if x < y {
            y = y - x;
        } else if(y == x) {
            return x;
        }
    }
    return x;
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
    while i < len(alpha) {
        print(alpha[i]);
        dictionary[alpha[i]] = i;
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
