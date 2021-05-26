fn main() -> int {
    map memory = {};
    memory["hello"] = 4;
    int x = memory["hello"];
    print(x);
    memory["sup"] = "world";
    print(memory["sup"]);
    return 0;
}