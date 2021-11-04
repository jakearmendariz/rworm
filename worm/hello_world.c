fn playing_with_arrays(int[] b) -> int {
    string[] s = [""; 3];
    s[0] = "hello";
    s[1] = " ";
    s[2] = "world";
    string a = s[0] + s[1];
    print(a);
    assert(s[0] + s[1] + s[2] == "hello world");
    return 0;
}

fn main() -> string {
    int x = 10;
    int[] arr = [|i| i; 5];
    assert(playing_with_arrays(arr) == 0);
    return "Hello World";
}

