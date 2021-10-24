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

fn char_stuff(char a, char b) -> string {
    char c = 'c';
    return a + b + c;
}


fn test_mapping() -> int {
    map<char, int> dictionary = {char: int};
    int i = 0;
    string alpha = "abcdefghijklmnopqrstuvwxyz";
    while i < len(alpha) {
        dictionary[alpha[i]] = i;
        i = i + 1;
    }
    assert(dictionary['a'] == 0);
    assert(dictionary['z'] == 25);
    return 0;
}
