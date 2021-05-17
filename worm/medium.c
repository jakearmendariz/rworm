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

fn fucking_with_arrays(int[] b) -> int {
    string[] s = [""; 3];
    s[0] = "hello";
    s[1] = " ";
    s[2] = "stupid";
    assert(s[0] + s[1] + s[2] == "hello stupid");
    return 0;
}

fn char_stuff(char a, char b) -> string {
    char c = 'c';
    return a + b + c;
}