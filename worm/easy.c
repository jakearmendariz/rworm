fn sum(int a, int b) -> int {
    int c = a + b;
    print(c);
    if c > 50 {
        int x = 7;
        return x;
    }
    return c;
}

fn main() -> int {
    int x = 10;
    int y = 20;
    return sum(sum(x, y), sum(x,y));
}