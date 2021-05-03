fn sum(int a, int b) -> int {
    int c = a + b;
    int d = c;
    return c ;
}

fn main() -> int {
    int x = 10;
    int y = 20;
    int d = sum(sum(x, y), sum(x,y));
    return d ;
}