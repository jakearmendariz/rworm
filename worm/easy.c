fn sum(int a, int b) -> int {
    int c = a + b;
    return c;
}

fn sum_up(int limit) -> int {
    int sum = 0;
    int j = 0;
    int[] nums = [(i+1)*6; limit]; /* [1, 2, 3, 4] */
    while j < limit {
        sum = sum(sum, nums[j]); 
        print(nums[j]);
        j = j + 1;
    }
    return sum;
}

fn main() -> int {
    int x = 10;
    int y = 20;
    /* return sum(sum(x, y), sum(x,y)); */
    return sum_up(10);
}