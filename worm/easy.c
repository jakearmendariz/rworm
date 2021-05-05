/* 
* Worm Programming language test suite
*/
 
fn sum(int a, int b) -> int {
    int c = a + b;
    return c;
}

fn provide_arr(int limit) -> int[] {
    int[] nums = [|i| i; limit];
    return nums;
}

fn sum_up(int limit) -> int {
    int sum = 0;
    int j = 0;
    int[] nums = provide_arr(limit);
    while j < limit {
        sum = sum(sum, nums[j]); 
        j = j + 1;
    }
    return sum;
}

fn floating_lcm(float a, float b) -> float {
    float ans = (a*b)/floating_gcd(a, b);
    print(ans);
    return ans;
}

fn floating_gcd(float x, float y) -> float {
    while(x != y) {
        if x > y {
            x = x - y;
        } if x < y {
            y = y - x;
        }
    }
    return x;
}

fn hello(string s1, string s2) -> string {
    string s = s1 + s2;
    return s;
}

fn fucking_with_arrays(int[] b) -> int {
    string[] s = [""; 3];
    s[0] = "hello";
    s[1] = " ";
    s[2] = "stupid";
    assert(s[0] + s[1] + s[2] == "hello stupid");
    return 0;
}

fn main() -> string {
    int x = 10;
    int y = 20;
    /* nested function calls test */
    assert(sum(sum(x, y), sum(x,y)) == 60);

    /* array creation, passing though function */
    assert(sum_up(10) == 45);

    /* checking string stuff */
    assert(hello("hello", ", world") == "hello, world");

    /* floating point test */
    assert(floating_lcm(10.0, 12.0) == 60.0);

    /* array creation tests */
    int []arr = [|i| i; 5];
    assert(fucking_with_arrays(arr) == 0);

    /* success */
    return "SUCESS";
}

fn test(int a) -> int {    
    
    return 0;
}
