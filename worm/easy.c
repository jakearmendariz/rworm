/* 
* Worm Programming language test suite
*/
 
import "worm/medium.c";

fn provide_arr(int limit) -> int[] {
    int[] nums = [|i| i; limit];
    return nums;
}

fn scope_test(int limit) -> int{
    int i = 0;
    int sum = 0;
    while i < sum {
        sum = sum(sum, i);
        i = i + 1;
    }
    return sum;
}


fn sum_up(int limit) -> int {
    int thesum = 0;
    int j = 0;
    int[] nums = provide_arr(limit);
    while j < limit {
        thesum = sum(nums[j], thesum); 
        j = j + 1;
    }
    return thesum;
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
    
    /* assert(6 == scope_test(4)); is broken*/
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
