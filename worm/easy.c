/* 
* Worm Programming language test suite
*/
 
import "worm/medium.c";

fn sum(int a, int b) -> int {
    int c = a + b;
    return c;
}

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
