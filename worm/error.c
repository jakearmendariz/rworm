
fn sum(int a, int b) -> int {
    int c = a + b;
    return c;
}

fn provide_arr(int limit) -> int[] {
    int[] nums = [|i| i; limit];
    string c = 'a';
    return nums;
}

fn scope_test(int limit) -> int{
    int i = 0;
    int sum = 0;
    while i < sum {
        sum = sum(sum, i);
        i = i + 1;
        int d = i;
    }
    d = 1;
    return sum;
}


fn sum_up(int limit) -> int {
    int thesum = 0;
    int j = 0;
    int[] a = providearr(limit);
    int[] nums = provide_arr(limit);
    while j < limit {
        thesum = sum(nums[j], thesum); 
        j = j + 1;
    }
    j = "types are off"; /*0;*/
    return thesum;
}

fn main() -> string {
    string a = fuck();
    /* assert(6 == scope_test(4)); is broken*/
    int x = 10;
    int y = 20;
    /* nested function calls test */
    assert(sum(sum(x, y), sum(x,y)) == 60);  

    /* array creation, passing though function */
    assert(sum_up(10) == 45);

    
    /* success */
    return "SUCESS";
}