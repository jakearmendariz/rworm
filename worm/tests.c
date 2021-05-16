import "worm/easy.c";
import "worm/medium.c";

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

    assert(char_stuff('a', 'b') == "abc");

    /* success */
    return "SUCESS";
}