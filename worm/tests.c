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

    /* array creation tests */
    int []arr = [|i| i; 5];
    assert(playing_with_arrays(arr) == 0);

    /* assert(test_chars('a', 'b') == "abc"); */

    assert(test_mapping() == 0);

    assert(test_struct() == 0);

    assert(test_append() == 0);

    /* success */
    return "SUCESS";
}